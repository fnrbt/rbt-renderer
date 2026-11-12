namespace Rbt.Renderer

open System.Collections.Generic

module Raster =

    // ── Temp framebuffer pool ──
    // Avoids allocating ~3MB per Group/Clip/Mask/etc. per frame.
    // THREAD-LOCAL: rendering uses this pool as per-frame scratch, so giving each thread its own
    // pool lets frames render concurrently (parallel encode) without racing on push/pop.
    let private fbPool = new System.Threading.ThreadLocal<Stack<uint32[]>>(fun () -> Stack<uint32[]>())

    let private getTempFb (w: int) (h: int) : Framebuffer =
        let pool = fbPool.Value
        let size = w * h
        let px =
            if pool.Count > 0 then
                let arr = pool.Pop()
                if arr.Length >= size then
                    System.Array.Clear(arr, 0, size)
                    arr
                else
                    Array.zeroCreate size
            else
                Array.zeroCreate size
        { W = w; H = h; Px = System.Memory(px, 0, size) }

    let private releaseTempFb (fb: Framebuffer) =
        match Framebuffer.tryGetArray fb with
        | Some arr -> fbPool.Value.Push(arr)
        | None -> ()

    /// Blend two packed BGRA premultiplied pixels (delegates to Pixel.blend)
    let inline blendPx (src: uint32) (dst: uint32) : uint32 = Pixel.blend src dst

    /// Legacy Color blend for blendWithMode (rare blend modes that need channel extraction)
    let blend (src: Color) (dst: Color) =
        if src.A = 255uy then src
        elif src.A = 0uy then dst
        else
            let inv = 255 - int src.A
            let r = int src.R + (int dst.R * inv + 128) / 255
            let g = int src.G + (int dst.G * inv + 128) / 255
            let b = int src.B + (int dst.B * inv + 128) / 255
            let a = int src.A + (int dst.A * inv + 128) / 255
            { R = byte r; G = byte g; B = byte b; A = byte a }

    let rgbToHsl (r: float) (g: float) (b: float) =
        let cmax = max r (max g b)
        let cmin = min r (min g b)
        let d = cmax - cmin
        let l = (cmax + cmin) / 2.0
        if d < 1e-10 then (0.0, 0.0, l)
        else
            let s = if l <= 0.5 then d / (cmax + cmin) else d / (2.0 - cmax - cmin)
            let h =
                if cmax = r then (g - b) / d + (if g < b then 6.0 else 0.0)
                elif cmax = g then (b - r) / d + 2.0
                else (r - g) / d + 4.0
            (h / 6.0, s, l)

    let hslToRgb (h: float) (s: float) (l: float) =
        if s < 1e-10 then (l, l, l)
        else
            let q = if l < 0.5 then l * (1.0 + s) else l + s - l * s
            let p = 2.0 * l - q
            let hue2rgb p q (t: float) =
                let t = if t < 0.0 then t + 1.0 elif t > 1.0 then t - 1.0 else t
                if t < 1.0/6.0 then p + (q - p) * 6.0 * t
                elif t < 1.0/2.0 then q
                elif t < 2.0/3.0 then p + (q - p) * (2.0/3.0 - t) * 6.0
                else p
            (hue2rgb p q (h + 1.0/3.0), hue2rgb p q h, hue2rgb p q (h - 1.0/3.0))

    let hslLum (r: float) (g: float) (b: float) = 0.299 * r + 0.587 * g + 0.114 * b

    let hslSat (r: float) (g: float) (b: float) = max r (max g b) - min r (min g b)

    let clipColor (r: float) (g: float) (b: float) =
        let l = hslLum r g b
        let n = min r (min g b)
        let x = max r (max g b)
        let mutable r, g, b = r, g, b
        if n < 0.0 && l <> n then
            let f = l / (l - n)
            r <- l + (r - l) * f
            g <- l + (g - l) * f
            b <- l + (b - l) * f
        if x > 1.0 && x <> l then
            let f = (1.0 - l) / (x - l)
            r <- l + (r - l) * f
            g <- l + (g - l) * f
            b <- l + (b - l) * f
        (r, g, b)

    let setLum (r: float) (g: float) (b: float) (l: float) =
        let d = l - hslLum r g b
        clipColor (r + d) (g + d) (b + d)
    /// PDF nonseparable blending uses SetSat on RGB components, not an HSL
    /// hue/saturation conversion (which shifts the intermediate luminance).
    let setSat (r: float) (g: float) (b: float) (sat: float) =
        let lo = min r (min g b)
        let hi = max r (max g b)
        if hi <= lo then (0.0, 0.0, 0.0)
        else
            let mid = r + g + b - lo - hi
            let middle = (mid - lo) * sat / (hi - lo)
            let channel c = if c >= hi then sat elif c <= lo then 0.0 else middle
            (channel r, channel g, channel b)


    let blendWithMode (mode: BlendMode) (srcPx: uint32) (dstPx: uint32) : uint32 =
        // Both src and dst are in premultiplied alpha space
        match mode with
        | Normal -> blendPx srcPx dstPx
        | _ ->
            let src = Pixel.toColor srcPx
            let dst = Pixel.toColor dstPx
            let result =
                match mode with
                // Porter-Duff modes operate directly in premultiplied space
                | SrcIn ->
                    let da = float dst.A / 255.0
                    let f (v: byte) = byte (min 255.0 (float v * da))
                    { R = f src.R; G = f src.G; B = f src.B; A = f src.A }
                | SrcOut ->
                    let inv = 1.0 - float dst.A / 255.0
                    let f (v: byte) = byte (min 255.0 (float v * inv))
                    { R = f src.R; G = f src.G; B = f src.B; A = f src.A }
                | DstOver ->
                    let inv = 1.0 - float dst.A / 255.0
                    let ch (s: byte) (d: byte) = byte (min 255.0 (float d + float s * inv))
                    let oa = float dst.A + float src.A * inv
                    { R = ch src.R dst.R; G = ch src.G dst.G; B = ch src.B dst.B; A = byte (min 255.0 oa) }
                | DstAtop ->
                    let sa = float src.A / 255.0
                    let inv = 1.0 - float dst.A / 255.0
                    let ch (s: byte) (d: byte) = byte (min 255.0 (float d * sa + float s * inv))
                    { R = ch src.R dst.R; G = ch src.G dst.G; B = ch src.B dst.B; A = src.A }
                | Xor ->
                    let invS = 1.0 - float src.A / 255.0
                    let invD = 1.0 - float dst.A / 255.0
                    let ch (s: byte) (d: byte) = byte (min 255.0 (float s * invD + float d * invS))
                    let oa = float src.A * invD + float dst.A * invS
                    { R = ch src.R dst.R; G = ch src.G dst.G; B = ch src.B dst.B; A = byte (min 255.0 oa) }
                | _ ->
                    // Separable and HSL blend modes: unpremultiply, compute blend, premul composite
                    let sa = float src.A / 255.0
                    let da = float dst.A / 255.0
                    let oa = sa + da * (1.0 - sa)
                    if oa < 1e-10 then { R = 0uy; G = 0uy; B = 0uy; A = 0uy }
                    else
                        // Unpremultiply to get straight-alpha color values
                        let cs_r = if sa > 1e-10 then float src.R / (sa * 255.0) else 0.0
                        let cs_g = if sa > 1e-10 then float src.G / (sa * 255.0) else 0.0
                        let cs_b = if sa > 1e-10 then float src.B / (sa * 255.0) else 0.0
                        let cd_r = if da > 1e-10 then float dst.R / (da * 255.0) else 0.0
                        let cd_g = if da > 1e-10 then float dst.G / (da * 255.0) else 0.0
                        let cd_b = if da > 1e-10 then float dst.B / (da * 255.0) else 0.0
                        let (br, bg, bb) =
                            match mode with
                            | Hue ->
                                let (r, g, b) = setSat cs_r cs_g cs_b (hslSat cd_r cd_g cd_b)
                                setLum r g b (hslLum cd_r cd_g cd_b)
                            | Saturation ->
                                let (r, g, b) = setSat cd_r cd_g cd_b (hslSat cs_r cs_g cs_b)
                                setLum r g b (hslLum cd_r cd_g cd_b)
                            | ColorMode ->
                                setLum cs_r cs_g cs_b (hslLum cd_r cd_g cd_b)
                            | Luminosity ->
                                setLum cd_r cd_g cd_b (hslLum cs_r cs_g cs_b)
                            | _ ->
                                // Separable blend function B(Cs, Cd) per channel
                                let bfn sv dv =
                                    match mode with
                                    | Multiply -> sv * dv
                                    | Screen -> sv + dv - sv * dv
                                    | Overlay ->
                                        if dv < 0.5 then 2.0 * sv * dv
                                        else 1.0 - 2.0 * (1.0 - sv) * (1.0 - dv)
                                    | HardLight ->
                                        if sv <= 0.5 then 2.0 * sv * dv
                                        else 1.0 - 2.0 * (1.0 - sv) * (1.0 - dv)
                                    | Darken -> min sv dv
                                    | Lighten -> max sv dv
                                    | ColorDodge ->
                                        if dv <= 0.0 then 0.0
                                        elif sv >= 1.0 then 1.0
                                        else min 1.0 (dv / (1.0 - sv))
                                    | ColorBurn ->
                                        if dv >= 1.0 then 1.0
                                        elif sv <= 0.0 then 0.0
                                        else 1.0 - min 1.0 ((1.0 - dv) / sv)
                                    | SoftLight ->
                                        if sv <= 0.5 then dv - (1.0 - 2.0 * sv) * dv * (1.0 - dv)
                                        else
                                            let d = if dv <= 0.25 then ((16.0 * dv - 12.0) * dv + 4.0) * dv else sqrt dv
                                            dv + (2.0 * sv - 1.0) * (d - dv)
                                    | Difference -> abs (sv - dv)
                                    | Exclusion -> sv + dv - 2.0 * sv * dv
                                    | _ -> sv
                                (bfn cs_r cd_r, bfn cs_g cd_g, bfn cs_b cd_b)
                        // Standard premul compositing: out = (1-da)*src + (1-sa)*dst + sa*da*B
                        let compose (srcP: byte) (dstP: byte) (bv: float) =
                            let v = (1.0 - da) * float srcP + (1.0 - sa) * float dstP + sa * da * bv * 255.0
                            byte (max 0.0 (min 255.0 v))
                        { R = compose src.R dst.R br; G = compose src.G dst.G bg; B = compose src.B dst.B bb; A = byte (oa * 255.0) }
            Pixel.fromColor result

    let putPixel (fb: Framebuffer) x y (c: Color) =
        if x >= 0 && x < fb.W && y >= 0 && y < fb.H then
            let px = fb.Px.Span
            let i = y * fb.W + x
            px.[i] <- blendPx (Pixel.fromColorPremul c) px.[i]

    /// Write a pre-packed premultiplied pixel with bounds check
    let inline putPx (fb: Framebuffer) x y (ppx: uint32) =
        if x >= 0 && x < fb.W && y >= 0 && y < fb.H then
            let px = fb.Px.Span
            let i = y * fb.W + x
            px.[i] <- blendPx ppx px.[i]

    let applySpread (spread: SpreadMode) (t: float) =
        match spread with
        | Pad -> t |> max 0.0 |> min 1.0
        | Reflect ->
            let t2 = t - 2.0 * floor (t / 2.0)
            if t2 > 1.0 then 2.0 - t2 else t2
        | Repeat ->
            let t' = t - floor t
            if t' < 0.0 then t' + 1.0 else t'

    let sampleGradientStops (stops: GradientStop list) (t: float) =
        let sorted = stops |> List.sortBy (fun s -> s.Offset)
        match sorted with
        | [] -> Color.black
        | [s] -> s.StopColor
        | _ ->
            if t <= sorted.Head.Offset then sorted.Head.StopColor
            elif t >= (List.last sorted).Offset then (List.last sorted).StopColor
            else
                let rec findPair lst =
                    match lst with
                    | a :: (b :: _ as rest) ->
                        if t >= a.Offset && t <= b.Offset then (a, b)
                        else findPair rest
                    | _ -> (sorted.Head, sorted.Head)
                let (a, b) = findPair sorted
                let f = if b.Offset = a.Offset then 0.0 else (t - a.Offset) / (b.Offset - a.Offset)
                // Interpolate in premultiplied-alpha space (as browsers do, and to
                // match this backend's premultiplied compositing): a fade to a
                // transparent stop keeps the opaque neighbour's hue rather than
                // bleeding through black. Returned as straight (un-premul) Color
                // since the rest of the pipeline premultiplies at composite time.
                let c0 = a.StopColor
                let c1 = b.StopColor
                let a0 = float c0.A / 255.0
                let a1 = float c1.A / 255.0
                let lf (v0: float) (v1: float) = v0 + (v1 - v0) * f
                let pr = lf (float c0.R * a0) (float c1.R * a1)
                let pg = lf (float c0.G * a0) (float c1.G * a1)
                let pb = lf (float c0.B * a0) (float c1.B * a1)
                let pa = lf a0 a1
                if pa <= 0.0 then Color.rgba 0uy 0uy 0uy 0uy
                else
                    // pr/pg/pb are premultiplied in 0..255; un-premultiply by /pa.
                    let toByte (v: float) = byte (max 0.0 (min 255.0 (v / pa + 0.5)))
                    Color.rgba (toByte pr) (toByte pg) (toByte pb) (byte (max 0.0 (min 255.0 (pa * 255.0 + 0.5))))

    let sampleGradientStopsSortedPremul (sorted: GradientStop[]) (t: float) =
        if sorted.Length = 0 then
            Pixel.fromColorPremul Color.black
        elif sorted.Length = 1 || t <= sorted.[0].Offset then
            Pixel.fromColorPremul sorted.[0].StopColor
        elif t >= sorted.[sorted.Length - 1].Offset then
            Pixel.fromColorPremul sorted.[sorted.Length - 1].StopColor
        else
            let mutable idx = 0
            while idx + 1 < sorted.Length && not (t >= sorted.[idx].Offset && t <= sorted.[idx + 1].Offset) do
                idx <- idx + 1
            let a = sorted.[idx]
            let b = sorted.[min (idx + 1) (sorted.Length - 1)]
            let f = if b.Offset = a.Offset then 0.0 else (t - a.Offset) / (b.Offset - a.Offset)
            let c0 = a.StopColor
            let c1 = b.StopColor
            let a0 = float c0.A / 255.0
            let a1 = float c1.A / 255.0
            let lf (v0: float) (v1: float) = v0 + (v1 - v0) * f
            let pr = lf (float c0.R * a0) (float c1.R * a1)
            let pg = lf (float c0.G * a0) (float c1.G * a1)
            let pb = lf (float c0.B * a0) (float c1.B * a1)
            let pa = lf a0 a1
            if pa <= 0.0 then 0u
            else
                let toByte (v: float) = byte (max 0.0 (min 255.0 (v + 0.5)))
                Pixel.pack (toByte pb) (toByte pg) (toByte pr) (byte (max 0.0 (min 255.0 (pa * 255.0 + 0.5))))

    let sampleBilinear (src: Framebuffer) (u: float) (v: float) : uint32 =
        let x0 = int (floor u)
        let y0 = int (floor v)
        let x1 = x0 + 1
        let y1 = y0 + 1
        let fx = u - float x0
        let fy = v - float y0
        let clampX x = max 0 (min (src.W - 1) x)
        let clampY y = max 0 (min (src.H - 1) y)
        let spx = src.Px.Span
        let c00 = spx.[clampY y0 * src.W + clampX x0]
        let c10 = spx.[clampY y0 * src.W + clampX x1]
        let c01 = spx.[clampY y1 * src.W + clampX x0]
        let c11 = spx.[clampY y1 * src.W + clampX x1]
        let lerp (a: byte) (b: byte) (t: float) = float a + (float b - float a) * t
        let r = lerp (byte (lerp (Pixel.r c00) (Pixel.r c10) fx)) (byte (lerp (Pixel.r c01) (Pixel.r c11) fx)) fy
        let g = lerp (byte (lerp (Pixel.g c00) (Pixel.g c10) fx)) (byte (lerp (Pixel.g c01) (Pixel.g c11) fx)) fy
        let b = lerp (byte (lerp (Pixel.b c00) (Pixel.b c10) fx)) (byte (lerp (Pixel.b c01) (Pixel.b c11) fx)) fy
        let a = lerp (byte (lerp (Pixel.a c00) (Pixel.a c10) fx)) (byte (lerp (Pixel.a c01) (Pixel.a c11) fx)) fy
        Pixel.pack (byte (max 0.0 (min 255.0 b))) (byte (max 0.0 (min 255.0 g))) (byte (max 0.0 (min 255.0 r))) (byte (max 0.0 (min 255.0 a)))

    let samplePaintAt (paint: Paint) (x: int) (y: int) =
        match paint with
        | NoPaint -> Color.black
        | Solid c -> c
        | LinearGradient (p0, p1, stops, spread) ->
            let dx = float p1.X - float p0.X
            let dy = float p1.Y - float p0.Y
            let len2 = dx * dx + dy * dy
            if len2 < 0.0001 then
                match stops with
                | [] -> Color.black
                | s :: _ -> s.StopColor
            else
                let px = float x - float p0.X
                let py = float y - float p0.Y
                let tRaw = (px * dx + py * dy) / len2
                let t = applySpread spread tRaw
                sampleGradientStops stops t
        | RadialGradient (center, radius, stops, spread) ->
            let r = float radius
            if r < 0.0001 then
                match stops with
                | [] -> Color.black
                | s :: _ -> s.StopColor
            else
                let dx = float x - float center.X
                let dy = float y - float center.Y
                let dist = sqrt (dx * dx + dy * dy)
                let tRaw = dist / r
                let t = applySpread spread tRaw
                sampleGradientStops stops t
        | ConicGradient (center, angle, stops, spread) ->
            let dx = float x - float center.X
            let dy = float y - float center.Y
            let theta = atan2 dy dx
            let pi2 = 2.0 * System.Math.PI
            let mutable tRaw = (theta - angle) / pi2
            tRaw <- tRaw - floor tRaw
            let t = applySpread spread tRaw
            sampleGradientStops stops t
        | FocalRadialGradient (center, radius, focus, stops, spread) ->
            let r = float radius
            if r < 0.0001 then
                match stops with
                | [] -> Color.black
                | s :: _ -> s.StopColor
            else
                let fx = float focus.X - float center.X
                let fy = float focus.Y - float center.Y
                let dx = float x - float focus.X
                let dy = float y - float focus.Y
                let a = dx * dx + dy * dy
                let b = 2.0 * (dx * fx + dy * fy)
                let c = fx * fx + fy * fy - r * r
                let disc = b * b - 4.0 * a * c
                let tRaw =
                    if disc < 0.0 || a < 1e-10 then
                        let dist = sqrt (dx * dx + dy * dy)
                        dist / r
                    else
                        let sq = sqrt disc
                        let t1 = (-b + sq) / (2.0 * a)
                        let t2 = (-b - sq) / (2.0 * a)
                        let t = if t1 > 0.0 then t1 else t2
                        if t > 0.0 then 1.0 / t else 0.0
                let t = applySpread spread tRaw
                sampleGradientStops stops t
        | TilingPaint (src, xform, tileX, tileY) ->
            let inv = Matrix3x2.inverse xform
            let u = inv.M11 * float x + inv.M21 * float y + inv.DX
            let v = inv.M12 * float x + inv.M22 * float y + inv.DY
            let applyTile (mode: SpreadMode) (coord: float) (size: int) =
                let s = float size
                match mode with
                | Pad -> max 0.0 (min (s - 1.0) coord)
                | Repeat ->
                    let t = coord - s * floor (coord / s)
                    if t < 0.0 then t + s else t
                | Reflect ->
                    let t = coord / s
                    let t2 = t - 2.0 * floor (t / 2.0)
                    let t' = if t2 > 1.0 then 2.0 - t2 else t2
                    t' * s
            let su = applyTile tileX u src.W
            let sv = applyTile tileY v src.H
            Pixel.toColor (sampleBilinear src su sv) |> Color.fromPremul
        | ImagePaint (src, xform) ->
            let inv = Matrix3x2.inverse xform
            let u = inv.M11 * float x + inv.M21 * float y + inv.DX
            let v = inv.M12 * float x + inv.M22 * float y + inv.DY
            if u < -0.5 || u > float src.W - 0.5 || v < -0.5 || v > float src.H - 0.5 then
                { R = 0uy; G = 0uy; B = 0uy; A = 0uy }
            else
                Pixel.toColor (sampleBilinear src u v) |> Color.fromPremul

    let bresenham (fb: Framebuffer) (a: Vec2) (b: Vec2) (c: Color) (width: float<px>) =
        let x0 = int (float a.X) in let y0 = int (float a.Y)
        let x1 = int (float b.X) in let y1 = int (float b.Y)
        let dx = abs (x1 - x0) in let dy = -(abs (y1 - y0))
        let sx = if x0 < x1 then 1 else -1
        let sy = if y0 < y1 then 1 else -1
        let hw = int (float width / 2.0)
        let rec loop x y err =
            for oy = -hw to hw do
                for ox = -hw to hw do
                    putPixel fb (x + ox) (y + oy) c
            if x = x1 && y = y1 then ()
            else
                let e2 = 2 * err
                let err, x = if e2 >= dy then err + dy, x + sx else err, x
                let err, y = if e2 <= dx then err + dx, y + sy else err, y
                loop x y err
        loop x0 y0 (dx + dy)

    let fillRect (fb: Framebuffer) (r: Rect) (c: Color) =
        let x0 = max 0 (int (float r.Pos.X))
        let y0 = max 0 (int (float r.Pos.Y))
        let x1 = min fb.W (int (float (r.Pos.X + r.Size.X)))
        let y1 = min fb.H (int (float (r.Pos.Y + r.Size.Y)))
        if x1 <= x0 || y1 <= y0 then () else
        let premul = Pixel.fromColorPremul c
        let pa = Pixel.a premul
        let px = fb.Px.Span
        if pa = 255uy then
            // Opaque: overwrite entire spans
            for y = y0 to y1 - 1 do
                px.Slice(y * fb.W + x0, x1 - x0).Fill(premul)
        elif pa > 0uy then
            // Semi-transparent: blend, but skip per-pixel bounds check + toPremul
            for y = y0 to y1 - 1 do
                let rowOff = y * fb.W
                for x = x0 to x1 - 1 do
                    px.[rowOff + x] <- blendPx premul px.[rowOff + x]

    let fillPaintRect (fb: Framebuffer) (x: int) (y: int) (w: int) (h: int) (paint: Paint) =
        let x0 = max 0 x
        let y0 = max 0 y
        let x1 = min fb.W (x + w)
        let y1 = min fb.H (y + h)
        if x1 > x0 && y1 > y0 then
            match paint with
            | NoPaint -> ()
            | Solid c -> fillRect fb (rect (float x0) (float y0) (float (x1 - x0)) (float (y1 - y0))) c
            | LinearGradient (p0, p1, stops, spread) ->
                let sorted = stops |> List.sortBy (fun s -> s.Offset) |> List.toArray
                let dx = float p1.X - float p0.X
                let dy = float p1.Y - float p0.Y
                let len2 = dx * dx + dy * dy
                if len2 < 0.0001 then
                    let c =
                        if sorted.Length = 0 then Color.black
                        else sorted.[0].StopColor
                    fillRect fb (rect (float x0) (float y0) (float (x1 - x0)) (float (y1 - y0))) c
                else
                    let px = fb.Px.Span
                    let invLen2 = 1.0 / len2
                    let stepX = dx * invLen2
                    let p0x = float p0.X
                    let p0y = float p0.Y
                    for py = y0 to y1 - 1 do
                        let row = py * fb.W
                        let mutable tRaw = ((float x0 - p0x) * dx + (float py - p0y) * dy) * invLen2
                        for pxX = x0 to x1 - 1 do
                            let premul = sampleGradientStopsSortedPremul sorted (applySpread spread tRaw)
                            let a = Pixel.a premul
                            if a = 255uy then
                                px.[row + pxX] <- premul
                            elif a > 0uy then
                                px.[row + pxX] <- blendPx premul px.[row + pxX]
                            tRaw <- tRaw + stepX
            | RadialGradient (center, radius, stops, spread) ->
                let sorted = stops |> List.sortBy (fun s -> s.Offset) |> List.toArray
                let r = float radius
                if r < 0.0001 then
                    let c =
                        if sorted.Length = 0 then Color.black
                        else sorted.[0].StopColor
                    fillRect fb (rect (float x0) (float y0) (float (x1 - x0)) (float (y1 - y0))) c
                else
                    let px = fb.Px.Span
                    let invR = 1.0 / r
                    let cx = float center.X
                    let cy = float center.Y
                    for py = y0 to y1 - 1 do
                        let row = py * fb.W
                        let dy = float py - cy
                        for pxX = x0 to x1 - 1 do
                            let dx = float pxX - cx
                            let tRaw = sqrt (dx * dx + dy * dy) * invR
                            let premul = sampleGradientStopsSortedPremul sorted (applySpread spread tRaw)
                            let a = Pixel.a premul
                            if a = 255uy then
                                px.[row + pxX] <- premul
                            elif a > 0uy then
                                px.[row + pxX] <- blendPx premul px.[row + pxX]
            | _ ->
                let px = fb.Px.Span
                for py = y0 to y1 - 1 do
                    let row = py * fb.W
                    for pxX = x0 to x1 - 1 do
                        let c = samplePaintAt paint pxX py
                        let premul = Pixel.fromColorPremul c
                        let a = Pixel.a premul
                        if a = 255uy then
                            px.[row + pxX] <- premul
                        elif a > 0uy then
                            px.[row + pxX] <- blendPx premul px.[row + pxX]

    /// Fast rounded rect fill using circle math for corner rows.
    /// Avoids path flattening + scanline intersection entirely.
    let fillRoundedRectFast (fb: Framebuffer) (ox: float) (oy: float) (w: float) (h: float) (cr: float) (c: Color) =
        let premul = Pixel.fromColorPremul c
        let pa = Pixel.a premul
        if pa = 0uy then () else
        let px = fb.Px.Span
        let cr = min cr (min (w / 2.0) (h / 2.0))
        let r2 = cr * cr
        // Use pixel-center test: only process rows where center (row+0.5) is inside [oy, oy+h]
        let y0 = max 0 (int (ceil (oy - 0.5)))
        let y1 = min fb.H (int (floor (oy + h - 0.5)) + 1)
        for row = y0 to y1 - 1 do
            let fy = float row + 0.5
            let dy_top = (oy + cr) - fy
            let dy_bot = fy - (oy + h - cr)
            let xStart, xEnd =
                if dy_top > 0.0 && dy_top <= cr then
                    let dx = sqrt (max 0.0 (r2 - dy_top * dy_top))
                    (int (ceil (ox + cr - dx)), int (floor (ox + w - cr + dx)) + 1)
                elif dy_bot > 0.0 && dy_bot <= cr then
                    let dx = sqrt (max 0.0 (r2 - dy_bot * dy_bot))
                    (int (ceil (ox + cr - dx)), int (floor (ox + w - cr + dx)) + 1)
                else
                    (int (ceil ox), int (floor (ox + w)) + 1)
            let xs = max 0 xStart
            let xe = min fb.W xEnd
            if xe > xs then
                let rowOff = row * fb.W
                if pa = 255uy then
                    px.Slice(rowOff + xs, xe - xs).Fill(premul)
                else
                    for x = xs to xe - 1 do
                        let i = rowOff + x
                        px.[i] <- blendPx premul px.[i]

    /// Fast rounded rect fill with 4 independent corner radii.
    let fillRoundedRect4Fast (fb: Framebuffer) (ox: float) (oy: float) (w: float) (h: float)
                             (rtl: float) (rtr: float) (rbr: float) (rbl: float) (c: Color) =
        let premul = Pixel.fromColorPremul c
        let pa = Pixel.a premul
        if pa = 0uy then () else
        let px = fb.Px.Span
        let maxR = min (w / 2.0) (h / 2.0)
        let rtl = min rtl maxR
        let rtr = min rtr maxR
        let rbr = min rbr maxR
        let rbl = min rbl maxR
        // Use pixel-center test: only process rows where center (row+0.5) is inside [oy, oy+h]
        let y0 = max 0 (int (ceil (oy - 0.5)))
        let y1 = min fb.H (int (floor (oy + h - 0.5)) + 1)
        for row = y0 to y1 - 1 do
            let fy = float row + 0.5
            // Left edge
            let mutable xLeft = ox
            let dy_tl = (oy + rtl) - fy
            let dy_bl = fy - (oy + h - rbl)
            if dy_tl > 0.0 && dy_tl <= rtl then
                let dx = sqrt (max 0.0 (rtl * rtl - dy_tl * dy_tl))
                xLeft <- ox + rtl - dx
            elif dy_bl > 0.0 && dy_bl <= rbl then
                let dx = sqrt (max 0.0 (rbl * rbl - dy_bl * dy_bl))
                xLeft <- ox + rbl - dx
            // Right edge
            let mutable xRight = ox + w
            let dy_tr = (oy + rtr) - fy
            let dy_br = fy - (oy + h - rbr)
            if dy_tr > 0.0 && dy_tr <= rtr then
                let dx = sqrt (max 0.0 (rtr * rtr - dy_tr * dy_tr))
                xRight <- ox + w - rtr + dx
            elif dy_br > 0.0 && dy_br <= rbr then
                let dx = sqrt (max 0.0 (rbr * rbr - dy_br * dy_br))
                xRight <- ox + w - rbr + dx
            let xs = max 0 (int (ceil xLeft))
            let xe = min fb.W (int (floor xRight) + 1)
            if xe > xs then
                let rowOff = row * fb.W
                if pa = 255uy then
                    px.Slice(rowOff + xs, xe - xs).Fill(premul)
                else
                    for x = xs to xe - 1 do
                        let i = rowOff + x
                        px.[i] <- blendPx premul px.[i]

    let strokeRect (fb: Framebuffer) (r: Rect) (c: Color) (w: float<px>) =
        let tl = r.Pos
        let tr = { X = r.Pos.X + r.Size.X; Y = r.Pos.Y }
        let br = { X = r.Pos.X + r.Size.X; Y = r.Pos.Y + r.Size.Y }
        let bl = { X = r.Pos.X; Y = r.Pos.Y + r.Size.Y }
        bresenham fb tl tr c w
        bresenham fb tr br c w
        bresenham fb br bl c w
        bresenham fb bl tl c w

    let fillCircle (fb: Framebuffer) (center: Vec2) (radius: float<px>) (c: Color) =
        let cx = int (float center.X)
        let cy = int (float center.Y)
        let r = int (float radius)
        let r2 = r * r
        for dy = -r to r do
            let halfW = int (sqrt (float (r2 - dy * dy)))
            for dx = -halfW to halfW do
                putPixel fb (cx + dx) (cy + dy) c

    let fillTri (fb: Framebuffer) (v0: Vec2) (v1: Vec2) (v2: Vec2) (c: Color) =
        let x0 = float v0.X in let y0 = float v0.Y
        let x1 = float v1.X in let y1 = float v1.Y
        let x2 = float v2.X in let y2 = float v2.Y
        let minX = max 0 (int (min x0 (min x1 x2)))
        let minY = max 0 (int (min y0 (min y1 y2)))
        let maxX = min (fb.W - 1) (int (max x0 (max x1 x2)))
        let maxY = min (fb.H - 1) (int (max y0 (max y1 y2)))
        let edge ax ay bx by px py = (px - ax) * (by - ay) - (py - ay) * (bx - ax)
        let area = edge x0 y0 x1 y1 x2 y2
        if abs area < 0.001 then () // degenerate
        else
            for y = minY to maxY do
                for x = minX to maxX do
                    let px = float x + 0.5
                    let py = float y + 0.5
                    let w0 = edge x1 y1 x2 y2 px py
                    let w1 = edge x2 y2 x0 y0 px py
                    let w2 = edge x0 y0 x1 y1 px py
                    let inside =
                        if area > 0.0 then w0 >= 0.0 && w1 >= 0.0 && w2 >= 0.0
                        else w0 <= 0.0 && w1 <= 0.0 && w2 <= 0.0
                    if inside then putPixel fb x y c

    let clearFb (fb: Framebuffer) (c: Color) =
        fb.Px.Span.Fill(Pixel.fromColorPremul c)

    // ── Bezier flattening ──

    let inline private dist2 (ax: float) (ay: float) (bx: float) (by: float) =
        let dx = ax - bx
        let dy = ay - by
        dx * dx + dy * dy

    let flattenQuad (x0: float) (y0: float) (cx: float) (cy: float) (x1: float) (y1: float) (tol: float) =
        let acc = ResizeArray<float * float>()
        let rec sub x0 y0 cx cy x1 y1 depth =
            let mx = (x0 + 2.0 * cx + x1) / 4.0
            let my = (y0 + 2.0 * cy + y1) / 4.0
            if depth > 8 || dist2 mx my ((x0 + x1) / 2.0) ((y0 + y1) / 2.0) < tol * tol then
                acc.Add(x1, y1)
            else
                let q0x = (x0 + cx) / 2.0
                let q0y = (y0 + cy) / 2.0
                let q1x = (cx + x1) / 2.0
                let q1y = (cy + y1) / 2.0
                let mx2 = (q0x + q1x) / 2.0
                let my2 = (q0y + q1y) / 2.0
                sub x0 y0 q0x q0y mx2 my2 (depth + 1)
                sub mx2 my2 q1x q1y x1 y1 (depth + 1)
        sub x0 y0 cx cy x1 y1 0
        acc

    let flattenCubic (x0: float) (y0: float) (c1x: float) (c1y: float) (c2x: float) (c2y: float) (x1: float) (y1: float) (tol: float) =
        let acc = ResizeArray<float * float>()
        let rec sub x0 y0 c1x c1y c2x c2y x1 y1 depth =
            let mx = (x0 + 3.0 * c1x + 3.0 * c2x + x1) / 8.0
            let my = (y0 + 3.0 * c1y + 3.0 * c2y + y1) / 8.0
            if depth > 8 || dist2 mx my ((x0 + x1) / 2.0) ((y0 + y1) / 2.0) < tol * tol then
                acc.Add(x1, y1)
            else
                let ab_x = (x0 + c1x) / 2.0
                let ab_y = (y0 + c1y) / 2.0
                let bc_x = (c1x + c2x) / 2.0
                let bc_y = (c1y + c2y) / 2.0
                let cd_x = (c2x + x1) / 2.0
                let cd_y = (c2y + y1) / 2.0
                let abc_x = (ab_x + bc_x) / 2.0
                let abc_y = (ab_y + bc_y) / 2.0
                let bcd_x = (bc_x + cd_x) / 2.0
                let bcd_y = (bc_y + cd_y) / 2.0
                let mid_x = (abc_x + bcd_x) / 2.0
                let mid_y = (abc_y + bcd_y) / 2.0
                sub x0 y0 ab_x ab_y abc_x abc_y mid_x mid_y (depth + 1)
                sub mid_x mid_y bcd_x bcd_y cd_x cd_y x1 y1 (depth + 1)
        sub x0 y0 c1x c1y c2x c2y x1 y1 0
        acc

    let flattenArc (sx: float) (sy: float) (rx: float) (ry: float) (rotation: float) (largeArc: bool) (sweep: bool) (ex: float) (ey: float) (tol: float) =
        let acc = ResizeArray<float * float>()
        let pi = System.Math.PI
        let mutable rx = abs rx
        let mutable ry = abs ry
        if rx < 0.0001 || ry < 0.0001 then
            acc.Add(ex, ey)
        else
            let cosR = cos rotation
            let sinR = sin rotation
            let dx2 = (sx - ex) / 2.0
            let dy2 = (sy - ey) / 2.0
            let x1p = cosR * dx2 + sinR * dy2
            let y1p = -sinR * dx2 + cosR * dy2
            let x1p2 = x1p * x1p
            let y1p2 = y1p * y1p
            let rx2 = rx * rx
            let ry2 = ry * ry
            let lambda = x1p2 / rx2 + y1p2 / ry2
            if lambda > 1.0 then
                let s = sqrt lambda
                rx <- rx * s
                ry <- ry * s
            let rx2 = rx * rx
            let ry2 = ry * ry
            let num = rx2 * ry2 - rx2 * y1p2 - ry2 * x1p2
            let den = rx2 * y1p2 + ry2 * x1p2
            let sq = if den < 0.0001 then 0.0 else sqrt (max 0.0 (num / den))
            let sign = if largeArc = sweep then -1.0 else 1.0
            let cxp = sign * sq * rx * y1p / ry
            let cyp = sign * sq * (-ry) * x1p / rx
            let cx = cosR * cxp - sinR * cyp + (sx + ex) / 2.0
            let cy = sinR * cxp + cosR * cyp + (sy + ey) / 2.0
            let angleBetween ux uy vx vy =
                let dot = ux * vx + uy * vy
                let len = sqrt (ux * ux + uy * uy) * sqrt (vx * vx + vy * vy)
                let c = if len < 1e-10 then 1.0 else max -1.0 (min 1.0 (dot / len))
                let a = acos c
                if ux * vy - uy * vx < 0.0 then -a else a
            let theta1 = angleBetween 1.0 0.0 ((x1p - cxp) / rx) ((y1p - cyp) / ry)
            let mutable dtheta = angleBetween ((x1p - cxp) / rx) ((y1p - cyp) / ry) ((-x1p - cxp) / rx) ((-y1p - cyp) / ry)
            if not sweep && dtheta > 0.0 then dtheta <- dtheta - 2.0 * pi
            elif sweep && dtheta < 0.0 then dtheta <- dtheta + 2.0 * pi
            let steps = max 4 (int (abs dtheta / 0.2) + 1)
            for i = 1 to steps do
                let t = float i / float steps
                let theta = theta1 + dtheta * t
                let px = rx * cos theta
                let py = ry * sin theta
                let x = cosR * px - sinR * py + cx
                let y = sinR * px + cosR * py + cy
                acc.Add(x, y)
        acc

    let flattenPath (segs: PathSeg list) : (float * float) list list =
        let contours = ResizeArray<(float * float) list>()
        let cur = ResizeArray<float * float>()
        let mutable cx, cy = 0.0, 0.0
        let mutable startX, startY = 0.0, 0.0
        let finishContour () =
            if cur.Count > 1 then
                contours.Add(Seq.toList cur)
            cur.Clear()
        for seg in segs do
            match seg with
            | MoveTo p ->
                finishContour ()
                cx <- float p.X; cy <- float p.Y
                startX <- cx; startY <- cy
                cur.Add(cx, cy)
            | LineTo p ->
                let nx, ny = float p.X, float p.Y
                cur.Add(nx, ny)
                cx <- nx; cy <- ny
            | QuadTo (cp, pt) ->
                let pts = flattenQuad cx cy (float cp.X) (float cp.Y) (float pt.X) (float pt.Y) 0.5
                for p in pts do cur.Add(p)
                cx <- float pt.X; cy <- float pt.Y
            | CubicTo (c1, c2, pt) ->
                let pts = flattenCubic cx cy (float c1.X) (float c1.Y) (float c2.X) (float c2.Y) (float pt.X) (float pt.Y) 0.5
                for p in pts do cur.Add(p)
                cx <- float pt.X; cy <- float pt.Y
            | ArcTo (rx, ry, rot, la, sw, pt) ->
                let pts = flattenArc cx cy (float rx) (float ry) rot la sw (float pt.X) (float pt.Y) 0.5
                for p in pts do cur.Add(p)
                cx <- float pt.X; cy <- float pt.Y
            | Close ->
                if cur.Count > 0 then
                    cur.Add(startX, startY)
                    cx <- startX; cy <- startY
        finishContour ()
        Seq.toList contours
    /// Stroke contours retain whether they ended with Close. An open contour
    /// returning to its start still has two endpoint caps, not a closing join.
    let flattenStrokePath (segs: PathSeg list) =
        let contours = ResizeArray<(float * float) list * bool>()
        let current = ResizeArray<PathSeg>()
        let mutable start = vec2 0.0 0.0
        let finish closed =
            for contour in flattenPath (List.ofSeq current) do
                let points =
                    contour
                    |> List.fold (fun acc point ->
                        match acc with
                        | previous :: _ when previous = point -> acc
                        | _ -> point :: acc) []
                    |> List.rev
                if points.Length > 1 then contours.Add(points, closed)
            current.Clear()
        for segment in segs do
            match segment with
            | MoveTo point ->
                finish false
                start <- point
                current.Add segment
            | Close ->
                current.Add Close
                finish true
                // A later segment after Close begins at the closed start.
                current.Add(MoveTo start)
            | _ -> current.Add segment
        finish false
        List.ofSeq contours


    // ── Scanline fill (even-odd rule) ──

    let scanlineFill (fb: Framebuffer) (contours: (float * float) list list) (c: Color) (fillRule: FillRule) =
        let mutable minY = fb.H
        let mutable maxY = 0
        for contour in contours do
            for (_, y) in contour do
                let iy = int y
                if iy < minY then minY <- iy
                if iy > maxY then maxY <- iy
        minY <- max 0 minY
        maxY <- min (fb.H - 1) maxY
        match fillRule with
        | EvenOdd ->
            let xs = ResizeArray<float>()
            for row = minY to maxY do
                xs.Clear()
                let scanY = float row + 0.5
                for contour in contours do
                    let pts = contour
                    let n = List.length pts
                    if n >= 2 then
                        for i = 0 to n - 2 do
                            let (x0, y0) = pts.[i]
                            let (x1, y1) = pts.[i + 1]
                            if (y0 <= scanY && y1 > scanY) || (y1 <= scanY && y0 > scanY) then
                                let t = (scanY - y0) / (y1 - y0)
                                xs.Add(x0 + t * (x1 - x0))
                xs.Sort()
                let mutable i = 0
                while i + 1 < xs.Count do
                    let xStart = max 0 (int (ceil xs.[i]))
                    let xEnd = min (fb.W - 1) (int (floor xs.[i + 1]))
                    for x = xStart to xEnd do
                        putPixel fb x row c
                    i <- i + 2
        | NonZero ->
            let crossings = ResizeArray<float * int>()
            for row = minY to maxY do
                crossings.Clear()
                let scanY = float row + 0.5
                for contour in contours do
                    let pts = contour
                    let n = List.length pts
                    if n >= 2 then
                        for i = 0 to n - 2 do
                            let (x0, y0) = pts.[i]
                            let (x1, y1) = pts.[i + 1]
                            if (y0 <= scanY && y1 > scanY) || (y1 <= scanY && y0 > scanY) then
                                let t = (scanY - y0) / (y1 - y0)
                                let xHit = x0 + t * (x1 - x0)
                                let dir = if y0 < y1 then 1 else -1
                                crossings.Add(xHit, dir)
                crossings.Sort(fun (a, _) (b, _) -> compare a b)
                let mutable winding = 0
                let mutable i = 0
                while i < crossings.Count do
                    let (xHit, dir) = crossings.[i]
                    let prevWinding = winding
                    winding <- winding + dir
                    if prevWinding = 0 && winding <> 0 then
                        let spanStart = xHit
                        i <- i + 1
                        while i < crossings.Count && winding <> 0 do
                            let (xh2, d2) = crossings.[i]
                            winding <- winding + d2
                            if winding = 0 then
                                let xStart = max 0 (int (ceil spanStart))
                                let xEnd = min (fb.W - 1) (int (floor xh2))
                                for x = xStart to xEnd do
                                    putPixel fb x row c
                            i <- i + 1
                    else
                        i <- i + 1

    let scanlineFillPaint (fb: Framebuffer) (contours: (float * float) list list) (paint: Paint) (fillRule: FillRule) =
        match paint with
        | NoPaint -> ()
        | Solid c -> scanlineFill fb contours c fillRule
        | LinearGradient _ | RadialGradient _ | ConicGradient _ | FocalRadialGradient _ | TilingPaint _ | ImagePaint _ ->
            let mutable minY = fb.H
            let mutable maxY = 0
            for contour in contours do
                for (_, y) in contour do
                    let iy = int y
                    if iy < minY then minY <- iy
                    if iy > maxY then maxY <- iy
            minY <- max 0 minY
            maxY <- min (fb.H - 1) maxY
            match fillRule with
            | EvenOdd ->
                let xs = ResizeArray<float>()
                for row = minY to maxY do
                    xs.Clear()
                    let scanY = float row + 0.5
                    for contour in contours do
                        let pts = contour
                        let n = List.length pts
                        if n >= 2 then
                            for i = 0 to n - 2 do
                                let (x0, y0) = pts.[i]
                                let (x1, y1) = pts.[i + 1]
                                if (y0 <= scanY && y1 > scanY) || (y1 <= scanY && y0 > scanY) then
                                    let t = (scanY - y0) / (y1 - y0)
                                    xs.Add(x0 + t * (x1 - x0))
                    xs.Sort()
                    let mutable i = 0
                    while i + 1 < xs.Count do
                        let xStart = max 0 (int (ceil xs.[i]))
                        let xEnd = min (fb.W - 1) (int (floor xs.[i + 1]))
                        for x = xStart to xEnd do
                            let c = samplePaintAt paint x row
                            putPixel fb x row c
                        i <- i + 2
            | NonZero ->
                let crossings = ResizeArray<float * int>()
                for row = minY to maxY do
                    crossings.Clear()
                    let scanY = float row + 0.5
                    for contour in contours do
                        let pts = contour
                        let n = List.length pts
                        if n >= 2 then
                            for i = 0 to n - 2 do
                                let (x0, y0) = pts.[i]
                                let (x1, y1) = pts.[i + 1]
                                if (y0 <= scanY && y1 > scanY) || (y1 <= scanY && y0 > scanY) then
                                    let t = (scanY - y0) / (y1 - y0)
                                    let xHit = x0 + t * (x1 - x0)
                                    let dir = if y0 < y1 then 1 else -1
                                    crossings.Add(xHit, dir)
                    crossings.Sort(fun (a, _) (b, _) -> compare a b)
                    let mutable winding = 0
                    let mutable i = 0
                    while i < crossings.Count do
                        let (xHit, dir) = crossings.[i]
                        let prevWinding = winding
                        winding <- winding + dir
                        if prevWinding = 0 && winding <> 0 then
                            let spanStart = xHit
                            i <- i + 1
                            while i < crossings.Count && winding <> 0 do
                                let (xh2, d2) = crossings.[i]
                                winding <- winding + d2
                                if winding = 0 then
                                    let xStart = max 0 (int (ceil spanStart))
                                    let xEnd = min (fb.W - 1) (int (floor xh2))
                                    for x = xStart to xEnd do
                                        let c = samplePaintAt paint x row
                                        putPixel fb x row c
                                i <- i + 1
                        else
                            i <- i + 1

    let buildClipMask (w: int) (h: int) (contours: (float * float) list list) (fillRule: FillRule) : byte[] =
        let mask = Array.zeroCreate (w * h)
        let mutable minY = h
        let mutable maxY = 0
        for contour in contours do
            for (_, y) in contour do
                let iy = int y
                if iy < minY then minY <- iy
                if iy > maxY then maxY <- iy
        minY <- max 0 minY
        maxY <- min (h - 1) maxY
        match fillRule with
        | EvenOdd ->
            let xs = ResizeArray<float>()
            for row = minY to maxY do
                xs.Clear()
                let scanY = float row + 0.5
                for contour in contours do
                    let pts = contour
                    let n = List.length pts
                    if n >= 2 then
                        for i = 0 to n - 2 do
                            let (x0, y0) = pts.[i]
                            let (x1, y1) = pts.[i + 1]
                            if (y0 <= scanY && y1 > scanY) || (y1 <= scanY && y0 > scanY) then
                                let t = (scanY - y0) / (y1 - y0)
                                xs.Add(x0 + t * (x1 - x0))
                xs.Sort()
                let mutable i = 0
                while i + 1 < xs.Count do
                    let xStart = max 0 (int (ceil xs.[i]))
                    let xEnd = min (w - 1) (int (floor xs.[i + 1]))
                    for x = xStart to xEnd do
                        mask.[row * w + x] <- 255uy
                    i <- i + 2
        | NonZero ->
            let crossings = ResizeArray<float * int>()
            for row = minY to maxY do
                crossings.Clear()
                let scanY = float row + 0.5
                for contour in contours do
                    let pts = contour
                    let n = List.length pts
                    if n >= 2 then
                        for i = 0 to n - 2 do
                            let (x0, y0) = pts.[i]
                            let (x1, y1) = pts.[i + 1]
                            if (y0 <= scanY && y1 > scanY) || (y1 <= scanY && y0 > scanY) then
                                let t = (scanY - y0) / (y1 - y0)
                                let xHit = x0 + t * (x1 - x0)
                                let dir = if y0 < y1 then 1 else -1
                                crossings.Add(xHit, dir)
                crossings.Sort(fun (a, _) (b, _) -> compare a b)
                let mutable winding = 0
                let mutable i = 0
                while i < crossings.Count do
                    let (xHit, dir) = crossings.[i]
                    let prevWinding = winding
                    winding <- winding + dir
                    if prevWinding = 0 && winding <> 0 then
                        let spanStart = xHit
                        i <- i + 1
                        while i < crossings.Count && winding <> 0 do
                            let (xh2, d2) = crossings.[i]
                            winding <- winding + d2
                            if winding = 0 then
                                let xStart = max 0 (int (ceil spanStart))
                                let xEnd = min (w - 1) (int (floor xh2))
                                for x = xStart to xEnd do
                                    mask.[row * w + x] <- 255uy
                            i <- i + 1
                    else
                        i <- i + 1
        mask

    // ── Analytical anti-aliased scanline fill ──

    let addSpanCoverage (coverage: float[]) (w: int) (x0: float) (x1: float) =
        if x1 <= x0 then () else
        let sx = max 0.0 x0
        let ex = min (float w) x1
        if ex <= sx then () else
        let pxStart = int sx
        let pxEnd = int (ex - 1e-10) |> max pxStart |> min (w - 1)
        if pxStart >= w then () else
        if pxStart = pxEnd then
            coverage.[pxStart] <- coverage.[pxStart] + (ex - sx)
        else
            coverage.[pxStart] <- coverage.[pxStart] + (float (pxStart + 1) - sx)
            for x = pxStart + 1 to pxEnd - 1 do
                coverage.[x] <- coverage.[x] + 1.0
            if pxEnd < w then
                coverage.[pxEnd] <- coverage.[pxEnd] + (ex - float pxEnd)

    let scanlineFillAA (fb: Framebuffer) (contours: (float * float) list list) (c: Color) (samples: int) (fillRule: FillRule) =
        let mutable minY = fb.H
        let mutable maxY = 0
        for contour in contours do
            for (_, y) in contour do
                let iy = int y
                if iy < minY then minY <- iy
                if iy > maxY then maxY <- iy
        minY <- max 0 minY
        maxY <- min (fb.H - 1) maxY
        let n = max 1 samples
        let coverage = Array.zeroCreate<float> fb.W
        match fillRule with
        | EvenOdd ->
            let xs = ResizeArray<float>()
            for row = minY to maxY do
                Array.fill coverage 0 fb.W 0.0
                for s = 0 to n - 1 do
                    xs.Clear()
                    let scanY = float row + (float s + 0.5) / float n
                    for contour in contours do
                        let pts = contour
                        let len = List.length pts
                        if len >= 2 then
                            for i = 0 to len - 2 do
                                let (x0, y0) = pts.[i]
                                let (x1, y1) = pts.[i + 1]
                                if (y0 <= scanY && y1 > scanY) || (y1 <= scanY && y0 > scanY) then
                                    let t = (scanY - y0) / (y1 - y0)
                                    xs.Add(x0 + t * (x1 - x0))
                    xs.Sort()
                    let mutable j = 0
                    while j + 1 < xs.Count do
                        addSpanCoverage coverage fb.W xs.[j] xs.[j + 1]
                        j <- j + 2
                for x = 0 to fb.W - 1 do
                    if coverage.[x] > 0.0 then
                        let alpha = float c.A * (min 1.0 (coverage.[x] / float n))
                        let c' = { c with A = byte (min 255.0 alpha) }
                        putPixel fb x row c'
        | NonZero ->
            let crossings = ResizeArray<float * int>()
            for row = minY to maxY do
                Array.fill coverage 0 fb.W 0.0
                for s = 0 to n - 1 do
                    crossings.Clear()
                    let scanY = float row + (float s + 0.5) / float n
                    for contour in contours do
                        let pts = contour
                        let len = List.length pts
                        if len >= 2 then
                            for i = 0 to len - 2 do
                                let (x0, y0) = pts.[i]
                                let (x1, y1) = pts.[i + 1]
                                if (y0 <= scanY && y1 > scanY) || (y1 <= scanY && y0 > scanY) then
                                    let t = (scanY - y0) / (y1 - y0)
                                    let xHit = x0 + t * (x1 - x0)
                                    let dir = if y0 < y1 then 1 else -1
                                    crossings.Add(xHit, dir)
                    crossings.Sort(fun (a, _) (b, _) -> compare a b)
                    let mutable winding = 0
                    let mutable j = 0
                    while j < crossings.Count do
                        let (xHit, dir) = crossings.[j]
                        let prevWinding = winding
                        winding <- winding + dir
                        if prevWinding = 0 && winding <> 0 then
                            let spanStart = xHit
                            j <- j + 1
                            while j < crossings.Count && winding <> 0 do
                                let (xh2, d2) = crossings.[j]
                                winding <- winding + d2
                                if winding = 0 then
                                    addSpanCoverage coverage fb.W spanStart xh2
                                j <- j + 1
                        else
                            j <- j + 1
                for x = 0 to fb.W - 1 do
                    if coverage.[x] > 0.0 then
                        let alpha = float c.A * (min 1.0 (coverage.[x] / float n))
                        let c' = { c with A = byte (min 255.0 alpha) }
                        putPixel fb x row c'

    let scanlineFillPaintAA (fb: Framebuffer) (contours: (float * float) list list) (paint: Paint) (samples: int) (fillRule: FillRule) =
        match paint with
        | NoPaint -> ()
        | Solid c -> scanlineFillAA fb contours c samples fillRule
        | LinearGradient _ | RadialGradient _ | ConicGradient _ | FocalRadialGradient _ | TilingPaint _ | ImagePaint _ ->
            let mutable minY = fb.H
            let mutable maxY = 0
            for contour in contours do
                for (_, y) in contour do
                    let iy = int y
                    if iy < minY then minY <- iy
                    if iy > maxY then maxY <- iy
            minY <- max 0 minY
            maxY <- min (fb.H - 1) maxY
            let n = max 1 samples
            let coverage = Array.zeroCreate<float> fb.W
            match fillRule with
            | EvenOdd ->
                let xs = ResizeArray<float>()
                for row = minY to maxY do
                    Array.fill coverage 0 fb.W 0.0
                    for s = 0 to n - 1 do
                        xs.Clear()
                        let scanY = float row + (float s + 0.5) / float n
                        for contour in contours do
                            let pts = contour
                            let len = List.length pts
                            if len >= 2 then
                                for i = 0 to len - 2 do
                                    let (x0, y0) = pts.[i]
                                    let (x1, y1) = pts.[i + 1]
                                    if (y0 <= scanY && y1 > scanY) || (y1 <= scanY && y0 > scanY) then
                                        let t = (scanY - y0) / (y1 - y0)
                                        xs.Add(x0 + t * (x1 - x0))
                        xs.Sort()
                        let mutable j = 0
                        while j + 1 < xs.Count do
                            addSpanCoverage coverage fb.W xs.[j] xs.[j + 1]
                            j <- j + 2
                    for x = 0 to fb.W - 1 do
                        if coverage.[x] > 0.0 then
                            let c = samplePaintAt paint x row
                            let alpha = float c.A * (min 1.0 (coverage.[x] / float n))
                            let c' = { c with A = byte (min 255.0 alpha) }
                            putPixel fb x row c'
            | NonZero ->
                let crossings = ResizeArray<float * int>()
                for row = minY to maxY do
                    Array.fill coverage 0 fb.W 0.0
                    for s = 0 to n - 1 do
                        crossings.Clear()
                        let scanY = float row + (float s + 0.5) / float n
                        for contour in contours do
                            let pts = contour
                            let len = List.length pts
                            if len >= 2 then
                                for i = 0 to len - 2 do
                                    let (x0, y0) = pts.[i]
                                    let (x1, y1) = pts.[i + 1]
                                    if (y0 <= scanY && y1 > scanY) || (y1 <= scanY && y0 > scanY) then
                                        let t = (scanY - y0) / (y1 - y0)
                                        let xHit = x0 + t * (x1 - x0)
                                        let dir = if y0 < y1 then 1 else -1
                                        crossings.Add(xHit, dir)
                        crossings.Sort(fun (a, _) (b, _) -> compare a b)
                        let mutable winding = 0
                        let mutable j = 0
                        while j < crossings.Count do
                            let (xHit, dir) = crossings.[j]
                            let prevWinding = winding
                            winding <- winding + dir
                            if prevWinding = 0 && winding <> 0 then
                                let spanStart = xHit
                                j <- j + 1
                                while j < crossings.Count && winding <> 0 do
                                    let (xh2, d2) = crossings.[j]
                                    winding <- winding + d2
                                    if winding = 0 then
                                        addSpanCoverage coverage fb.W spanStart xh2
                                    j <- j + 1
                            else
                                j <- j + 1
                    for x = 0 to fb.W - 1 do
                        if coverage.[x] > 0.0 then
                            let c = samplePaintAt paint x row
                            let alpha = float c.A * (min 1.0 (coverage.[x] / float n))
                            let c' = { c with A = byte (min 255.0 alpha) }
                            putPixel fb x row c'

    let buildClipMaskAA (w: int) (h: int) (contours: (float * float) list list) (samples: int) (fillRule: FillRule) : byte[] =
        let mask = Array.zeroCreate (w * h)
        let mutable minY = h
        let mutable maxY = 0
        for contour in contours do
            for (_, y) in contour do
                let iy = int y
                if iy < minY then minY <- iy
                if iy > maxY then maxY <- iy
        minY <- max 0 minY
        maxY <- min (h - 1) maxY
        let n = max 1 samples
        let coverage = Array.zeroCreate<float> w
        match fillRule with
        | EvenOdd ->
            let xs = ResizeArray<float>()
            for row = minY to maxY do
                Array.fill coverage 0 w 0.0
                for s = 0 to n - 1 do
                    xs.Clear()
                    let scanY = float row + (float s + 0.5) / float n
                    for contour in contours do
                        let pts = contour
                        let len = List.length pts
                        if len >= 2 then
                            for i = 0 to len - 2 do
                                let (x0, y0) = pts.[i]
                                let (x1, y1) = pts.[i + 1]
                                if (y0 <= scanY && y1 > scanY) || (y1 <= scanY && y0 > scanY) then
                                    let t = (scanY - y0) / (y1 - y0)
                                    xs.Add(x0 + t * (x1 - x0))
                    xs.Sort()
                    let mutable j = 0
                    while j + 1 < xs.Count do
                        addSpanCoverage coverage w xs.[j] xs.[j + 1]
                        j <- j + 2
                for x = 0 to w - 1 do
                    if coverage.[x] > 0.0 then
                        mask.[row * w + x] <- byte (min 255.0 (255.0 * min 1.0 (coverage.[x] / float n)))
        | NonZero ->
            let crossings = ResizeArray<float * int>()
            for row = minY to maxY do
                Array.fill coverage 0 w 0.0
                for s = 0 to n - 1 do
                    crossings.Clear()
                    let scanY = float row + (float s + 0.5) / float n
                    for contour in contours do
                        let pts = contour
                        let len = List.length pts
                        if len >= 2 then
                            for i = 0 to len - 2 do
                                let (x0, y0) = pts.[i]
                                let (x1, y1) = pts.[i + 1]
                                if (y0 <= scanY && y1 > scanY) || (y1 <= scanY && y0 > scanY) then
                                    let t = (scanY - y0) / (y1 - y0)
                                    let xHit = x0 + t * (x1 - x0)
                                    let dir = if y0 < y1 then 1 else -1
                                    crossings.Add(xHit, dir)
                    crossings.Sort(fun (a, _) (b, _) -> compare a b)
                    let mutable winding = 0
                    let mutable j = 0
                    while j < crossings.Count do
                        let (xHit, dir) = crossings.[j]
                        let prevWinding = winding
                        winding <- winding + dir
                        if prevWinding = 0 && winding <> 0 then
                            let spanStart = xHit
                            j <- j + 1
                            while j < crossings.Count && winding <> 0 do
                                let (xh2, d2) = crossings.[j]
                                winding <- winding + d2
                                if winding = 0 then
                                    addSpanCoverage coverage w spanStart xh2
                                j <- j + 1
                        else
                            j <- j + 1
                for x = 0 to w - 1 do
                    if coverage.[x] > 0.0 then
                        mask.[row * w + x] <- byte (min 255.0 (255.0 * min 1.0 (coverage.[x] / float n)))
        mask

    // ── Glyph bitmap cache ──

    [<Struct>]
    type GlyphBitmapKey =
        { FontHash: int; GlyphId: int; Ppem: int
          SubX: int; SubY: int; AaSamples: int }

    type GlyphBitmap =
        { Alpha: byte[]; W: int; H: int
          BearingX: int; BearingY: int; ByteSize: int }

    let private fromTextBitmapGlyph (bmp: Text.BitmapGlyph) : GlyphBitmap =
        { Alpha = bmp.Alpha
          W = bmp.W
          H = bmp.H
          BearingX = bmp.BearingX
          BearingY = bmp.BearingY
          ByteSize = bmp.ByteSize }

    let private glyphBitmapCache = Dictionary<GlyphBitmapKey, GlyphBitmap>()
    let mutable private glyphCacheTotalBytes = 0
    let private glyphCacheBudget = 4 * 1024 * 1024 // 4 MB
    // Guards the cache dict + byte counter: rendering can happen on more than one
    // thread (event loop + test-control/screenshot), and a bare Dictionary
    // corrupts under concurrent read+insert.
    let private glyphCacheLock = obj ()

    let isPureTranslation (m: Matrix3x2) =
        m.M11 = 1.0 && m.M12 = 0.0 && m.M21 = 0.0 && m.M22 = 1.0

    let rasterizeGlyphBitmap (font: Text.FontData) (glyphId: int) (ppem: int) (subX: int) (subY: int) (aaSamples: int) : GlyphBitmap option =
        match Text.tryGlyphBitmap font glyphId ppem with
        | Some bmp ->
            Some (fromTextBitmapGlyph bmp)
        | None ->
            let outline = Text.glyphOutline font glyphId
            if outline.IsEmpty then None
            else
                let scale = float ppem / Text.unitsPerEm font
                let subPxX = float subX * 0.25
                let subPxY = float subY * 0.25
                // Scale and translate by sub-pixel offset
                let segs = outline |> List.map (Text.translatePathSeg subPxX subPxY scale)
                let contours = flattenPath segs
                // Compute tight bounding box
                let mutable minX = infinity
                let mutable minY = infinity
                let mutable maxX = -infinity
                let mutable maxY = -infinity
                for contour in contours do
                    for (x, y) in contour do
                        if x < minX then minX <- x
                        if y < minY then minY <- y
                        if x > maxX then maxX <- x
                        if y > maxY then maxY <- y
                if minX >= maxX || minY >= maxY then None
                else
                    // Add 1px padding
                    let bx = int (floor minX) - 1
                    let by = int (floor minY) - 1
                    let bw = int (ceil maxX) - bx + 2
                    let bh = int (ceil maxY) - by + 2
                    if bw <= 0 || bh <= 0 || bw > 512 || bh > 512 then None
                    else
                        // Offset contours to bitmap-local coords
                        let localContours =
                            contours |> List.map (List.map (fun (x, y) -> (x - float bx, y - float by)))
                        let alpha =
                            if aaSamples > 0 then
                                buildClipMaskAA bw bh localContours aaSamples NonZero
                            else
                                buildClipMask bw bh localContours NonZero
                        Some { Alpha = alpha; W = bw; H = bh; BearingX = bx; BearingY = by; ByteSize = alpha.Length }

    let getCachedGlyphBitmap (font: Text.FontData) (glyphId: int) (ppem: int) (subX: int) (subY: int) (aaSamples: int) : GlyphBitmap option =
        let fontHash = Text.fontId font
        let key = { FontHash = fontHash; GlyphId = glyphId; Ppem = ppem; SubX = subX; SubY = subY; AaSamples = aaSamples }
        let cached = lock glyphCacheLock (fun () -> match glyphBitmapCache.TryGetValue(key) with | true, bmp -> Some bmp | _ -> None)
        match cached with
        | Some bmp -> Some bmp
        | None ->
            // Rasterize outside the lock (the expensive part); a duplicate raster
            // on a race is harmless since the result is identical for a given key.
            match rasterizeGlyphBitmap font glyphId ppem subX subY aaSamples with
            | None -> None
            | Some bmp ->
                lock glyphCacheLock (fun () ->
                    if glyphCacheTotalBytes + bmp.ByteSize > glyphCacheBudget then
                        glyphBitmapCache.Clear()
                        glyphCacheTotalBytes <- 0
                    glyphBitmapCache.[key] <- bmp
                    glyphCacheTotalBytes <- glyphCacheTotalBytes + bmp.ByteSize)
                Some bmp

    let blitGlyphSolid (fb: Framebuffer) (bmp: GlyphBitmap) (dstX: int) (dstY: int) (color: Color) =
        for row = 0 to bmp.H - 1 do
            let py = dstY + bmp.BearingY + row
            if py >= 0 && py < fb.H then
                for col = 0 to bmp.W - 1 do
                    let a = bmp.Alpha.[row * bmp.W + col]
                    if a > 0uy then
                        let px = dstX + bmp.BearingX + col
                        if px >= 0 && px < fb.W then
                            let ca = int color.A * int a / 255
                            let c = { color with A = byte ca }
                            putPixel fb px py c

    let blitGlyphPaint (fb: Framebuffer) (bmp: GlyphBitmap) (dstX: int) (dstY: int) (paint: Paint) =
        for row = 0 to bmp.H - 1 do
            let py = dstY + bmp.BearingY + row
            if py >= 0 && py < fb.H then
                for col = 0 to bmp.W - 1 do
                    let a = bmp.Alpha.[row * bmp.W + col]
                    if a > 0uy then
                        let px = dstX + bmp.BearingX + col
                        if px >= 0 && px < fb.W then
                            let c = samplePaintAt paint px py
                            let ca = int c.A * int a / 255
                            let c' = { c with A = byte ca }
                            putPixel fb px py c'

    let blitGlyphTransformed (fb: Framebuffer) (bmp: GlyphBitmap) (baseX: float) (baseY: float) (m: Matrix3x2) (paint: Paint) =
        for row = 0 to bmp.H - 1 do
            for col = 0 to bmp.W - 1 do
                let a = bmp.Alpha.[row * bmp.W + col]
                if a > 0uy then
                    let sx = baseX + float bmp.BearingX + float col
                    let sy = baseY + float bmp.BearingY + float row
                    let p = Matrix3x2.transformVec2 m (vec2 sx sy)
                    let px = int (floor (float p.X + 0.000001))
                    let py = int (floor (float p.Y + 0.000001))
                    if px >= 0 && px < fb.W && py >= 0 && py < fb.H then
                        let c =
                            match paint with
                            | Solid color -> color
                            | _ -> samplePaintAt paint px py
                        let ca = int c.A * int a / 255
                        putPixel fb px py { c with A = byte ca }

    let clearGlyphBitmapCache () =
        lock glyphCacheLock (fun () ->
            glyphBitmapCache.Clear()
            glyphCacheTotalBytes <- 0)

    // ── Dash polyline ──

    let dashPolyline (pts: (float * float) list) (dashArray: float list) (dashOffset: float) =
        match dashArray with
        | [] -> [pts]
        | _ ->
            let dashes = Array.ofList dashArray
            let totalDash = Array.sum dashes
            if totalDash < 0.001 then [pts]
            else
                let result = ResizeArray<(float * float) list>()
                let cur = ResizeArray<float * float>()
                let mutable dashIdx = 0
                let mutable dashRemain = dashes.[0]
                // Apply dash offset
                let mutable off = dashOffset % totalDash
                if off < 0.0 then off <- off + totalDash
                while off > 0.0 do
                    if off >= dashRemain then
                        off <- off - dashRemain
                        dashIdx <- (dashIdx + 1) % dashes.Length
                        dashRemain <- dashes.[dashIdx]
                    else
                        dashRemain <- dashRemain - off
                        off <- 0.0
                let mutable isOn = (dashIdx % 2 = 0)
                let arr = Array.ofList pts
                if arr.Length < 2 then [pts]
                else
                    if isOn then cur.Add(arr.[0])
                    for i = 0 to arr.Length - 2 do
                        let (x0, y0) = arr.[i]
                        let (x1, y1) = arr.[i + 1]
                        let segDx = x1 - x0
                        let segDy = y1 - y0
                        let segLen = sqrt (segDx * segDx + segDy * segDy)
                        let mutable consumed = 0.0
                        while consumed < segLen - 0.0001 do
                            let remain = segLen - consumed
                            if dashRemain <= remain then
                                consumed <- consumed + dashRemain
                                let frac = consumed / segLen
                                let px = x0 + segDx * frac
                                let py = y0 + segDy * frac
                                if isOn then
                                    cur.Add(px, py)
                                    if cur.Count > 1 then result.Add(Seq.toList cur)
                                    cur.Clear()
                                else
                                    cur.Add(px, py)
                                dashIdx <- (dashIdx + 1) % dashes.Length
                                dashRemain <- dashes.[dashIdx]
                                isOn <- not isOn
                            else
                                dashRemain <- dashRemain - remain
                                consumed <- segLen
                                if isOn then cur.Add(x1, y1)
                    if cur.Count > 1 then result.Add(Seq.toList cur)
                    if result.Count = 0 then []
                    else Seq.toList result

    /// Split a stroke into visible dashes and preserve a genuine closed seam.
    let strokeDashes pts closed dashArray dashOffset =
        let parts = dashPolyline pts dashArray dashOffset
        if not closed || List.isEmpty parts then
            parts |> List.map (fun part -> part, false)
        else
            let seam = List.head pts
            let first = List.head parts
            let last = List.last parts
            if List.head first = seam && List.last last = seam then
                if parts.Length = 1 then [first, true]
                else
                    let middle = parts |> List.skip 1 |> List.take (parts.Length - 2)
                    let joined = last @ List.tail first
                    (joined, false) :: (middle |> List.map (fun part -> part, false))
            else parts |> List.map (fun part -> part, false)


    // ── Stroke expansion ──

    let expandStroke (pts: (float * float) list) (closed: bool) (style: StrokeStyle) : PathSeg list =
        let points = Array.ofList pts
        let closed = closed && points.Length > 3
        // Move the seam into the first edge so the original close point is
        // handled by the same join logic as every other vertex. Closed paths
        // have joins, not endpoint caps.
        let arr =
            if closed then
                let x0, y0 = points.[0]
                let x1, y1 = points.[1]
                let midpoint = (x0 + x1) * 0.5, (y0 + y1) * 0.5
                Array.init (points.Length + 1) (fun i ->
                    if i = 0 || i = points.Length then midpoint
                    else points.[i])
            else points
        let n = arr.Length
        let cap = if closed then Butt else style.Cap
        if n < 2 then []
        else
            let hw = float style.Width / 2.0
            let pi = System.Math.PI

            let normalize (dx, dy) =
                let len = sqrt (dx * dx + dy * dy)
                if len < 0.0001 then (0.0, 0.0) else (dx / len, dy / len)

            let perp (dx, dy) = (-dy, dx)

            // Compute per-segment direction and perpendicular
            let dirs = Array.init (n - 1) (fun i ->
                let (x0, y0) = arr.[i]
                let (x1, y1) = arr.[i + 1]
                normalize (x1 - x0, y1 - y0))

            let perps = dirs |> Array.map perp

            let left = ResizeArray<float * float>()
            let right = ResizeArray<float * float>()

            // First point
            let (px0, py0) = perps.[0]
            let (ax, ay) = arr.[0]
            left.Add(ax + px0 * hw, ay + py0 * hw)
            right.Add(ax - px0 * hw, ay - py0 * hw)

            // Interior joins
            for i = 1 to n - 2 do
                let (ix, iy) = arr.[i]
                let (p0x, p0y) = perps.[i - 1]
                let (p1x, p1y) = perps.[i]

                let lp0 = (ix + p0x * hw, iy + p0y * hw)
                let lp1 = (ix + p1x * hw, iy + p1y * hw)
                let rp0 = (ix - p0x * hw, iy - p0y * hw)
                let rp1 = (ix - p1x * hw, iy - p1y * hw)

                match style.Join with
                | Bevel ->
                    left.Add(fst lp0, snd lp0)
                    left.Add(fst lp1, snd lp1)
                    right.Add(fst rp0, snd rp0)
                    right.Add(fst rp1, snd rp1)
                | Miter ->
                    let (d0x, d0y) = dirs.[i - 1]
                    let (d1x, d1y) = dirs.[i]
                    let cross = d0x * d1y - d0y * d1x
                    if abs cross < 0.0001 then
                        left.Add(fst lp1, snd lp1)
                        right.Add(fst rp1, snd rp1)
                    else
                        // Line-line intersection for miter
                        let miterLimit = style.MiterLimit
                        let sinHalf = abs cross / 2.0
                        let miterLen = if sinHalf < 0.0001 then miterLimit + 1.0 else 1.0 / sinHalf
                        if miterLen > miterLimit then
                            // Fallback to bevel
                            left.Add(fst lp0, snd lp0)
                            left.Add(fst lp1, snd lp1)
                            right.Add(fst rp0, snd rp0)
                            right.Add(fst rp1, snd rp1)
                        else
                            // Compute miter point
                            let (lp0x, lp0y) = lp0
                            let t_num = (fst lp1 - lp0x) * d1y - (snd lp1 - lp0y) * d1x
                            let t_den = d0x * d1y - d0y * d1x
                            let t = t_num / t_den
                            let mx = lp0x + d0x * t
                            let my = lp0y + d0y * t
                            left.Add(mx, my)

                            let (rp0x, rp0y) = rp0
                            let tr_num = (fst rp1 - rp0x) * d1y - (snd rp1 - rp0y) * d1x
                            let tr = tr_num / t_den
                            let mrx = rp0x + d0x * tr
                            let mry = rp0y + d0y * tr
                            right.Add(mrx, mry)
                | RoundJoin ->
                    // Arc from prev perp to next perp
                    let angle0 = atan2 p0y p0x
                    let angle1 = atan2 p1y p1x
                    let mutable da = angle1 - angle0
                    if da > pi then da <- da - 2.0 * pi
                    elif da < -pi then da <- da + 2.0 * pi
                    let steps = max 2 (int (abs da / 0.3) + 1)
                    for s = 0 to steps do
                        let frac = float s / float steps
                        let a = angle0 + da * frac
                        left.Add(ix + cos a * hw, iy + sin a * hw)
                    let rAngle0 = atan2 (-p0y) (-p0x)
                    let rAngle1 = atan2 (-p1y) (-p1x)
                    let mutable rda = rAngle1 - rAngle0
                    if rda > pi then rda <- rda - 2.0 * pi
                    elif rda < -pi then rda <- rda + 2.0 * pi
                    for s = 0 to steps do
                        let frac = float s / float steps
                        let a = rAngle0 + rda * frac
                        right.Add(ix + cos a * hw, iy + sin a * hw)

            // Last point
            let (pxN, pyN) = perps.[n - 2]
            let (lx, ly) = arr.[n - 1]
            left.Add(lx + pxN * hw, ly + pyN * hw)
            right.Add(lx - pxN * hw, ly - pyN * hw)

            // Build outline: left forward + end cap + right reversed + start cap
            let toVec (x, y) = vec2 x y
            let leftArr = left.ToArray()
            let rightArr = right.ToArray()

            let result = ResizeArray<PathSeg>()
            // Left side forward
            result.Add(MoveTo (toVec leftArr.[0]))
            for i = 1 to leftArr.Length - 1 do
                result.Add(LineTo (toVec leftArr.[i]))

            // End cap
            let (ex, ey) = arr.[n - 1]
            let (edx, edy) = dirs.[n - 2]
            match cap with
            | Butt ->
                result.Add(LineTo (toVec rightArr.[rightArr.Length - 1]))
            | Square ->
                let capX = ex + edx * hw
                let capY = ey + edy * hw
                let (lpx, lpy) = perps.[n - 2]
                result.Add(LineTo (toVec (capX + lpx * hw, capY + lpy * hw)))
                result.Add(LineTo (toVec (capX - lpx * hw, capY - lpy * hw)))
                result.Add(LineTo (toVec rightArr.[rightArr.Length - 1]))
            | Round ->
                let startAngle = atan2 (snd perps.[n - 2]) (fst perps.[n - 2])
                let steps = max 4 (int (pi / 0.3) + 1)
                for s = 1 to steps do
                    let frac = float s / float steps
                    let a = startAngle - pi * frac
                    result.Add(LineTo (toVec (ex + cos a * hw, ey + sin a * hw)))

            // Right side reversed
            for i = rightArr.Length - 1 downto 0 do
                result.Add(LineTo (toVec rightArr.[i]))

            // Start cap
            let (sx, sy) = arr.[0]
            let (sdx, sdy) = dirs.[0]
            match cap with
            | Butt ->
                result.Add(Close)
            | Square ->
                let capX = sx - sdx * hw
                let capY = sy - sdy * hw
                let (spx, spy) = perps.[0]
                result.Add(LineTo (toVec (capX - spx * hw, capY - spy * hw)))
                result.Add(LineTo (toVec (capX + spx * hw, capY + spy * hw)))
                result.Add(Close)
            | Round ->
                let startAngle = atan2 (-(snd perps.[0])) (-(fst perps.[0]))
                let steps = max 4 (int (pi / 0.3) + 1)
                for s = 1 to steps - 1 do
                    let frac = float s / float steps
                    let a = startAngle - pi * frac
                    result.Add(LineTo (toVec (sx + cos a * hw, sy + sin a * hw)))
                result.Add(Close)

            Seq.toList result

    // ── Opacity helper ──

    let applyOpacity (c: uint32) (opacity: float) : uint32 =
        // In premultiplied space, scale all channels by opacity
        let f v = byte (max 0.0 (min 255.0 (float v * opacity)))
        Pixel.pack (f (Pixel.b c)) (f (Pixel.g c)) (f (Pixel.r c)) (f (Pixel.a c))

    // ── Filter functions ──

    let boxBlurHInto (src: uint32[]) (dst: uint32[]) (w: int) (h: int) (radius: int) =
        let d = radius * 2 + 1
        for y = 0 to h - 1 do
            let mutable rSum = 0
            let mutable gSum = 0
            let mutable bSum = 0
            let mutable aSum = 0
            let rowOff = y * w
            for kx = -radius to radius do
                let sx = max 0 (min (w - 1) kx)
                let c = src.[rowOff + sx]
                rSum <- rSum + int (Pixel.r c)
                gSum <- gSum + int (Pixel.g c)
                bSum <- bSum + int (Pixel.b c)
                aSum <- aSum + int (Pixel.a c)
            dst.[rowOff] <- Pixel.pack (byte (bSum / d)) (byte (gSum / d)) (byte (rSum / d)) (byte (aSum / d))
            for x = 1 to w - 1 do
                let addX = min (w - 1) (x + radius)
                let remX = max 0 (x - radius - 1)
                let ac = src.[rowOff + addX]
                let rc = src.[rowOff + remX]
                rSum <- rSum + int (Pixel.r ac) - int (Pixel.r rc)
                gSum <- gSum + int (Pixel.g ac) - int (Pixel.g rc)
                bSum <- bSum + int (Pixel.b ac) - int (Pixel.b rc)
                aSum <- aSum + int (Pixel.a ac) - int (Pixel.a rc)
                dst.[rowOff + x] <- Pixel.pack (byte (bSum / d)) (byte (gSum / d)) (byte (rSum / d)) (byte (aSum / d))

    let boxBlurVInto (src: uint32[]) (dst: uint32[]) (w: int) (h: int) (radius: int) =
        let d = radius * 2 + 1
        for x = 0 to w - 1 do
            let mutable rSum = 0
            let mutable gSum = 0
            let mutable bSum = 0
            let mutable aSum = 0
            for ky = -radius to radius do
                let sy = max 0 (min (h - 1) ky)
                let c = src.[sy * w + x]
                rSum <- rSum + int (Pixel.r c)
                gSum <- gSum + int (Pixel.g c)
                bSum <- bSum + int (Pixel.b c)
                aSum <- aSum + int (Pixel.a c)
            dst.[x] <- Pixel.pack (byte (bSum / d)) (byte (gSum / d)) (byte (rSum / d)) (byte (aSum / d))
            for y = 1 to h - 1 do
                let addY = min (h - 1) (y + radius)
                let remY = max 0 (y - radius - 1)
                let ac = src.[addY * w + x]
                let rc = src.[remY * w + x]
                rSum <- rSum + int (Pixel.r ac) - int (Pixel.r rc)
                gSum <- gSum + int (Pixel.g ac) - int (Pixel.g rc)
                bSum <- bSum + int (Pixel.b ac) - int (Pixel.b rc)
                aSum <- aSum + int (Pixel.a ac) - int (Pixel.a rc)
                dst.[y * w + x] <- Pixel.pack (byte (bSum / d)) (byte (gSum / d)) (byte (rSum / d)) (byte (aSum / d))

    let boxBlur (px: uint32[]) (w: int) (h: int) (radius: int) =
        if radius <= 0 then px
        else
            // Ping-pong between two buffers (2 allocs instead of 6)
            let a = Array.copy px
            let b = Array.zeroCreate px.Length
            boxBlurHInto a b w h radius   // a -> b
            boxBlurVInto b a w h radius   // b -> a
            boxBlurHInto a b w h radius   // a -> b
            boxBlurVInto b a w h radius   // b -> a
            boxBlurHInto a b w h radius   // a -> b
            boxBlurVInto b a w h radius   // b -> a
            a

    let applyBrightness (px: uint32[]) (factor: float) =
        let clamp v = byte (max 0 (min 255 (int (float v * factor))))
        px |> Array.map (fun c -> Pixel.pack (clamp (Pixel.b c)) (clamp (Pixel.g c)) (clamp (Pixel.r c)) (Pixel.a c))

    let applyContrast (px: uint32[]) (factor: float) =
        let adj v =
            let f = (float v / 255.0 - 0.5) * factor + 0.5
            byte (max 0 (min 255 (int (f * 255.0))))
        px |> Array.map (fun c -> Pixel.pack (adj (Pixel.b c)) (adj (Pixel.g c)) (adj (Pixel.r c)) (Pixel.a c))

    let applySaturate (px: uint32[]) (factor: float) =
        px |> Array.map (fun c ->
            let r = float (Pixel.r c) / 255.0
            let g = float (Pixel.g c) / 255.0
            let b = float (Pixel.b c) / 255.0
            let gray = 0.2126 * r + 0.7152 * g + 0.0722 * b
            let ch v = byte (max 0 (min 255 (int ((gray + (v - gray) * factor) * 255.0))))
            Pixel.pack (ch b) (ch g) (ch r) (Pixel.a c))

    let applyGrayscale (px: uint32[]) (factor: float) =
        px |> Array.map (fun c ->
            let a = float (Pixel.a c) / 255.0
            if a < 1e-10 then c
            else
                let r = float (Pixel.r c) / a / 255.0
                let g = float (Pixel.g c) / a / 255.0
                let b = float (Pixel.b c) / a / 255.0
                let gray = 0.2126 * r + 0.7152 * g + 0.0722 * b
                let mix v = gray + (v - gray) * (1.0 - factor)
                let ch v = byte (max 0 (min 255 (int (mix v * a * 255.0))))
                Pixel.pack (ch b) (ch g) (ch r) (Pixel.a c))

    let applySepia (px: uint32[]) (factor: float) =
        px |> Array.map (fun c ->
            let a = float (Pixel.a c) / 255.0
            if a < 1e-10 then c
            else
                let r = float (Pixel.r c) / a / 255.0
                let g = float (Pixel.g c) / a / 255.0
                let b = float (Pixel.b c) / a / 255.0
                let sr = min 1.0 (0.393 * r + 0.769 * g + 0.189 * b)
                let sg = min 1.0 (0.349 * r + 0.686 * g + 0.168 * b)
                let sb = min 1.0 (0.272 * r + 0.534 * g + 0.131 * b)
                let mr = r + (sr - r) * factor
                let mg = g + (sg - g) * factor
                let mb = b + (sb - b) * factor
                let ch v = byte (max 0 (min 255 (int (v * a * 255.0))))
                Pixel.pack (ch mb) (ch mg) (ch mr) (Pixel.a c))

    let applyInvert (px: uint32[]) (factor: float) =
        px |> Array.map (fun c ->
            let a = float (Pixel.a c) / 255.0
            if a < 1e-10 then c
            else
                let r = float (Pixel.r c) / a / 255.0
                let g = float (Pixel.g c) / a / 255.0
                let b = float (Pixel.b c) / a / 255.0
                let inv v = v * (1.0 - factor) + (1.0 - v) * factor
                let ch v = byte (max 0 (min 255 (int (inv v * a * 255.0))))
                Pixel.pack (ch b) (ch g) (ch r) (Pixel.a c))

    let applyHueRotate (px: uint32[]) (degrees: float) =
        // Rodrigues rotation matrix around (1,1,1)/sqrt(3)
        let rad = degrees * System.Math.PI / 180.0
        let cosA = cos rad
        let sinA = sin rad
        let oneMinusCos = 1.0 - cosA
        let k = 1.0 / 3.0 // (1/sqrt(3))^2
        let sq3 = 1.0 / sqrt 3.0
        // Rotation matrix: R = I*cos + (1-cos)*kkT + sin*K
        let r00 = cosA + oneMinusCos * k
        let r01 = oneMinusCos * k - sinA * sq3
        let r02 = oneMinusCos * k + sinA * sq3
        let r10 = oneMinusCos * k + sinA * sq3
        let r11 = cosA + oneMinusCos * k
        let r12 = oneMinusCos * k - sinA * sq3
        let r20 = oneMinusCos * k - sinA * sq3
        let r21 = oneMinusCos * k + sinA * sq3
        let r22 = cosA + oneMinusCos * k
        px |> Array.map (fun c ->
            let a = float (Pixel.a c) / 255.0
            if a < 1e-10 then c
            else
                let r = float (Pixel.r c) / a / 255.0
                let g = float (Pixel.g c) / a / 255.0
                let b = float (Pixel.b c) / a / 255.0
                let nr = max 0.0 (min 1.0 (r00 * r + r01 * g + r02 * b))
                let ng = max 0.0 (min 1.0 (r10 * r + r11 * g + r12 * b))
                let nb = max 0.0 (min 1.0 (r20 * r + r21 * g + r22 * b))
                let ch v = byte (max 0 (min 255 (int (v * a * 255.0))))
                Pixel.pack (ch nb) (ch ng) (ch nr) (Pixel.a c))

    let applyOpacityFilter (px: uint32[]) (factor: float) =
        px |> Array.map (fun c ->
            let ca = Pixel.a c
            let na = byte (max 0 (min 255 (int (float ca * factor))))
            let scale = if ca > 0uy then float na / float ca else 0.0
            Pixel.pack (byte (float (Pixel.b c) * scale)) (byte (float (Pixel.g c) * scale)) (byte (float (Pixel.r c) * scale)) na)

    let applyFilter (w: int) (h: int) (px: uint32[]) (f: Filter) =
        match f with
        | Blur radius -> boxBlur px w h radius
        | Brightness factor -> applyBrightness px factor
        | Contrast factor -> applyContrast px factor
        | Saturate factor -> applySaturate px factor
        | Grayscale factor -> applyGrayscale px factor
        | Sepia factor -> applySepia px factor
        | Invert factor -> applyInvert px factor
        | HueRotate degrees -> applyHueRotate px degrees
        | OpacityFilter factor -> applyOpacityFilter px factor
        | Custom pass -> pass w h px
        | DropShadow (dx, dy, blurRadius, shadowColor) ->
            // Build shadow from content alpha, offset, blur
            let shadow = Array.zeroCreate<uint32> (w * h)
            for y = 0 to h - 1 do
                for x = 0 to w - 1 do
                    let sx = x - int dx
                    let sy = y - int dy
                    if sx >= 0 && sx < w && sy >= 0 && sy < h then
                        let srcA = Pixel.a px.[sy * w + sx]
                        if srcA > 0uy then
                            let a = float srcA / 255.0 * float shadowColor.A / 255.0
                            shadow.[y * w + x] <- Pixel.fromColorPremul (Color.rgba shadowColor.R shadowColor.G shadowColor.B (byte (a * 255.0)))
            let blurred = boxBlur shadow w h blurRadius
            // Composite: shadow behind content
            let result = Array.copy blurred
            for i = 0 to w * h - 1 do
                if Pixel.a px.[i] > 0uy then
                    result.[i] <- blendPx px.[i] result.[i]
            result

    let applyFilters (w: int) (h: int) (filters: Filter list) (px: uint32[]) =
        filters |> List.fold (fun acc f -> applyFilter w h acc f) px

    let private tryAlphaBounds (fb: Framebuffer) =
        let px = fb.Px.Span
        let mutable minX = fb.W
        let mutable minY = fb.H
        let mutable maxX = -1
        let mutable maxY = -1
        for y = 0 to fb.H - 1 do
            let row = y * fb.W
            for x = 0 to fb.W - 1 do
                if Pixel.a px.[row + x] > 0uy then
                    if x < minX then minX <- x
                    if y < minY then minY <- y
                    if x > maxX then maxX <- x
                    if y > maxY then maxY <- y
        if maxX < minX || maxY < minY then None
        else Some (minX, minY, maxX, maxY)

    let private filterExpansion (filters: Filter list) =
        let mutable expansion = 0
        let mutable requiresFullFrame = false
        for filter in filters do
            match filter with
            | Blur radius ->
                expansion <- expansion + max 0 (radius * 3)
            | DropShadow (dx, dy, blurRadius, _) ->
                let offset = int (ceil (max (abs dx) (abs dy)))
                expansion <- expansion + offset + max 0 (blurRadius * 3)
            | Custom _ ->
                requiresFullFrame <- true
            | _ -> ()
        if requiresFullFrame then None else Some expansion

    let private compositeFilteredLayer (fb: Framebuffer) (filters: Filter list) (tmp: Framebuffer) =
        let fpx = fb.Px.Span
        match filterExpansion filters, tryAlphaBounds tmp with
        | Some pad, Some (minX, minY, maxX, maxY) ->
            let x0 = max 0 (minX - pad)
            let y0 = max 0 (minY - pad)
            let x1 = min (fb.W - 1) (maxX + pad)
            let y1 = min (fb.H - 1) (maxY + pad)
            let cw = x1 - x0 + 1
            let ch = y1 - y0 + 1
            if cw > 0 && ch > 0 then
                let crop = Array.zeroCreate<uint32> (cw * ch)
                let tpx = tmp.Px.Span
                for y = 0 to ch - 1 do
                    tpx.Slice((y0 + y) * tmp.W + x0, cw).CopyTo(System.Span(crop, y * cw, cw))
                let filtered = applyFilters cw ch filters crop
                for y = 0 to ch - 1 do
                    let srcRow = y * cw
                    let dstRow = (y0 + y) * fb.W + x0
                    for x = 0 to cw - 1 do
                        let sc = filtered.[srcRow + x]
                        if Pixel.a sc > 0uy then
                            fpx.[dstRow + x] <- blendPx sc fpx.[dstRow + x]
        | None, _
        | _, Some _ ->
            let filtered = applyFilters fb.W fb.H filters (tmp.Px.ToArray())
            for i = 0 to fb.Px.Length - 1 do
                let sc = filtered.[i]
                if Pixel.a sc > 0uy then
                    fpx.[i] <- blendPx sc fpx.[i]
        | Some _, None -> ()

    // ── Transform-aware dispatch ──

    let transformVec (m: Matrix3x2) (x: float) (y: float) =
        (m.M11 * x + m.M21 * y + m.DX, m.M12 * x + m.M22 * y + m.DY)

    let transformPathSeg (m: Matrix3x2) (seg: PathSeg) =
        let xf (v: Vec2) = Matrix3x2.transformVec2 m v
        match seg with
        | MoveTo p -> MoveTo (xf p)
        | LineTo p -> LineTo (xf p)
        | QuadTo (cp, pt) -> QuadTo (xf cp, xf pt)
        | CubicTo (c1, c2, pt) -> CubicTo (xf c1, xf c2, xf pt)
        | ArcTo (rx, ry, rot, la, sw, pt) ->
            let scale = sqrt (m.M11 * m.M11 + m.M12 * m.M12)
            // Flip sweep direction for reflective transforms (negative determinant)
            let det = m.M11 * m.M22 - m.M12 * m.M21
            let sw' = if det < 0.0 then not sw else sw
            // For circular arcs (rx ≈ ry), rotation is irrelevant; skip to avoid
            // floating-point artifacts from atan2 returning π for reflections
            let rot' =
                if abs (float rx - float ry) < 0.001 then rot
                else rot + atan2 m.M12 m.M11
            ArcTo (rx * scale, ry * scale, rot', la, sw', xf pt)
        | Close -> Close

    let translatePathSeg (dx: float) (dy: float) (seg: PathSeg) =
        let tf (v: Vec2) = vec2 (float v.X + dx) (float v.Y + dy)
        match seg with
        | MoveTo p -> MoveTo (tf p)
        | LineTo p -> LineTo (tf p)
        | QuadTo (cp, pt) -> QuadTo (tf cp, tf pt)
        | CubicTo (c1, c2, pt) -> CubicTo (tf c1, tf c2, tf pt)
        | ArcTo (rx, ry, rot, la, sw, pt) -> ArcTo (rx, ry, rot, la, sw, tf pt)
        | Close -> Close

    let pathBounds (segs: PathSeg list) =
        let mutable any = false
        let mutable minX = System.Double.PositiveInfinity
        let mutable minY = System.Double.PositiveInfinity
        let mutable maxX = System.Double.NegativeInfinity
        let mutable maxY = System.Double.NegativeInfinity
        let add (p: Vec2) =
            any <- true
            let x = float p.X
            let y = float p.Y
            if x < minX then minX <- x
            if y < minY then minY <- y
            if x > maxX then maxX <- x
            if y > maxY then maxY <- y
        for seg in segs do
            match seg with
            | MoveTo p
            | LineTo p -> add p
            | QuadTo (cp, pt) ->
                add cp
                add pt
            | CubicTo (c1, c2, pt) ->
                add c1
                add c2
                add pt
            | ArcTo (_, _, _, _, _, pt) -> add pt
            | Close -> ()
        if any then
            Some (int (floor minX), int (floor minY), int (ceil maxX), int (ceil maxY))
        else None

    let transformRect (m: Matrix3x2) (r: Rect) =
        let p0 = Matrix3x2.transformVec2 m r.Pos
        let p1 = Matrix3x2.transformVec2 m { X = r.Pos.X + r.Size.X; Y = r.Pos.Y }
        let p2 = Matrix3x2.transformVec2 m { X = r.Pos.X + r.Size.X; Y = r.Pos.Y + r.Size.Y }
        let p3 = Matrix3x2.transformVec2 m { X = r.Pos.X; Y = r.Pos.Y + r.Size.Y }
        (p0, p1, p2, p3)

    let transformPaint (m: Matrix3x2) (paint: Paint) =
        match paint with
        | NoPaint -> NoPaint
        | Solid _ -> paint
        | LinearGradient (p0, p1, stops, spread) ->
            LinearGradient (Matrix3x2.transformVec2 m p0, Matrix3x2.transformVec2 m p1, stops, spread)
        | RadialGradient (center, radius, stops, spread) ->
            let det = m.M11 * m.M22 - m.M12 * m.M21
            let scale = sqrt (abs det)
            RadialGradient (Matrix3x2.transformVec2 m center, radius * scale, stops, spread)
        | ConicGradient (center, angle, stops, spread) ->
            let rotAngle = atan2 m.M12 m.M11
            ConicGradient (Matrix3x2.transformVec2 m center, angle + rotAngle, stops, spread)
        | FocalRadialGradient (center, radius, focus, stops, spread) ->
            let det = m.M11 * m.M22 - m.M12 * m.M21
            let scale = sqrt (abs det)
            FocalRadialGradient (Matrix3x2.transformVec2 m center, radius * scale, Matrix3x2.transformVec2 m focus, stops, spread)
        | TilingPaint (src, xform, tileX, tileY) ->
            TilingPaint (src, Matrix3x2.multiply xform m, tileX, tileY)
        | ImagePaint (src, xform) ->
            ImagePaint (src, Matrix3x2.multiply xform m)

    /// Outline expansion after path coordinates have been transformed into device space.
    let strokeStyleInDeviceSpace (m: Matrix3x2) (style: StrokeStyle) =
        let scale = sqrt (abs (m.M11 * m.M22 - m.M12 * m.M21))
        { style with
            Paint = transformPaint m style.Paint
            Width = if style.Width = 0.0<px> then 1.0<px> else style.Width * scale
            DashArray = style.DashArray |> List.map (fun d -> d * scale)
            DashOffset = style.DashOffset * scale }

    /// Outline expansion before the GL transform: compensate only exact hairlines.
    let hairlineStyleInUserSpace (m: Matrix3x2) (style: StrokeStyle) =
        if style.Width <> 0.0<px> then style
        else
            let scale = sqrt (abs (m.M11 * m.M22 - m.M12 * m.M21))
            { style with Width = if scale > 0.0 then 1.0<px> / scale else 1.0<px> }

    let roundedRectPath (r: Rect) (cr: float<px>) : PathSeg list =
        let pi = System.Math.PI
        let halfW = float r.Size.X / 2.0
        let halfH = float r.Size.Y / 2.0
        let cr = float (min cr (min (halfW * 1.0<px>) (halfH * 1.0<px>)))
        let x0 = float r.Pos.X
        let y0 = float r.Pos.Y
        let x1 = x0 + float r.Size.X
        let y1 = y0 + float r.Size.Y
        [ MoveTo (vec2 (x0 + cr) y0)
          LineTo (vec2 (x1 - cr) y0)
          ArcTo (cr * 1.0<px>, cr * 1.0<px>, 0.0, false, true, vec2 x1 (y0 + cr))
          LineTo (vec2 x1 (y1 - cr))
          ArcTo (cr * 1.0<px>, cr * 1.0<px>, 0.0, false, true, vec2 (x1 - cr) y1)
          LineTo (vec2 (x0 + cr) y1)
          ArcTo (cr * 1.0<px>, cr * 1.0<px>, 0.0, false, true, vec2 x0 (y1 - cr))
          LineTo (vec2 x0 (y0 + cr))
          ArcTo (cr * 1.0<px>, cr * 1.0<px>, 0.0, false, true, vec2 (x0 + cr) y0)
          Close ]

    let roundedRectPath4 (r: Rect) (tl: float<px>) (tr: float<px>) (br: float<px>) (bl: float<px>) : PathSeg list =
        let w = float r.Size.X
        let h = float r.Size.Y
        // CSS spec: proportionally clamp when adjacent radii exceed edge length
        let scale = min 1.0 (min (w / (float tl + float tr)) (min (w / (float bl + float br)) (min (h / (float tl + float bl)) (h / (float tr + float br)))))
        let tl = float tl * scale |> max 0.0
        let tr = float tr * scale |> max 0.0
        let br = float br * scale |> max 0.0
        let bl = float bl * scale |> max 0.0
        let x0 = float r.Pos.X
        let y0 = float r.Pos.Y
        let x1 = x0 + w
        let y1 = y0 + h
        [ MoveTo (vec2 (x0 + tl) y0)
          LineTo (vec2 (x1 - tr) y0)
          ArcTo (tr * 1.0<px>, tr * 1.0<px>, 0.0, false, true, vec2 x1 (y0 + tr))
          LineTo (vec2 x1 (y1 - br))
          ArcTo (br * 1.0<px>, br * 1.0<px>, 0.0, false, true, vec2 (x1 - br) y1)
          LineTo (vec2 (x0 + bl) y1)
          ArcTo (bl * 1.0<px>, bl * 1.0<px>, 0.0, false, true, vec2 x0 (y1 - bl))
          LineTo (vec2 x0 (y0 + tl))
          ArcTo (tl * 1.0<px>, tl * 1.0<px>, 0.0, false, true, vec2 (x0 + tl) y0)
          Close ]

    let ellipsePath (cx: float) (cy: float) (rx: float) (ry: float) : PathSeg list =
        [ MoveTo (vec2 (cx + rx) cy)
          ArcTo (rx * 1.0<px>, ry * 1.0<px>, 0.0, false, true, vec2 cx (cy + ry))
          ArcTo (rx * 1.0<px>, ry * 1.0<px>, 0.0, false, true, vec2 (cx - rx) cy)
          ArcTo (rx * 1.0<px>, ry * 1.0<px>, 0.0, false, true, vec2 cx (cy - ry))
          ArcTo (rx * 1.0<px>, ry * 1.0<px>, 0.0, false, true, vec2 (cx + rx) cy)
          Close ]

    let nearestSample (src: Framebuffer) (u: float) (v: float) : uint32 =
        let x = int (round u) |> max 0 |> min (src.W - 1)
        let y = int (round v) |> max 0 |> min (src.H - 1)
        src.Px.Span.[y * src.W + x]

    let inline private clampIndex (maxIndex: int) (value: int) =
        if value < 0 then 0
        elif value > maxIndex then maxIndex
        else value

    let inline private lerpByte (a: byte) (b: byte) (t: float) =
        float a + (float b - float a) * t

    let inline private clampByte (value: float) =
        byte (max 0.0 (min 255.0 value))

    let inline private bilinearPx (c00: uint32) (c10: uint32) (c01: uint32) (c11: uint32) (fx: float) (fy: float) =
        let r0 = byte (lerpByte (Pixel.r c00) (Pixel.r c10) fx)
        let r1 = byte (lerpByte (Pixel.r c01) (Pixel.r c11) fx)
        let g0 = byte (lerpByte (Pixel.g c00) (Pixel.g c10) fx)
        let g1 = byte (lerpByte (Pixel.g c01) (Pixel.g c11) fx)
        let b0 = byte (lerpByte (Pixel.b c00) (Pixel.b c10) fx)
        let b1 = byte (lerpByte (Pixel.b c01) (Pixel.b c11) fx)
        let a0 = byte (lerpByte (Pixel.a c00) (Pixel.a c10) fx)
        let a1 = byte (lerpByte (Pixel.a c01) (Pixel.a c11) fx)
        Pixel.pack
            (clampByte (lerpByte b0 b1 fy))
            (clampByte (lerpByte g0 g1 fy))
            (clampByte (lerpByte r0 r1 fy))
            (clampByte (lerpByte a0 a1 fy))

    // ── Path Boolean Operations (Weiler-Atherton simplified) ──

    let segmentIntersect (a0x, a0y) (a1x, a1y) (b0x, b0y) (b1x, b1y) =
        let dax = a1x - a0x
        let day = a1y - a0y
        let dbx = b1x - b0x
        let dby = b1y - b0y
        let cross = dax * dby - day * dbx
        if abs cross < 1e-10 then None
        else
            let dx = b0x - a0x
            let dy = b0y - a0y
            let tA = (dx * dby - dy * dbx) / cross
            let tB = (dx * day - dy * dax) / cross
            if tA >= 0.0 && tA <= 1.0 && tB >= 0.0 && tB <= 1.0 then
                Some (a0x + tA * dax, a0y + tA * day, tA, tB)
            else None

    let pointInPolygon (px, py) (poly: (float * float)[]) =
        let mutable inside = false
        let n = poly.Length
        let mutable j = n - 1
        for i = 0 to n - 1 do
            let (xi, yi) = poly.[i]
            let (xj, yj) = poly.[j]
            if (yi > py) <> (yj > py) && px < (xj - xi) * (py - yi) / (yj - yi) + xi then
                inside <- not inside
            j <- i
        inside

    let pathBoolOp (op: BoolOp) (segsA: PathSeg list) (segsB: PathSeg list) : PathSeg list =
        let contA = flattenPath segsA
        let contB = flattenPath segsB
        let polyA = contA |> List.collect id |> Array.ofList
        let polyB = contB |> List.collect id |> Array.ofList
        if polyA.Length < 3 || polyB.Length < 3 then
            match op with
            | Union -> segsA @ segsB
            | Intersect -> []
            | BoolDifference -> segsA
            | BoolXor -> segsA @ segsB
        else
            // Scanline-based approach: build combined paths based on inside/outside test
            let toSegs (poly: (float * float)[]) =
                if poly.Length < 2 then []
                else
                    [ yield MoveTo (vec2 (fst poly.[0]) (snd poly.[0]))
                      for i = 1 to poly.Length - 1 do
                          yield LineTo (vec2 (fst poly.[i]) (snd poly.[i]))
                      yield Close ]
            let aInB = pointInPolygon polyA.[0] polyB
            let bInA = pointInPolygon polyB.[0] polyA
            match op with
            | Union ->
                // Both outlines, filling with NonZero will unite them
                (toSegs polyA) @ (toSegs polyB)
            | Intersect ->
                if aInB && bInA then toSegs polyA
                elif aInB then toSegs polyA
                elif bInA then toSegs polyB
                else
                    // Clip: collect segments of A inside B and B inside A
                    let result = ResizeArray<PathSeg>()
                    // Simplified: intersect by collecting crossings
                    let ptsA = polyA
                    let ptsB = polyB
                    let crossings = ResizeArray<float * float>()
                    for i = 0 to polyA.Length - 1 do
                        let a0 = polyA.[i]
                        let a1 = polyA.[(i + 1) % polyA.Length]
                        for j = 0 to polyB.Length - 1 do
                            let b0 = polyB.[j]
                            let b1 = polyB.[(j + 1) % polyB.Length]
                            match segmentIntersect a0 a1 b0 b1 with
                            | Some (x, y, _, _) -> crossings.Add(x, y)
                            | None -> ()
                    if crossings.Count >= 2 then
                        // Build intersection polygon from crossing points + inside points
                        let insidePts = ResizeArray<float * float>()
                        for pt in polyA do
                            if pointInPolygon pt polyB then insidePts.Add(pt)
                        for pt in polyB do
                            if pointInPolygon pt polyA then insidePts.Add(pt)
                        for pt in crossings do insidePts.Add(pt)
                        if insidePts.Count >= 3 then
                            // Sort by angle from centroid
                            let cx = insidePts |> Seq.averageBy fst
                            let cy = insidePts |> Seq.averageBy snd
                            let sorted = insidePts |> Seq.sortBy (fun (x, y) -> atan2 (y - cy) (x - cx)) |> Array.ofSeq
                            result.AddRange(toSegs sorted)
                    Seq.toList result
            | BoolDifference ->
                if bInA then
                    // B is fully inside A: A outline + reversed B outline (creates hole with EvenOdd)
                    let revB = Array.rev polyB
                    (toSegs polyA) @ (toSegs revB)
                elif aInB then
                    [] // A entirely inside B, difference is empty
                else
                    // Partial overlap
                    let result = ResizeArray<PathSeg>()
                    let insidePts = ResizeArray<float * float>()
                    for pt in polyA do
                        if not (pointInPolygon pt polyB) then insidePts.Add(pt)
                    let crossings = ResizeArray<float * float>()
                    for i = 0 to polyA.Length - 1 do
                        let a0 = polyA.[i]
                        let a1 = polyA.[(i + 1) % polyA.Length]
                        for j = 0 to polyB.Length - 1 do
                            let b0 = polyB.[j]
                            let b1 = polyB.[(j + 1) % polyB.Length]
                            match segmentIntersect a0 a1 b0 b1 with
                            | Some (x, y, _, _) -> crossings.Add(x, y)
                            | None -> ()
                    for pt in crossings do insidePts.Add(pt)
                    if insidePts.Count >= 3 then
                        let cx = insidePts |> Seq.averageBy fst
                        let cy = insidePts |> Seq.averageBy snd
                        let sorted = insidePts |> Seq.sortBy (fun (x, y) -> atan2 (y - cy) (x - cx)) |> Array.ofSeq
                        result.AddRange(toSegs sorted)
                    Seq.toList result
            | BoolXor ->
                // XOR: both outlines with EvenOdd fill
                (toSegs polyA) @ (toSegs polyB)

    let circleToPath (center: Vec2) (radius: float<px>) (nSegs: int) =
        [ let step = 2.0 * System.Math.PI / float nSegs
          yield MoveTo (vec2 (float center.X + float radius) (float center.Y))
          for i = 1 to nSegs - 1 do
              let a = float i * step
              yield LineTo (vec2 (float center.X + float radius * cos a) (float center.Y + float radius * sin a))
          yield Close ]

    /// Detect if transformed clip path is an axis-aligned rectangle.
    /// Returns (x, y, w, h) in pixel coordinates if so.
    let tryRectClip (segs: PathSeg list) : (int * int * int * int) option =
        match segs with
        | [MoveTo p0; LineTo p1; LineTo p2; LineTo p3; Close]
        | [MoveTo p0; LineTo p1; LineTo p2; LineTo p3] ->
            let minX = min (min p0.X p1.X) (min p2.X p3.X)
            let minY = min (min p0.Y p1.Y) (min p2.Y p3.Y)
            let maxX = max (max p0.X p1.X) (max p2.X p3.X)
            let maxY = max (max p0.Y p1.Y) (max p2.Y p3.Y)
            // Each point must be at a corner of the bounding rect
            let isCorner (p: Vec2) =
                (p.X = minX || p.X = maxX) && (p.Y = minY || p.Y = maxY)
            if isCorner p0 && isCorner p1 && isCorner p2 && isCorner p3 then
                let x0 = int (floor (float minX))
                let y0 = int (floor (float minY))
                let x1 = int (ceil (float maxX))
                let y1 = int (ceil (float maxY))
                if x1 > x0 && y1 > y0 then Some (x0, y0, x1 - x0, y1 - y0)
                else None
            else None
        | _ -> None

    /// A unit-opacity transform or clip is a graphics-state wrapper, not an
    /// object boundary. A non-unit-opacity Group is an indivisible object.
    let rec transparencyObjects (cmd: Cmd) : Cmd list =
        match cmd with
        | Group (xform, opacity, children) when opacity >= 1.0 ->
            children
            |> List.collect (fun child ->
                transparencyObjects child |> List.map (fun obj -> Group (xform, 1.0, [obj])))
        | Clip (path, rule, children) ->
            children
            |> List.collect (fun child ->
                transparencyObjects child |> List.map (fun obj -> Clip (path, rule, [obj])))
        | SaveRestore children ->
            children |> List.collect (fun child ->
                transparencyObjects child |> List.map (fun obj -> Group (Matrix3x2.identity, 1.0, [obj])))
        | AntiAlias (samples, children) ->
            children |> List.collect (fun child ->
                transparencyObjects child |> List.map (fun obj -> AntiAlias (samples, [obj])))
        | _ -> [cmd]
    /// A blend mode is applied against the group's backdrop, not while
    /// rendering the object onto its initially transparent source surface.
    let rec transparencyObjectBlend cmd =
        match cmd with
        | WithShape (shape, [child]) ->
            let source, mode = transparencyObjectBlend child
            WithShape (shape, [source]), mode
        | Blend (mode, children) -> SaveRestore children, mode
        | Clip (path, rule, [child]) ->
            let source, mode = transparencyObjectBlend child
            Clip (path, rule, [source]), mode
        | Group (matrix, opacity, [child]) ->
            let source, mode = transparencyObjectBlend child
            Group (matrix, opacity, [source]), mode
        | AntiAlias (samples, [child]) ->
            let source, mode = transparencyObjectBlend child
            AntiAlias (samples, [source]), mode
        | Mask (mask, [child]) ->
            let source, mode = transparencyObjectBlend child
            Mask (mask, [source]), mode
        | _ -> cmd, Normal

    /// Both nonisolated interiors and a non-Normal group boundary need the
    /// object's actual backdrop, through graphics-state-only wrappers.
    let rec transparencyNeedsBackdrop = function
        | TransparencyGroup (isolated, _, _, mode, _, _) -> not isolated || mode <> Normal
        | ColorManagedGroup (_, isolated, _, _, mode, _, _) -> not isolated || mode <> Normal
        | ColorManagedGroup4 (_, isolated, _, _, mode, _, _) -> not isolated || mode <> Normal
        | WithShape (_, [child]) -> transparencyNeedsBackdrop child
        | WithShape (_, children) -> not children.IsEmpty
        | Group (_, opacity, [child]) when opacity >= 1.0 -> transparencyNeedsBackdrop child
        | Clip (_, _, [child])
        | SaveRestore [child]
        | AntiAlias (_, [child]) -> transparencyNeedsBackdrop child
        | _ -> false

    // Apply AA at the object dispatch rather than rendering its wrapper to an
    // empty temporary surface, so a nested group retains its backdrop.
    let rec transparencyObjectSampling samples = function
        | AntiAlias (innerSamples, [child]) -> transparencyObjectSampling innerSamples child
        | Group (matrix, opacity, [child]) when opacity >= 1.0 ->
            let innerSamples, inner = transparencyObjectSampling samples child
            innerSamples, Group (matrix, opacity, [inner])
        | Clip (path, rule, [child]) ->
            let innerSamples, inner = transparencyObjectSampling samples child
            innerSamples, Clip (path, rule, [inner])
        | child -> samples, child

    /// The candidate is the object already rendered against its proper base.
    let accumulateTransparencyCandidate (group: Framebuffer) (alpha: Framebuffer)
                                        (backdrop: Framebuffer) (candidate: Framebuffer)
                                        (source: Framebuffer) (shape: Framebuffer option)
                                        (isolated: bool) =
        let gp = group.Px.Span
        let ap = alpha.Px.Span
        let bp = backdrop.Px.Span
        let cp = candidate.Px.Span
        let sp = source.Px.Span
        for i = 0 to group.Px.Length - 1 do
            let sa = int (Pixel.a sp.[i])
            let oldA = int (Pixel.a ap.[i])
            match shape with
            | None ->
                gp.[i] <- cp.[i]
                ap.[i] <- uint32 (sa + (oldA * (255 - sa) + 127) / 255) <<< 24
            | Some shapeFb ->
                let inv = 255 - int (Pixel.a shapeFb.Px.Span.[i])
                let old = gp.[i]
                let basePx = if isolated then 0u else bp.[i]
                let result = cp.[i]
                let channel get =
                    byte (max 0 (min 255 (int (get result) + (int (get old) - int (get basePx)) * inv / 255)))
                gp.[i] <- Pixel.pack (channel Pixel.b) (channel Pixel.g) (channel Pixel.r) (channel Pixel.a)
                ap.[i] <- uint32 (min 255 (sa + (oldA * inv + 127) / 255)) <<< 24



    /// Accumulate one already-rendered object. Alpha is tracked separately from
    /// color because a nonisolated group contains backdrop color even where its
    /// own contribution has zero alpha.
    let accumulateTransparencyObject (group: Framebuffer) (alpha: Framebuffer)
                                     (backdrop: Framebuffer) (source: Framebuffer)
                                     (shape: Framebuffer option) (isolated: bool) (mode: BlendMode) =
        let gp = group.Px.Span
        let ap = alpha.Px.Span
        let bp = backdrop.Px.Span
        let sp = source.Px.Span
        for i = 0 to group.Px.Length - 1 do
            let src = sp.[i]
            let sa = int (Pixel.a src)
            match shape with
            | None ->
                gp.[i] <- blendWithMode mode src gp.[i]
                let oldA = int (Pixel.a ap.[i])
                let newA = sa + (oldA * (255 - sa) + 127) / 255
                ap.[i] <- uint32 newA <<< 24
            | Some shapeFb ->
                let sh = int (Pixel.a shapeFb.Px.Span.[i])
                let inv = 255 - sh
                let old = gp.[i]
                let basePx = if isolated then 0u else bp.[i]
                let blended = blendWithMode mode src basePx
                let channel get =
                    max 0 (min 255 (int (get blended) + ((int (get old) - int (get basePx)) * inv) / 255)) |> byte
                gp.[i] <- Pixel.pack (channel Pixel.b) (channel Pixel.g) (channel Pixel.r) (channel Pixel.a)
                let oldA = int (Pixel.a ap.[i])
                ap.[i] <- uint32 (min 255 (sa + (oldA * inv + 127) / 255)) <<< 24

    let inline private convertedChannel alpha x =
        byte (max 0.0 (min 1.0 x) * alpha + 0.5)

    let inline private extractTransparencyChannel current backdrop inv =
        byte (max 0 (min 255 (int current - (int backdrop * inv + 127) / 255)))

    /// Framebuffer pixels are premultiplied; conversions operate on straight
    /// normalized components. Preserve the original alpha through conversion.
    let convertTransparencyPixel convert pixel =
        let a = Pixel.a pixel
        if a = 0uy then 0u
        else
            let scale = 1.0 / float a
            let struct (r, g, b) =
                convert (min 1.0 (float (Pixel.r pixel) * scale))
                        (min 1.0 (float (Pixel.g pixel) * scale))
                        (min 1.0 (float (Pixel.b pixel) * scale))
            let premultipliedAlpha = float a
            Pixel.pack (convertedChannel premultipliedAlpha b)
                       (convertedChannel premultipliedAlpha g)
                       (convertedChannel premultipliedAlpha r) a

    /// Extract the group's contribution from its original backdrop before
    /// applying group-wide opacity, soft mask and blend mode exactly once.
    let finishTransparencyGroup (destination: Framebuffer) (group: Framebuffer)
                                (alpha: Framebuffer) (mask: Framebuffer option)
                                (isolated: bool) (opacity: float) (mode: BlendMode)
                                (conversion: ColorSpaceConversion option) (groupBackdrop: Framebuffer) =
        let dp = destination.Px.Span
        let gp = group.Px.Span
        let ap = alpha.Px.Span
        let bp = groupBackdrop.Px.Span
        for i = 0 to destination.Px.Length - 1 do
            let a = int (Pixel.a ap.[i])
            if a > 0 then
                let current = gp.[i]
                let backdrop = dp.[i]
                let source =
                    if isolated then current
                    else
                        let inv = 255 - a
                        let original = bp.[i]
                        Pixel.pack
                            (extractTransparencyChannel (Pixel.b current) (Pixel.b original) inv)
                            (extractTransparencyChannel (Pixel.g current) (Pixel.g original) inv)
                            (extractTransparencyChannel (Pixel.r current) (Pixel.r original) inv)
                            (byte a)
                let source =
                    match conversion with
                    | Some value -> convertTransparencyPixel value.ToParent source
                    | None -> source
                let maskAlpha =
                    match mask with
                    | Some maskFb -> float (Pixel.a maskFb.Px.Span.[i]) / 255.0
                    | None -> 1.0
                let sc = applyOpacity source (opacity * maskAlpha)
                if Pixel.a sc > 0uy then dp.[i] <- blendWithMode mode sc backdrop

    let rec dispatchWithTransform (fb: Framebuffer) (m: Matrix3x2) (cmd: Cmd) =
        let isIdentity (m: Matrix3x2) =
            m.M11 = 1.0 && m.M12 = 0.0 && m.M21 = 0.0 && m.M22 = 1.0 && m.DX = 0.0 && m.DY = 0.0
        match cmd with
        | WithShape (_, cmds) -> cmds |> List.iter (dispatchWithTransform fb m)
        | Clear c -> clearFb fb c
        | Line (a, b, c, w) ->
            let a' = Matrix3x2.transformVec2 m a
            let b' = Matrix3x2.transformVec2 m b
            bresenham fb a' b' c w
        | FillRect (r, c) ->
            if isIdentity m then fillRect fb r c
            else
                let (p0, p1, p2, p3) = transformRect m r
                fillTri fb p0 p1 p2 c
                fillTri fb p0 p2 p3 c
        | StrokeRect (r, c, w) ->
            if isIdentity m then strokeRect fb r c w
            else
                let (p0, p1, p2, p3) = transformRect m r
                bresenham fb p0 p1 c w
                bresenham fb p1 p2 c w
                bresenham fb p2 p3 c w
                bresenham fb p3 p0 c w
        | FillCircle (center, radius, c) ->
            if isIdentity m then fillCircle fb center radius c
            else
                let path = circleToPath center radius 32 |> List.map (transformPathSeg m)
                let contours = flattenPath path
                scanlineFill fb contours c EvenOdd
        | FillTri (a, b, c, col) ->
            fillTri fb (Matrix3x2.transformVec2 m a) (Matrix3x2.transformVec2 m b) (Matrix3x2.transformVec2 m c) col
        | FillPath (segs, paint, fillRule) ->
            let paint' = transformPaint m paint
            let segs' = segs |> List.map (transformPathSeg m)
            match tryRectClip segs' with
            | Some (x, y, w, h) -> fillPaintRect fb x y w h paint'
            | None ->
                let contours = flattenPath segs'
                scanlineFillPaint fb contours paint' fillRule
        | StrokePath (segs, style) ->
            let style' = strokeStyleInDeviceSpace m style
            let segs' = segs |> List.map (transformPathSeg m)
            let contours = flattenStrokePath segs'
            for contour, closed in contours do
                let dashed = strokeDashes contour closed (style'.DashArray |> List.map float) (float style'.DashOffset)
                for sub, subClosed in dashed do
                    let outline = expandStroke sub subClosed style'
                    let outlineContours = flattenPath outline
                    scanlineFillPaint fb outlineContours style'.Paint EvenOdd
        | FillRoundedRect (r, cr, c) ->
            let isPureTranslate = m.M12 = 0.0 && m.M21 = 0.0 && m.M11 = 1.0 && m.M22 = 1.0
            if isPureTranslate then
                fillRoundedRectFast fb (float r.Pos.X + m.DX) (float r.Pos.Y + m.DY) (float r.Size.X) (float r.Size.Y) (float cr) c
            else
                let path = roundedRectPath r cr |> List.map (transformPathSeg m)
                let contours = flattenPath path
                scanlineFill fb contours c EvenOdd
        | StrokeRoundedRect (r, cr, style) ->
            let style' = strokeStyleInDeviceSpace m style
            let path = roundedRectPath r cr |> List.map (transformPathSeg m)
            let contours = flattenStrokePath path
            for contour, closed in contours do
                let outline = expandStroke contour closed style'
                let outlineContours = flattenPath outline
                scanlineFillPaint fb outlineContours style'.Paint EvenOdd
        | FillRoundedRect4 (r, tl, tr, br, bl, c) ->
            let isPureTranslate = m.M12 = 0.0 && m.M21 = 0.0 && m.M11 = 1.0 && m.M22 = 1.0
            if isPureTranslate then
                fillRoundedRect4Fast fb (float r.Pos.X + m.DX) (float r.Pos.Y + m.DY) (float r.Size.X) (float r.Size.Y) (float tl) (float tr) (float br) (float bl) c
            else
                let path = roundedRectPath4 r tl tr br bl |> List.map (transformPathSeg m)
                let contours = flattenPath path
                scanlineFill fb contours c EvenOdd
        | StrokeRoundedRect4 (r, tl, tr, br, bl, style) ->
            let style' = strokeStyleInDeviceSpace m style
            let path = roundedRectPath4 r tl tr br bl |> List.map (transformPathSeg m)
            let contours = flattenStrokePath path
            for contour, closed in contours do
                let outline = expandStroke contour closed style'
                let outlineContours = flattenPath outline
                scanlineFillPaint fb outlineContours style'.Paint EvenOdd
        | DrawImageReduced (src, srcRect, dstRect, smooth) ->
            let filtered, filteredRect = ImageReduction.prepare src srcRect dstRect m
            dispatchWithTransform fb m (DrawImage (filtered, filteredRect, dstRect, smooth))
        | DrawImage (src, srcRect, dstRect, smooth) ->
            let determinant = m.M11 * m.M22 - m.M12 * m.M21
            let dx, dy = float dstRect.Pos.X, float dstRect.Pos.Y
            let dw, dh = float dstRect.Size.X, float dstRect.Size.Y
            if abs determinant > 1e-12 && dw > 0.0 && dh > 0.0 && src.W > 0 && src.H > 0 then
                let p0 = Matrix3x2.transformVec2 m (vec2 dx dy)
                let p1 = Matrix3x2.transformVec2 m (vec2 (dx + dw) dy)
                let p2 = Matrix3x2.transformVec2 m (vec2 dx (dy + dh))
                let p3 = Matrix3x2.transformVec2 m (vec2 (dx + dw) (dy + dh))
                let x0 = max 0 (int (floor (float (min (min p0.X p1.X) (min p2.X p3.X)))))
                let y0 = max 0 (int (floor (float (min (min p0.Y p1.Y) (min p2.Y p3.Y)))))
                let x1 = min fb.W (int (ceil (float (max (max p0.X p1.X) (max p2.X p3.X)))))
                let y1 = min fb.H (int (ceil (float (max (max p0.Y p1.Y) (max p2.Y p3.Y)))))
                let inv = Matrix3x2.inverse m
                let sx, sy = float srcRect.Pos.X, float srcRect.Pos.Y
                let sw, sh = float srcRect.Size.X, float srcRect.Size.Y
                let spx = src.Px.Span
                let dpx = fb.Px.Span
                for y = y0 to y1 - 1 do
                    for x = x0 to x1 - 1 do
                        let p = Matrix3x2.transformVec2 inv (vec2 (float x + 0.5) (float y + 0.5))
                        let u, v = (float p.X - dx) / dw, (float p.Y - dy) / dh
                        if u >= 0.0 && u < 1.0 && v >= 0.0 && v < 1.0 then
                            let sampleX, sampleY = sx + u * sw, sy + v * sh
                            let pixel =
                                if smooth then sampleBilinear src (sampleX - 0.5) (sampleY - 0.5)
                                else
                                    let ix = clampIndex (src.W - 1) (int (floor sampleX))
                                    let iy = clampIndex (src.H - 1) (int (floor sampleY))
                                    spx.[iy * src.W + ix]
                            let index = y * fb.W + x
                            dpx.[index] <- blendPx pixel dpx.[index]
        | BoxShadow (r, offsetX, offsetY, blurRadius, spread, color, inset, cornerRadius) ->
            let spreadF = float spread
            let shadowRect =
                if inset then
                    rect (float r.Pos.X + spreadF) (float r.Pos.Y + spreadF) (float r.Size.X - 2.0 * spreadF) (float r.Size.Y - 2.0 * spreadF)
                else
                    rect (float r.Pos.X - spreadF) (float r.Pos.Y - spreadF) (float r.Size.X + 2.0 * spreadF) (float r.Size.Y + 2.0 * spreadF)
            let shadowCr =
                match cornerRadius with
                | Some cr ->
                    let adj = if inset then float cr - spreadF else float cr + spreadF
                    max 0.0 adj
                | None -> 0.0
            let shadowPath =
                if shadowCr > 0.0 then roundedRectPath shadowRect (shadowCr * 1.0<px>) |> List.map (transformPathSeg m)
                else
                    let (p0, p1, p2, p3) = transformRect m shadowRect
                    [ MoveTo p0; LineTo p1; LineTo p2; LineTo p3; Close ]
            let origPath =
                match cornerRadius with
                | Some cr -> roundedRectPath r cr |> List.map (transformPathSeg m)
                | None ->
                    let (p0, p1, p2, p3) = transformRect m r
                    [ MoveTo p0; LineTo p1; LineTo p2; LineTo p3; Close ]
            let shiftedShadowPath = shadowPath |> List.map (translatePathSeg offsetX offsetY)
            match pathBounds shiftedShadowPath with
            | None -> ()
            | Some (sx0, sy0, sx1, sy1) ->
                let pad = max 0 (blurRadius * 3)
                let cropX0 = max 0 (sx0 - pad)
                let cropY0 = max 0 (sy0 - pad)
                let cropX1 = min fb.W (sx1 + pad)
                let cropY1 = min fb.H (sy1 + pad)
                let cropW = cropX1 - cropX0
                let cropH = cropY1 - cropY0
                if cropW > 0 && cropH > 0 then
                    let tmp = getTempFb cropW cropH
                    let shadowContours =
                        shiftedShadowPath
                        |> List.map (translatePathSeg (float -cropX0) (float -cropY0))
                        |> flattenPath
                    scanlineFill tmp shadowContours color EvenOdd
                    let blurred = boxBlur (tmp.Px.ToArray()) cropW cropH blurRadius
                    releaseTempFb tmp
                    let origContours =
                        origPath
                        |> List.map (translatePathSeg (float -cropX0) (float -cropY0))
                        |> flattenPath
                    let mask = buildClipMask cropW cropH origContours EvenOdd
                    let fpx = fb.Px.Span
                    for y = 0 to cropH - 1 do
                        let srcRow = y * cropW
                        let dstRow = (cropY0 + y) * fb.W + cropX0
                        for x = 0 to cropW - 1 do
                            let sc = blurred.[srcRow + x]
                            if Pixel.a sc > 0uy then
                                let inside = mask.[srcRow + x] > 0uy
                                let draw = if inset then inside else not inside
                                if draw then
                                    fpx.[dstRow + x] <- blendPx sc fpx.[dstRow + x]
        | DrawBorder (r, top, right, bottom, left, tl, tr, br, bl) ->
            let x0 = float r.Pos.X
            let y0 = float r.Pos.Y
            let w = float r.Size.X
            let h = float r.Size.Y
            let x1 = x0 + w
            let y1 = y0 + h
            let tw = float top.Width
            let rw = float right.Width
            let bw = float bottom.Width
            let lw = float left.Width
            // CSS proportional clamping for corner radii
            let ftl = float tl
            let ftr = float tr
            let fbr = float br
            let fbl = float bl
            let sf = min 1.0 (min (w / (max 1e-10 (ftl + ftr))) (min (w / (max 1e-10 (fbl + fbr))) (min (h / (max 1e-10 (ftl + fbl))) (h / (max 1e-10 (ftr + fbr))))))
            let tl' = ftl * sf
            let tr' = ftr * sf
            let br' = fbr * sf
            let bl' = fbl * sf
            // Build outer rounded rect path
            let outerPath = roundedRectPath4 r (tl' * 1.0<px>) (tr' * 1.0<px>) (br' * 1.0<px>) (bl' * 1.0<px>) |> List.map (transformPathSeg m)
            let outerContours = flattenPath outerPath
            let outerMask = buildClipMask fb.W fb.H outerContours EvenOdd
            // Build inner rounded rect (shrunk by border widths)
            let innerTl = max 0.0 (tl' - max tw lw)
            let innerTr = max 0.0 (tr' - max tw rw)
            let innerBr = max 0.0 (br' - max bw rw)
            let innerBl = max 0.0 (bl' - max bw lw)
            let innerRect = rect (x0 + lw) (y0 + tw) (w - lw - rw) (h - tw - bw)
            let innerPath = roundedRectPath4 innerRect (innerTl * 1.0<px>) (innerTr * 1.0<px>) (innerBr * 1.0<px>) (innerBl * 1.0<px>) |> List.map (transformPathSeg m)
            let innerContours = flattenPath innerPath
            let innerMask = buildClipMask fb.W fb.H innerContours EvenOdd
            // Inner corner positions (where diagonals from outer corner to inner corner split sides)
            let ix0 = x0 + lw
            let iy0 = y0 + tw
            let ix1 = x1 - rw
            let iy1 = y1 - bw
            // For each pixel in the border region, determine which side owns it
            for py = max 0 (int y0) to min (fb.H - 1) (int (ceil y1) - 1) do
                for px = max 0 (int x0) to min (fb.W - 1) (int (ceil x1) - 1) do
                    let i = py * fb.W + px
                    if outerMask.[i] > 0uy && innerMask.[i] = 0uy then
                        let fpx = float px + 0.5
                        let fpy = float py + 0.5
                        // Determine side using diagonal cross-product tests at corners
                        // TL corner: diagonal from (x0,y0) to (ix0,iy0)
                        let crossTL = lw * (fpy - y0) - tw * (fpx - x0)
                        // TR corner: diagonal from (x1,y0) to (ix1,iy0)
                        let crossTR = tw * (fpx - x1) + rw * (fpy - y0)
                        // BR corner: diagonal from (x1,y1) to (ix1,iy1)
                        let crossBR = rw * (fpy - y1) - bw * (fpx - x1)
                        // BL corner: diagonal from (x0,y1) to (ix0,iy1)
                        let crossBL = bw * (fpx - x0) + lw * (fpy - y1)
                        let side =
                            if crossTL <= 0.0 && crossTR >= 0.0 then top
                            elif crossTR <= 0.0 && crossBR >= 0.0 then right
                            elif crossBR <= 0.0 && crossBL >= 0.0 then bottom
                            elif crossBL <= 0.0 && crossTL >= 0.0 then left
                            // Fallback: closest edge
                            else
                                let dTop = fpy - y0
                                let dBot = y1 - fpy
                                let dLft = fpx - x0
                                let dRgt = x1 - fpx
                                let minD = min dTop (min dBot (min dLft dRgt))
                                if minD = dTop then top
                                elif minD = dRgt then right
                                elif minD = dBot then bottom
                                else left
                        if side.Style <> NoBorder && side.Width > 0.0<px> then
                            let drawPixel =
                                match side.Style with
                                | BorderSolid -> true
                                | BorderDashed ->
                                    // Periodic dash along edge
                                    let pos =
                                        if side = top || side = bottom then fpx - x0
                                        else fpy - y0
                                    let dashLen = float side.Width * 3.0
                                    let phase = pos % (dashLen * 2.0)
                                    phase < dashLen
                                | BorderDotted ->
                                    let pos =
                                        if side = top || side = bottom then fpx - x0
                                        else fpy - y0
                                    let dotSpacing = float side.Width
                                    let phase = pos % (dotSpacing * 2.0)
                                    phase < dotSpacing
                                | BorderDouble ->
                                    // Two lines with gap: outer 1/3, gap 1/3, inner 1/3
                                    let depth =
                                        if side = top then fpy - y0
                                        elif side = bottom then y1 - fpy
                                        elif side = left then fpx - x0
                                        else x1 - fpx
                                    let sw = float side.Width
                                    let third = sw / 3.0
                                    depth < third || depth > (sw - third)
                                | NoBorder -> false
                            if drawPixel then
                                let fpx = fb.Px.Span
                                fpx.[i] <- blendPx (Pixel.fromColorPremul side.Color) fpx.[i]
        | BackdropFilter (clipPath, fillRule, filters, cmds) ->
            let clipPath' = clipPath |> List.map (transformPathSeg m)
            let contours = flattenPath clipPath'
            let mask = buildClipMask fb.W fb.H contours fillRule
            // Apply filters to a copy of current FB within the clipped region
            let snapshot : uint32[] = fb.Px.ToArray()
            let filtered = applyFilters fb.W fb.H filters snapshot
            let fpx = fb.Px.Span
            for i = 0 to fb.Px.Length - 1 do
                if mask.[i] > 0uy then
                    fpx.[i] <- filtered.[i]
            // Draw children on top
            cmds |> List.iter (dispatchWithTransform fb m)
        | Group3D (xform, opacity, cmds) ->
            // Render children to temp buffer, then inverse-project each parent pixel
            let tmp = getTempFb fb.W fb.H
            cmds |> List.iter (dispatchWithTransform tmp m)
            // Combine the current 2D transform with the 4x4
            let m4 = Matrix4x4.multiply (Matrix4x4.fromMatrix3x2 m) xform
            // For each pixel in parent, find source UV by inverse-projecting
            // We need to invert the 2D→2D mapping implied by the 4x4 (z=0 input plane)
            // The mapping is: x' = (M11*x+M21*y+M41)/(M14*x+M24*y+M44), y' = (M12*x+M22*y+M42)/(M14*x+M24*y+M44)
            // Given x',y' find x,y by solving the 2x2 rational system via Cramer's rule
            // Rearranging: x'*(M14*x+M24*y+M44) = M11*x+M21*y+M41
            //   => x*(M11 - x'*M14) + y*(M21 - x'*M24) = x'*M44 - M41
            //   => x*(M12 - y'*M14) + y*(M22 - y'*M24) = y'*M44 - M42
            for py = 0 to fb.H - 1 do
                for px = 0 to fb.W - 1 do
                    let x' = float px + 0.5
                    let y' = float py + 0.5
                    let a11 = m4.M11 - x' * m4.M14
                    let a12 = m4.M21 - x' * m4.M24
                    let b1  = x' * m4.M44 - m4.M41
                    let a21 = m4.M12 - y' * m4.M14
                    let a22 = m4.M22 - y' * m4.M24
                    let b2  = y' * m4.M44 - m4.M42
                    let det = a11 * a22 - a12 * a21
                    if abs det > 1e-10 then
                        let sx = (b1 * a22 - b2 * a12) / det
                        let sy = (a11 * b2 - a21 * b1) / det
                        let isx = int (floor sx)
                        let isy = int (floor sy)
                        if isx >= 0 && isx < fb.W && isy >= 0 && isy < fb.H then
                            let sc = tmp.Px.Span.[isy * fb.W + isx]
                            if Pixel.a sc > 0uy then
                                let sc' = if opacity < 1.0 then applyOpacity sc opacity else sc
                                let fpx = fb.Px.Span
                                let i = py * fb.W + px
                                fpx.[i] <- blendPx sc' fpx.[i]
            releaseTempFb tmp
        | PathBool (op, segsA, segsB, paint, fillRule) ->
            let paint' = transformPaint m paint
            let segsA' = segsA |> List.map (transformPathSeg m)
            let segsB' = segsB |> List.map (transformPathSeg m)
            let resultSegs = pathBoolOp op segsA' segsB'
            let contours = flattenPath resultSegs
            scanlineFillPaint fb contours paint' fillRule
        | Blit (src, dstPos, opacity) ->
            let dp = Matrix3x2.transformVec2 m dstPos
            let dx = int (float dp.X)
            let dy = int (float dp.Y)
            let spx = src.Px.Span
            let fpx = fb.Px.Span
            for sy = 0 to src.H - 1 do
                let ty = dy + sy
                if ty >= 0 && ty < fb.H then
                    for sx = 0 to src.W - 1 do
                        let tx = dx + sx
                        if tx >= 0 && tx < fb.W then
                            let sc = spx.[sy * src.W + sx]
                            if Pixel.a sc > 0uy then
                                let sc' = applyOpacity sc opacity
                                let i = ty * fb.W + tx
                                fpx.[i] <- blendPx sc' fpx.[i]
        | BlitExternalTexture _ ->
            // EGLImage/dmabuf textures live in GPU memory; the software backend has
            // no GL context to sample them. Skipped (e.g. headless PNG screenshots of
            // GPU-buffer surfaces are blank — the GPU streaming path renders them).
            ()
        | BlitTexture _ ->
            // Persistent GL texture; no GL context here. The compositor emits a
            // plain Blit instead for the software path, so this is normally unreached.
            ()
        | Group (xform, opacity, cmds) ->
            let combined = Matrix3x2.multiply xform m
            if opacity >= 1.0 then
                cmds |> List.iter (dispatchWithTransform fb combined)
            else
                let tmp = getTempFb fb.W fb.H
                cmds |> List.iter (dispatchWithTransform tmp combined)
                let tpx = tmp.Px.Span
                let fpx = fb.Px.Span
                for i = 0 to tmp.Px.Length - 1 do
                    let sc = applyOpacity tpx.[i] opacity
                    if Pixel.a sc > 0uy then
                        fpx.[i] <- blendPx sc fpx.[i]
                releaseTempFb tmp
        | TransparencyGroup (isolated, knockout, opacity, mode, mask, cmds) ->
            dispatchTransparencyGroup fb m 0 None isolated knockout opacity mode mask cmds
        | ColorManagedGroup (conversion, isolated, knockout, opacity, mode, mask, cmds) ->
            dispatchTransparencyGroup fb m 0 (Some conversion) isolated knockout opacity mode mask cmds
        | ColorManagedGroup4 _ -> dispatchComponentGroup fb m 0 cmd
        | Components4 _ -> invalidOp "Components4 requires a component-managed group"
        | Clip (clipSegs, fillRule, cmds) ->
            let clipSegs' = clipSegs |> List.map (transformPathSeg m)
            match tryRectClip clipSegs' with
            | Some (cx, cy, cw, ch) ->
                // Fast path: axis-aligned rect clip — no mask needed
                let cx' = max 0 cx
                let cy' = max 0 cy
                let cw' = max 0 (min fb.W (cx + cw) - cx')
                let ch' = max 0 (min fb.H (cy + ch) - cy')
                if cw' > 0 && ch' > 0 then
                    let tmp = getTempFb cw' ch'
                    for y = 0 to ch' - 1 do
                        fb.Px.Span.Slice((cy' + y) * fb.W + cx', cw').CopyTo(tmp.Px.Span.Slice(y * cw', cw'))
                    let m' = { m with DX = m.DX - float cx'; DY = m.DY - float cy' }
                    cmds |> List.iter (dispatchWithTransform tmp m')
                    let tpx = tmp.Px.Span
                    let fpx = fb.Px.Span
                    for dy = 0 to ch' - 1 do
                        let frow = (cy' + dy) * fb.W + cx'
                        let trow = dy * cw'
                        for dx = 0 to cw' - 1 do
                            fpx.[frow + dx] <- tpx.[trow + dx]
                    releaseTempFb tmp
            | None ->
                let contours = flattenPath clipSegs'
                let mask = buildClipMask fb.W fb.H contours fillRule
                let tmp = getTempFb fb.W fb.H
                fb.Px.Span.CopyTo(tmp.Px.Span)
                cmds |> List.iter (dispatchWithTransform tmp m)
                let tpx = tmp.Px.Span
                let fpx = fb.Px.Span
                for i = 0 to fb.Px.Length - 1 do
                    if mask.[i] > 0uy then
                        fpx.[i] <- tpx.[i]
                releaseTempFb tmp
        | Filtered (filters, cmds) ->
            let tmp = getTempFb fb.W fb.H
            cmds |> List.iter (dispatchWithTransform tmp m)
            compositeFilteredLayer fb filters tmp
            releaseTempFb tmp
        | Mask (maskCmds, cmds) ->
            let maskFb = getTempFb fb.W fb.H
            maskCmds |> List.iter (dispatchWithTransform maskFb m)
            let tmp = getTempFb fb.W fb.H
            cmds |> List.iter (dispatchWithTransform tmp m)
            let tpx = tmp.Px.Span
            let mpx = maskFb.Px.Span
            let fpx = fb.Px.Span
            for i = 0 to fb.Px.Length - 1 do
                let sc = tpx.[i]
                let maskAlpha = float (Pixel.a mpx.[i]) / 255.0
                if Pixel.a sc > 0uy && maskAlpha > 0.0 then
                    let sc' = applyOpacity sc maskAlpha
                    fpx.[i] <- blendPx sc' fpx.[i]
            releaseTempFb tmp
            releaseTempFb maskFb
        | StrokeToFill (segs, style, paint, fillRule) ->
            let style' = strokeStyleInDeviceSpace m style
            let paint' = transformPaint m paint
            let segs' = segs |> List.map (transformPathSeg m)
            let contours = flattenStrokePath segs'
            let allOutline = ResizeArray<PathSeg>()
            for contour, closed in contours do
                let dashed = strokeDashes contour closed (style'.DashArray |> List.map float) (float style'.DashOffset)
                for sub, subClosed in dashed do
                    let outline = expandStroke sub subClosed style'
                    allOutline.AddRange(outline)
            let outlineContours = flattenPath (Seq.toList allOutline)
            scanlineFillPaint fb outlineContours paint' fillRule
        | SaveRestore cmds ->
            let tmp = getTempFb fb.W fb.H
            cmds |> List.iter (dispatchWithTransform tmp m)
            let tpx = tmp.Px.Span
            let fpx = fb.Px.Span
            for i = 0 to fb.Px.Length - 1 do
                let sc = tpx.[i]
                if Pixel.a sc > 0uy then
                    fpx.[i] <- blendPx sc fpx.[i]
            releaseTempFb tmp
        | Blend (mode, cmds) ->
            let tmp = getTempFb fb.W fb.H
            cmds |> List.iter (dispatchWithTransform tmp m)
            let tpx = tmp.Px.Span
            let fpx = fb.Px.Span
            for i = 0 to fb.Px.Length - 1 do
                let sc = tpx.[i]
                if Pixel.a sc > 0uy then
                    fpx.[i] <- blendWithMode mode sc fpx.[i]
            releaseTempFb tmp
        | AntiAlias (samples, cmds) ->
            let tmp = getTempFb fb.W fb.H
            cmds |> List.iter (dispatchWithTransformAA tmp m samples)
            let tpx = tmp.Px.Span
            let fpx = fb.Px.Span
            for i = 0 to fb.Px.Length - 1 do
                let sc = tpx.[i]
                if Pixel.a sc > 0uy then
                    fpx.[i] <- blendPx sc fpx.[i]
            releaseTempFb tmp
        | DrawGlyphs (origin, runs, paint) ->
            let ox, oy = float origin.X, float origin.Y
            let paint' = transformPaint m paint
            for run in runs do
                let s = run.FontSize / Text.unitsPerEm run.Font
                let ppem = int (round run.FontSize)
                // Outline bitmap cache keys have integral ppem; do not round a
                // fractional outline size or its ink no longer matches layout.
                let exactBitmapSize =
                    match run.Font.Face with
                    | Text.BitmapFace _ -> true
                    | Text.OutlineFace _ -> run.FontSize = float ppem
                let useBitmapCache = isPureTranslation m && exactBitmapSize && ppem >= 1 && ppem <= 128
                for g in run.Glyphs do
                    if useBitmapCache then
                        let wx = ox + g.X + m.DX
                        let wy = oy + g.Y + m.DY
                        let dstX = int (floor wx)
                        let dstY = int (floor wy)
                        let subX = int (floor ((wx - float dstX) * 4.0)) |> max 0 |> min 3
                        let subY = int (floor ((wy - float dstY) * 4.0)) |> max 0 |> min 3
                        match getCachedGlyphBitmap run.Font g.GlyphId ppem subX subY 4 with
                        | Some bmp ->
                            match paint' with
                            | Solid c -> blitGlyphSolid fb bmp dstX dstY c
                            | _ -> blitGlyphPaint fb bmp dstX dstY paint'
                        | None -> ()
                    else
                        match Text.tryGlyphBitmap run.Font g.GlyphId ppem with
                        | Some bmp ->
                            blitGlyphTransformed fb (fromTextBitmapGlyph bmp) (ox + g.X) (oy + g.Y) m paint'
                        | None ->
                            let outline = Text.glyphOutline run.Font g.GlyphId
                            if not outline.IsEmpty then
                                let segs' = outline |> List.map (Text.translatePathSeg (ox + g.X) (oy + g.Y) s >> transformPathSeg m)
                                let contours = flattenPath segs'
                                match paint' with
                                | Solid c when isPureTranslation m && not exactBitmapSize ->
                                    scanlineFillAA fb contours c 4 NonZero
                                | Solid c -> scanlineFill fb contours c NonZero
                                | _ when isPureTranslation m && not exactBitmapSize ->
                                    scanlineFillPaintAA fb contours paint' 4 NonZero
                                | _ -> scanlineFillPaint fb contours paint' NonZero
        | RetainedGlesLayer (bounds, layer) ->
            layer.SoftwareFallback bounds |> List.iter (dispatchWithTransform fb m)
        | ShaderFill _ -> ()

    and dispatchWithTransformAA (fb: Framebuffer) (m: Matrix3x2) (aaSamples: int) (cmd: Cmd) =
        if aaSamples <= 0 then
            dispatchWithTransform fb m cmd
        else
            let isIdentity (m: Matrix3x2) =
                m.M11 = 1.0 && m.M12 = 0.0 && m.M21 = 0.0 && m.M22 = 1.0 && m.DX = 0.0 && m.DY = 0.0
            match cmd with
            | WithShape (_, cmds) -> cmds |> List.iter (dispatchWithTransformAA fb m aaSamples)
            | FillRect (r, c) ->
                // Axis-aligned rects don't need AA — use fast path directly
                if isIdentity m then fillRect fb r c
                else
                    let isPureTranslate = m.M12 = 0.0 && m.M21 = 0.0 && m.M11 = 1.0 && m.M22 = 1.0
                    if isPureTranslate then
                        let r' = { Pos = vec2 (float r.Pos.X + m.DX) (float r.Pos.Y + m.DY); Size = r.Size }
                        fillRect fb r' c
                    else
                        let (p0, p1, p2, p3) = transformRect m r
                        fillTri fb p0 p1 p2 c
                        fillTri fb p0 p2 p3 c
            | FillPath (segs, paint, fillRule) ->
                let paint' = transformPaint m paint
                let segs' = segs |> List.map (transformPathSeg m)
                match tryRectClip segs' with
                | Some (x, y, w, h) -> fillPaintRect fb x y w h paint'
                | None ->
                    let contours = flattenPath segs'
                    scanlineFillPaintAA fb contours paint' aaSamples fillRule
            | FillCircle (center, radius, c) ->
                let path = circleToPath center radius 64 |> List.map (transformPathSeg m)
                let contours = flattenPath path
                scanlineFillAA fb contours c aaSamples EvenOdd
            | StrokePath (segs, style) ->
                let style' = strokeStyleInDeviceSpace m style
                let segs' = segs |> List.map (transformPathSeg m)
                let contours = flattenStrokePath segs'
                for contour, closed in contours do
                    let dashed = strokeDashes contour closed (style'.DashArray |> List.map float) (float style'.DashOffset)
                    for sub, subClosed in dashed do
                        let outline = expandStroke sub subClosed style'
                        let outlineContours = flattenPath outline
                        match style'.Paint with
                        | Solid c -> scanlineFillAA fb outlineContours c aaSamples EvenOdd
                        | _ -> scanlineFillPaintAA fb outlineContours style'.Paint aaSamples EvenOdd
            | FillRoundedRect (r, cr, c) ->
                let isPureTranslate = m.M12 = 0.0 && m.M21 = 0.0 && m.M11 = 1.0 && m.M22 = 1.0
                if isPureTranslate then
                    fillRoundedRectFast fb (float r.Pos.X + m.DX) (float r.Pos.Y + m.DY) (float r.Size.X) (float r.Size.Y) (float cr) c
                else
                    let path = roundedRectPath r cr |> List.map (transformPathSeg m)
                    let contours = flattenPath path
                    scanlineFillAA fb contours c aaSamples EvenOdd
            | StrokeRoundedRect (r, cr, style) ->
                let style' = strokeStyleInDeviceSpace m style
                let path = roundedRectPath r cr |> List.map (transformPathSeg m)
                let contours = flattenStrokePath path
                for contour, closed in contours do
                    let outline = expandStroke contour closed style'
                    let outlineContours = flattenPath outline
                    match style'.Paint with
                    | Solid c -> scanlineFillAA fb outlineContours c aaSamples EvenOdd
                    | _ -> scanlineFillPaintAA fb outlineContours style'.Paint aaSamples EvenOdd
            | FillRoundedRect4 (r, tl, tr, br, bl, c) ->
                let isPureTranslate = m.M12 = 0.0 && m.M21 = 0.0 && m.M11 = 1.0 && m.M22 = 1.0
                if isPureTranslate then
                    fillRoundedRect4Fast fb (float r.Pos.X + m.DX) (float r.Pos.Y + m.DY) (float r.Size.X) (float r.Size.Y) (float tl) (float tr) (float br) (float bl) c
                else
                    let path = roundedRectPath4 r tl tr br bl |> List.map (transformPathSeg m)
                    let contours = flattenPath path
                    scanlineFillAA fb contours c aaSamples EvenOdd
            | StrokeRoundedRect4 (r, tl, tr, br, bl, style) ->
                let style' = strokeStyleInDeviceSpace m style
                let path = roundedRectPath4 r tl tr br bl |> List.map (transformPathSeg m)
                let contours = flattenStrokePath path
                for contour, closed in contours do
                    let outline = expandStroke contour closed style'
                    let outlineContours = flattenPath outline
                    match style'.Paint with
                    | Solid c -> scanlineFillAA fb outlineContours c aaSamples EvenOdd
                    | _ -> scanlineFillPaintAA fb outlineContours style'.Paint aaSamples EvenOdd
            | Clip (clipSegs, fillRule, cmds) ->
                let clipSegs' = clipSegs |> List.map (transformPathSeg m)
                match tryRectClip clipSegs' with
                | Some (cx, cy, cw, ch) ->
                    // Fast path: axis-aligned rect clip — no mask needed
                    let cx' = max 0 cx
                    let cy' = max 0 cy
                    let cw' = max 0 (min fb.W (cx + cw) - cx')
                    let ch' = max 0 (min fb.H (cy + ch) - cy')
                    if cw' > 0 && ch' > 0 then
                        let tmp = getTempFb cw' ch'
                        for y = 0 to ch' - 1 do
                            fb.Px.Span.Slice((cy' + y) * fb.W + cx', cw').CopyTo(tmp.Px.Span.Slice(y * cw', cw'))
                        let m' = { m with DX = m.DX - float cx'; DY = m.DY - float cy' }
                        cmds |> List.iter (dispatchWithTransformAA tmp m' aaSamples)
                        let tpx = tmp.Px.Span
                        let fpx = fb.Px.Span
                        for dy = 0 to ch' - 1 do
                            let frow = (cy' + dy) * fb.W + cx'
                            let trow = dy * cw'
                            for dx = 0 to cw' - 1 do
                                fpx.[frow + dx] <- tpx.[trow + dx]
                        releaseTempFb tmp
                | None ->
                    let contours = flattenPath clipSegs'
                    let mask = buildClipMaskAA fb.W fb.H contours aaSamples fillRule
                    let tmp = getTempFb fb.W fb.H
                    fb.Px.Span.CopyTo(tmp.Px.Span)
                    cmds |> List.iter (dispatchWithTransformAA tmp m aaSamples)
                    let tpx = tmp.Px.Span
                    let fpx = fb.Px.Span
                    for i = 0 to fb.Px.Length - 1 do
                        if mask.[i] > 0uy then
                            let coverage = int mask.[i]
                            let source = Pixel.toColor tpx.[i]
                            let original = Pixel.toColor fpx.[i]
                            let lerp (s: byte) (d: byte) = byte ((int s * coverage + int d * (255 - coverage) + 127) / 255)
                            fpx.[i] <- Pixel.fromColor { R = lerp source.R original.R; G = lerp source.G original.G; B = lerp source.B original.B; A = lerp source.A original.A }
                    releaseTempFb tmp
            | Group (xform, opacity, cmds) ->
                let combined = Matrix3x2.multiply xform m
                if opacity >= 1.0 then
                    cmds |> List.iter (dispatchWithTransformAA fb combined aaSamples)
                else
                    let tmp = getTempFb fb.W fb.H
                    cmds |> List.iter (dispatchWithTransformAA tmp combined aaSamples)
                    let tpx = tmp.Px.Span
                    let fpx = fb.Px.Span
                    for i = 0 to tmp.Px.Length - 1 do
                        let sc = applyOpacity tpx.[i] opacity
                        if Pixel.a sc > 0uy then
                            fpx.[i] <- blendPx sc fpx.[i]
                    releaseTempFb tmp
            | TransparencyGroup (isolated, knockout, opacity, mode, mask, cmds) ->
                dispatchTransparencyGroup fb m aaSamples None isolated knockout opacity mode mask cmds
            | ColorManagedGroup (conversion, isolated, knockout, opacity, mode, mask, cmds) ->
                dispatchTransparencyGroup fb m aaSamples (Some conversion) isolated knockout opacity mode mask cmds
            | ColorManagedGroup4 _ -> dispatchComponentGroup fb m aaSamples cmd
            | PathBool (op, segsA, segsB, paint, fillRule) ->
                let paint' = transformPaint m paint
                let segsA' = segsA |> List.map (transformPathSeg m)
                let segsB' = segsB |> List.map (transformPathSeg m)
                let resultSegs = pathBoolOp op segsA' segsB'
                let contours = flattenPath resultSegs
                scanlineFillPaintAA fb contours paint' aaSamples fillRule
            | Mask (maskCmds, cmds) ->
                let maskFb = getTempFb fb.W fb.H
                maskCmds |> List.iter (dispatchWithTransformAA maskFb m aaSamples)
                let tmp = getTempFb fb.W fb.H
                cmds |> List.iter (dispatchWithTransformAA tmp m aaSamples)
                let tpx = tmp.Px.Span
                let mpx = maskFb.Px.Span
                let fpx = fb.Px.Span
                for i = 0 to fb.Px.Length - 1 do
                    let sc = tpx.[i]
                    let maskAlpha = float (Pixel.a mpx.[i]) / 255.0
                    if Pixel.a sc > 0uy && maskAlpha > 0.0 then
                        let sc' = applyOpacity sc maskAlpha
                        fpx.[i] <- blendPx sc' fpx.[i]
                releaseTempFb tmp
                releaseTempFb maskFb
            | StrokeToFill (segs, style, paint, fillRule) ->
                let style' = strokeStyleInDeviceSpace m style
                let paint' = transformPaint m paint
                let segs' = segs |> List.map (transformPathSeg m)
                let contours = flattenStrokePath segs'
                let allOutline = ResizeArray<PathSeg>()
                for contour, closed in contours do
                    let dashed = strokeDashes contour closed (style'.DashArray |> List.map float) (float style'.DashOffset)
                    for sub, subClosed in dashed do
                        let outline = expandStroke sub subClosed style'
                        allOutline.AddRange(outline)
                let outlineContours = flattenPath (Seq.toList allOutline)
                scanlineFillPaintAA fb outlineContours paint' aaSamples fillRule
            | SaveRestore cmds ->
                let tmp = getTempFb fb.W fb.H
                cmds |> List.iter (dispatchWithTransformAA tmp m aaSamples)
                let tpx = tmp.Px.Span
                let fpx = fb.Px.Span
                for i = 0 to fb.Px.Length - 1 do
                    let sc = tpx.[i]
                    if Pixel.a sc > 0uy then
                        fpx.[i] <- blendPx sc fpx.[i]
                releaseTempFb tmp
            | AntiAlias (samples, cmds) ->
                cmds |> List.iter (dispatchWithTransformAA fb m samples)
            | Blend (mode, cmds) ->
                let tmp = getTempFb fb.W fb.H
                cmds |> List.iter (dispatchWithTransformAA tmp m aaSamples)
                let tpx = tmp.Px.Span
                let fpx = fb.Px.Span
                for i = 0 to fb.Px.Length - 1 do
                    let sc = tpx.[i]
                    if Pixel.a sc > 0uy then
                        fpx.[i] <- blendWithMode mode sc fpx.[i]
                releaseTempFb tmp
            | Filtered (filters, cmds) ->
                let tmp = getTempFb fb.W fb.H
                cmds |> List.iter (dispatchWithTransformAA tmp m aaSamples)
                compositeFilteredLayer fb filters tmp
                releaseTempFb tmp
            | DrawGlyphs (origin, runs, paint) ->
                let ox, oy = float origin.X, float origin.Y
                let paint' = transformPaint m paint
                for run in runs do
                    let s = run.FontSize / Text.unitsPerEm run.Font
                    let ppem = int (round run.FontSize)
                    let exactBitmapSize =
                        match run.Font.Face with
                        | Text.BitmapFace _ -> true
                        | Text.OutlineFace _ -> run.FontSize = float ppem
                    let useBitmapCache = isPureTranslation m && exactBitmapSize && ppem >= 1 && ppem <= 128
                    for g in run.Glyphs do
                        if useBitmapCache then
                            let wx = ox + g.X + m.DX
                            let wy = oy + g.Y + m.DY
                            let dstX = int (floor wx)
                            let dstY = int (floor wy)
                            let subX = int (floor ((wx - float dstX) * 4.0)) |> max 0 |> min 3
                            let subY = int (floor ((wy - float dstY) * 4.0)) |> max 0 |> min 3
                            match getCachedGlyphBitmap run.Font g.GlyphId ppem subX subY aaSamples with
                            | Some bmp ->
                                match paint' with
                                | Solid c -> blitGlyphSolid fb bmp dstX dstY c
                                | _ -> blitGlyphPaint fb bmp dstX dstY paint'
                            | None -> ()
                        else
                            match Text.tryGlyphBitmap run.Font g.GlyphId ppem with
                            | Some bmp ->
                                blitGlyphTransformed fb (fromTextBitmapGlyph bmp) (ox + g.X) (oy + g.Y) m paint'
                            | None ->
                                let outline = Text.glyphOutline run.Font g.GlyphId
                                if not outline.IsEmpty then
                                    let segs' = outline |> List.map (Text.translatePathSeg (ox + g.X) (oy + g.Y) s >> transformPathSeg m)
                                    let contours = flattenPath segs'
                                    match paint' with
                                    | Solid c -> scanlineFillAA fb contours c aaSamples NonZero
                                    | _ -> scanlineFillPaintAA fb contours paint' aaSamples NonZero
            | _ -> dispatchWithTransform fb m cmd
    and dispatchTransparencyGroup (fb: Framebuffer) (m: Matrix3x2) (aaSamples: int)
                                  (conversion: ColorSpaceConversion option) (isolated: bool) (knockout: bool) (opacity: float)
                                  (mode: BlendMode) (maskCmds: Cmd list option) (cmds: Cmd list) =
        if opacity > 0.0 && not cmds.IsEmpty then
            let groupBackdrop =
                match conversion with
                | Some value when not isolated ->
                    let converted = getTempFb fb.W fb.H
                    for i = 0 to fb.Px.Length - 1 do
                        converted.Px.Span.[i] <- convertTransparencyPixel value.ToGroup fb.Px.Span.[i]
                    converted
                | _ -> fb
            let group = getTempFb fb.W fb.H
            if not isolated then groupBackdrop.Px.Span.CopyTo(group.Px.Span)
            let alpha = if isolated && not knockout then group else getTempFb fb.W fb.H
            if isolated && not knockout then
                for cmd in cmds do
                    for obj in transparencyObjects cmd do
                        let samples, obj = transparencyObjectSampling aaSamples obj
                        dispatchWithTransformAA group m samples obj
            else
                let source = getTempFb fb.W fb.H
                let shape = if knockout then Some (getTempFb fb.W fb.H) else None
                let mutable candidate = None
                for cmd in cmds do
                    for obj in transparencyObjects cmd do
                        let samples, obj = transparencyObjectSampling aaSamples obj
                        let sourceCmd, objectMode = transparencyObjectBlend obj
                        source.Px.Span.Clear()
                        dispatchWithTransformAA source m samples sourceCmd
                        match shape with
                        | Some shapeFb ->
                            shapeFb.Px.Span.Clear()
                            dispatchWithTransformAA shapeFb m samples (Cmd.shapeOf obj)
                        | None -> ()
                        if transparencyNeedsBackdrop obj then
                            let candidateFb =
                                match candidate with
                                | Some value -> value
                                | None ->
                                    let value = getTempFb fb.W fb.H
                                    candidate <- Some value
                                    value
                            if knockout then
                                if isolated then candidateFb.Px.Span.Clear()
                                else groupBackdrop.Px.Span.CopyTo(candidateFb.Px.Span)
                            else group.Px.Span.CopyTo(candidateFb.Px.Span)
                            dispatchWithTransformAA candidateFb m samples obj
                            accumulateTransparencyCandidate group alpha groupBackdrop candidateFb source shape isolated
                        else
                            accumulateTransparencyObject group alpha groupBackdrop source shape isolated objectMode
                match candidate with Some candidateFb -> releaseTempFb candidateFb | None -> ()
                match shape with Some shapeFb -> releaseTempFb shapeFb | None -> ()
                releaseTempFb source
            let mask =
                match maskCmds with
                | None -> None
                | Some children ->
                    let maskFb = getTempFb fb.W fb.H
                    children |> List.iter (dispatchWithTransformAA maskFb m aaSamples)
                    Some maskFb
            finishTransparencyGroup fb group alpha mask isolated opacity mode conversion groupBackdrop
            match mask with Some maskFb -> releaseTempFb maskFb | None -> ()
            if not (isolated && not knockout) then releaseTempFb alpha
            releaseTempFb group
            if not isolated && conversion.IsSome then releaseTempFb groupBackdrop

    and dispatchComponentGroup (fb: Framebuffer) (m: Matrix3x2) (aaSamples: int) (cmd: Cmd) =
        let pixels, copyBack =
            match Framebuffer.tryGetArray fb with
            | Some pixels when pixels.Length = fb.W * fb.H -> pixels, false
            | _ -> fb.Px.ToArray(), true
        let renderObject commands =
            let result = Array.zeroCreate<uint32> (fb.W * fb.H)
            let target = Framebuffer.ofArray result fb.W fb.H
            commands |> List.iter (dispatchWithTransformAA target m aaSamples)
            result
        ComponentCompositor.render fb.W fb.H pixels renderObject cmd
        if copyBack then System.Span<uint32>(pixels).CopyTo(fb.Px.Span)


    let dispatch (fb: Framebuffer) (cmd: Cmd) =
        dispatchWithTransform fb Matrix3x2.identity cmd

    /// Rasterize an offscreen four-component surface. Both returned images
    /// have the same premultiplied alpha; the fourth component is grayscale.
    let rasterizeComponents width height transform aaSamples complement commands =
        let renderObject children =
            let pixels = Array.zeroCreate<uint32> (width * height)
            let target = Framebuffer.ofArray pixels width height
            children |> List.iter (dispatchWithTransformAA target transform aaSamples)
            pixels
        let first, fourth =
            ComponentCompositor.renderComponents width height renderObject complement commands
        Framebuffer.ofArray first width height, Framebuffer.ofArray fourth width height

    let create () : Backend =
        let mutable fb = Framebuffer.create 1 1 Color.black
        let profileSubmit (cmds: Cmd list) =
            let times = Dictionary<string, float>()
            let counts = Dictionary<string, int>()
            let add kind ms =
                let mutable oldMs = 0.0
                if times.TryGetValue(kind, &oldMs) then
                    times.[kind] <- oldMs + ms
                else
                    times.[kind] <- ms
                let mutable oldCount = 0
                if counts.TryGetValue(kind, &oldCount) then
                    counts.[kind] <- oldCount + 1
                else
                    counts.[kind] <- 1
            let paintKind = function
                | NoPaint -> "NoPaint"
                | Solid _ -> "Solid"
                | LinearGradient _ -> "LinearGradient"
                | RadialGradient _ -> "RadialGradient"
                | ConicGradient _ -> "ConicGradient"
                | FocalRadialGradient _ -> "FocalRadialGradient"
                | TilingPaint _ -> "TilingPaint"
                | ImagePaint _ -> "ImagePaint"
            let kindOf = function
                | WithShape _ -> "WithShape"
                | Clear _ -> "Clear"
                | Line _ -> "Line"
                | FillRect _ -> "FillRect"
                | StrokeRect _ -> "StrokeRect"
                | FillCircle _ -> "FillCircle"
                | FillTri _ -> "FillTri"
                | FillPath(_, paint, _) -> "FillPath:" + paintKind paint
                | StrokePath(_, style) -> "StrokePath:" + paintKind style.Paint
                | Group _ -> "Group"
                | TransparencyGroup _ -> "TransparencyGroup"
                | ColorManagedGroup _ -> "ColorManagedGroup"
                | ColorManagedGroup4 _ -> "ColorManagedGroup4"
                | Components4 _ -> "Components4"
                | Clip _ -> "Clip"
                | Filtered _ -> "Filtered"
                | FillRoundedRect _ -> "FillRoundedRect"
                | StrokeRoundedRect _ -> "StrokeRoundedRect"
                | FillRoundedRect4 _ -> "FillRoundedRect4"
                | StrokeRoundedRect4 _ -> "StrokeRoundedRect4"
                | DrawImage _ -> "DrawImage"
                | DrawImageReduced _ -> "DrawImageReduced"
                | BoxShadow _ -> "BoxShadow"
                | PathBool _ -> "PathBool"
                | Blit _ -> "Blit"
                | BlitExternalTexture _ -> "BlitExternalTexture"
                | BlitTexture _ -> "BlitTexture"
                | Mask _ -> "Mask"
                | StrokeToFill(_, _, paint, _) -> "StrokeToFill:" + paintKind paint
                | SaveRestore _ -> "SaveRestore"
                | Blend _ -> "Blend"
                | AntiAlias _ -> "AntiAlias"
                | DrawGlyphs _ -> "DrawGlyphs"
                | DrawBorder _ -> "DrawBorder"
                | BackdropFilter _ -> "BackdropFilter"
                | Group3D _ -> "Group3D"
                | ShaderFill _ -> "ShaderFill"
                | RetainedGlesLayer _ -> "RetainedGlesLayer"
            let sw = System.Diagnostics.Stopwatch()
            let rec submitOne (m: Matrix3x2) (aaSamples: int) (cmd: Cmd) =
                match cmd with
                | Group (xform, opacity, children) when opacity >= 1.0 ->
                    let combined = Matrix3x2.multiply xform m
                    children |> List.iter (submitOne combined aaSamples)
                | AntiAlias (samples, children) ->
                    children |> List.iter (submitOne m samples)
                | _ ->
                    sw.Restart()
                    dispatchWithTransformAA fb m aaSamples cmd
                    add (kindOf cmd) sw.Elapsed.TotalMilliseconds
            cmds |> List.iter (submitOne Matrix3x2.identity 4)
            let summary =
                times
                |> Seq.map (fun kvp ->
                    let count =
                        let mutable c = 0
                        if counts.TryGetValue(kvp.Key, &c) then c else 0
                    kvp.Key, count, kvp.Value)
                |> Seq.sortByDescending (fun (_, _, ms) -> ms)
                |> Seq.truncate 12
                |> Seq.map (fun (kind, count, ms) -> sprintf "%s=%d/%.1fms" kind count ms)
                |> String.concat " "
            eprintfn "[raster-profile] %s" summary
        { Init = fun w h -> fb <- Framebuffer.create w h Color.black
          InitInto = fun target -> clearFb target Color.black; fb <- target
          Submit = fun cmds ->
              if System.Environment.GetEnvironmentVariable("BROWSER_TRACE_RASTER_PROFILE") = "1" then
                  profileSubmit cmds
              else
                  cmds |> List.iter (dispatchWithTransformAA fb Matrix3x2.identity 4)
          Finish = fun () -> fb }
