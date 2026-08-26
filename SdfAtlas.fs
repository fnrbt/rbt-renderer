namespace Rbt.Renderer

open System.Collections.Generic

module SdfAtlas =

    let [<Literal>] SdfSize = 48
    let [<Literal>] SdfPadding = 4
    let [<Literal>] SdfSpread = 6.0f

    // ── SDF generation ──

    /// Flatten Rbt.OpenType PathCmd[] to polyline segments for rasterization.
    let private flattenOutline (cmds: Rbt.OpenType.Outline.PathCmd[]) : (float * float) list list =
        let contours = ResizeArray<(float * float) list>()
        let current = ResizeArray<float * float>()
        for cmd in cmds do
            match cmd with
            | Rbt.OpenType.Outline.MoveTo (x, y) ->
                if current.Count > 0 then
                    contours.Add(Seq.toList current)
                    current.Clear()
                current.Add((x, y))
            | Rbt.OpenType.Outline.LineTo (x, y) ->
                current.Add((x, y))
            | Rbt.OpenType.Outline.QuadTo (cx, cy, x, y) ->
                // Subdivide quadratic into line segments
                let (sx, sy) = if current.Count > 0 then current.[current.Count - 1] else (0.0, 0.0)
                let n = 8
                for i = 1 to n do
                    let t = float i / float n
                    let it = 1.0 - t
                    let px = it * it * sx + 2.0 * it * t * cx + t * t * x
                    let py = it * it * sy + 2.0 * it * t * cy + t * t * y
                    current.Add((px, py))
            | Rbt.OpenType.Outline.CubicTo (cx1, cy1, cx2, cy2, x, y) ->
                let (sx, sy) = if current.Count > 0 then current.[current.Count - 1] else (0.0, 0.0)
                let n = 12
                for i = 1 to n do
                    let t = float i / float n
                    let it = 1.0 - t
                    let px = it*it*it*sx + 3.0*it*it*t*cx1 + 3.0*it*t*t*cx2 + t*t*t*x
                    let py = it*it*it*sy + 3.0*it*it*t*cy1 + 3.0*it*t*t*cy2 + t*t*t*y
                    current.Add((px, py))
            | Rbt.OpenType.Outline.Close ->
                if current.Count > 0 then
                    contours.Add(Seq.toList current)
                    current.Clear()
        if current.Count > 0 then
            contours.Add(Seq.toList current)
        Seq.toList contours

    /// Scanline even-odd fill to binary bitmap.
    let private rasterizeBinary (contours: (float * float) list list) (w: int) (h: int) (ox: float) (oy: float) (scale: float) : bool[] =
        let bitmap = Array.zeroCreate<bool> (w * h)
        for py = 0 to h - 1 do
            let y = (float (h - 1 - py) + 0.5 - oy) / scale
            let mutable crossings = []
            for contour in contours do
                let pts = contour |> List.toArray
                let n = pts.Length
                for i = 0 to n - 1 do
                    let (x0, y0) = pts.[i]
                    let (x1, y1) = pts.[(i + 1) % n]
                    if (y0 <= y && y1 > y) || (y1 <= y && y0 > y) then
                        let t = (y - y0) / (y1 - y0)
                        let xi = x0 + t * (x1 - x0)
                        crossings <- xi :: crossings
            let sorted = crossings |> List.sort |> List.toArray
            let mutable inside = false
            let mutable ci = 0
            for px = 0 to w - 1 do
                let x = (float px + 0.5 - ox) / scale
                while ci < sorted.Length && sorted.[ci] <= x do
                    inside <- not inside
                    ci <- ci + 1
                if inside then bitmap.[py * w + px] <- true
        bitmap

    /// 1D Euclidean Distance Transform (Felzenszwalb-Huttenlocher).
    let private edt1d (f: float[]) (n: int) : float[] =
        let d = Array.zeroCreate<float> n
        let v = Array.zeroCreate<int> n
        let z = Array.create (n + 1) infinity
        v.[0] <- 0
        z.[0] <- -infinity
        z.[1] <- infinity
        let mutable k = 0
        for q = 1 to n - 1 do
            let mutable s = ((f.[q] + float (q * q)) - (f.[v.[k]] + float (v.[k] * v.[k]))) / (2.0 * float (q - v.[k]))
            while s <= z.[k] do
                k <- k - 1
                s <- ((f.[q] + float (q * q)) - (f.[v.[k]] + float (v.[k] * v.[k]))) / (2.0 * float (q - v.[k]))
            k <- k + 1
            v.[k] <- q
            z.[k] <- s
            z.[k + 1] <- infinity
        k <- 0
        for q = 0 to n - 1 do
            while z.[k + 1] < float q do k <- k + 1
            d.[q] <- float ((q - v.[k]) * (q - v.[k])) + f.[v.[k]]
        d

    /// 2D EDT: apply 1D row-wise then column-wise.
    let private edt2d (grid: float[]) (w: int) (h: int) : float[] =
        // Row pass
        let row = Array.zeroCreate<float> w
        for y = 0 to h - 1 do
            for x = 0 to w - 1 do row.[x] <- grid.[y * w + x]
            let d = edt1d row w
            for x = 0 to w - 1 do grid.[y * w + x] <- d.[x]
        // Column pass
        let col = Array.zeroCreate<float> h
        for x = 0 to w - 1 do
            for y = 0 to h - 1 do col.[y] <- grid.[y * w + x]
            let d = edt1d col h
            for y = 0 to h - 1 do grid.[y * w + x] <- d.[y]
        grid

    /// Generate SDF bitmap for a glyph.
    let generateSdf (font: Text.FontData) (glyphId: int) (sdfSize: int) : byte[] =
        match Text.tryOpenTypeFont font with
        | None -> Array.zeroCreate (sdfSize * sdfSize)
        | Some otFont ->
            let cmds = Rbt.OpenType.Font.outline otFont glyphId
            if cmds.Length = 0 then Array.zeroCreate (sdfSize * sdfSize)
            else
                let contours = flattenOutline cmds
                // Compute bbox
                let allPts = contours |> List.collect id
                let xs = allPts |> List.map fst
                let ys = allPts |> List.map snd
                let xmin = List.min xs
                let xmax = List.max xs
                let ymin = List.min ys
                let ymax = List.max ys
                let gw = xmax - xmin
                let gh = ymax - ymin
                let maxDim = max gw gh
                if maxDim < 1e-6 then Array.zeroCreate (sdfSize * sdfSize)
                else
                    let pad = float SdfPadding
                    let scale = (float sdfSize - 2.0 * pad) / maxDim
                    let ox = pad - xmin * scale + (float sdfSize - 2.0 * pad - gw * scale) / 2.0
                    let oy = pad - ymin * scale + (float sdfSize - 2.0 * pad - gh * scale) / 2.0
                    let bitmap = rasterizeBinary contours sdfSize sdfSize ox oy scale
                    let inf = 1e10
                    // Inside distance
                    let insideGrid = Array.init (sdfSize * sdfSize) (fun i -> if bitmap.[i] then 0.0 else inf)
                    let outsideGrid = Array.init (sdfSize * sdfSize) (fun i -> if bitmap.[i] then inf else 0.0)
                    let insideDist = edt2d insideGrid sdfSize sdfSize
                    let outsideDist = edt2d outsideGrid sdfSize sdfSize
                    let sdf = Array.zeroCreate<byte> (sdfSize * sdfSize)
                    for i = 0 to sdfSize * sdfSize - 1 do
                        let di = sqrt insideDist.[i]
                        let do' = sqrt outsideDist.[i]
                        let v = 128.0 + float SdfSpread * (do' - di)
                        sdf.[i] <- byte (max 0.0 (min 255.0 v))
                    sdf

    // ── Atlas packing ──

    type GlyphEntry = {
        U0: float32; V0: float32; U1: float32; V1: float32
        BearingX: float; BearingY: float
        GlyphW: float; GlyphH: float
    }

    type Atlas = {
        mutable Pixels: byte[]
        mutable W: int; mutable H: int
        mutable CursorX: int; mutable CursorY: int; mutable RowH: int
        Entries: Dictionary<int * int, GlyphEntry>
        mutable Dirty: bool
        mutable TexId: int
    }

    let createAtlas () : Atlas =
        let w, h = 512, 512
        { Pixels = Array.zeroCreate (w * h)
          W = w; H = h
          CursorX = 0; CursorY = 0; RowH = 0
          Entries = Dictionary()
          Dirty = true; TexId = 0 }

    let private atlasCache = Dictionary<int, Atlas>()

    let getAtlas (font: Text.FontData) : Atlas =
        let key = Text.fontId font
        match atlasCache.TryGetValue(key) with
        | true, a -> a
        | _ ->
            let a = createAtlas ()
            atlasCache.[key] <- a
            a

    let ensureGlyph (atlas: Atlas) (font: Text.FontData) (glyphId: int) : GlyphEntry =
        let key = (Text.fontId font, glyphId)
        match atlas.Entries.TryGetValue(key) with
        | true, e -> e
        | _ ->
            let sdf = generateSdf font glyphId SdfSize
            let gs = SdfSize
            // Place in atlas with shelf packing
            if atlas.CursorX + gs > atlas.W then
                atlas.CursorX <- 0
                atlas.CursorY <- atlas.CursorY + atlas.RowH
                atlas.RowH <- 0
            if atlas.CursorY + gs > atlas.H then
                // Grow atlas
                let newH = atlas.H * 2
                let newPixels = Array.zeroCreate (atlas.W * newH)
                System.Array.Copy(atlas.Pixels, newPixels, atlas.Pixels.Length)
                atlas.Pixels <- newPixels
                atlas.H <- newH
            // Copy SDF into atlas
            let ax, ay = atlas.CursorX, atlas.CursorY
            for row = 0 to gs - 1 do
                System.Array.Copy(sdf, row * gs, atlas.Pixels, (ay + row) * atlas.W + ax, gs)
            let u0 = float32 ax / float32 atlas.W
            let v0 = float32 ay / float32 atlas.H
            let u1 = float32 (ax + gs) / float32 atlas.W
            let v1 = float32 (ay + gs) / float32 atlas.H
            // Compute glyph metrics for positioning
            let otFont = Text.tryOpenTypeFont font
            let upm = otFont |> Option.map (fun f -> float f.Head.UnitsPerEm) |> Option.defaultValue 1.0
            let cmds = otFont |> Option.map (fun f -> Rbt.OpenType.Font.outline f glyphId) |> Option.defaultValue [||]
            let bx, by, gw, gh =
                if cmds.Length = 0 then 0.0, 0.0, 0.0, 0.0
                else
                    match otFont |> Option.bind (fun f -> Rbt.OpenType.Font.glyphBBox f glyphId) with
                    | Some bbox -> bbox.XMin / upm, bbox.YMin / upm, (bbox.XMax - bbox.XMin) / upm, (bbox.YMax - bbox.YMin) / upm
                    | None -> 0.0, 0.0, 0.0, 0.0
            let entry = { U0 = u0; V0 = v0; U1 = u1; V1 = v1
                          BearingX = bx; BearingY = by; GlyphW = gw; GlyphH = gh }
            atlas.Entries.[key] <- entry
            atlas.CursorX <- atlas.CursorX + gs
            atlas.RowH <- max atlas.RowH gs
            atlas.Dirty <- true
            entry

    let clearCaches () =
        atlasCache.Clear()
