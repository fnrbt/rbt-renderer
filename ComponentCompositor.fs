namespace Rbt.Renderer

/// Component-space transparency. Surfaces store premultiplied straight-space
/// components followed by alpha, in a Fable-compatible Float32Array.
module ComponentCompositor =
    /// A tightly packed premultiplied raster rectangle in device coordinates.
    type RasterRect =
        { X: int; Y: int; Width: int; Height: int; Pixels: uint32[] }

    /// Conservative device-space rectangle containing every pixel a command
    /// can affect, including effects of transparent source pixels. Backends
    /// must return the full surface when they cannot prove a tighter bound.
    type Bounds = { X: int; Y: int; Width: int; Height: int }

    // All accesses through this helper are inside loops bounded by the
    // validated surface length. Fable's checked Array.item call on each pixel
    // dominates sparse component compositing in the browser.
#if FABLE_COMPILER
    [<Fable.Core.Emit("$0[$1]")>]
    let private pixelAt (a: 'T[]) (i: int) : 'T = Fable.Core.Util.jsNative
#else
    let inline private pixelAt (a: 'T[]) (i: int) : 'T = a.[i]
#endif

#if FABLE_COMPILER
    [<Fable.Core.Emit("$0[$1] = $2")>]
    let private setPixel (a: 'T[]) (i: int) (value: 'T) : unit = Fable.Core.Util.jsNative
#else
    let inline private setPixel (a: 'T[]) (i: int) (value: 'T) = a.[i] <- value
#endif

    let inline private clamp x = max 0.0 (min 1.0 x)
    let inline private get (a: float32[]) i = float a.[i]
    let inline private put (a: float32[]) i v = a.[i] <- float32 (clamp v)
    let inline private straight (a: float32[]) i alpha = if alpha > 0.0 then clamp (get a i / alpha) else 0.0
    let inline private complemented complement x = if complement then 1.0-x else x
    let inline private toByte (a: float32[]) i = byte (int (clamp (get a i)*255.0+0.5))

    let private blendChannel mode b s =
        match mode with
        | Normal -> s
        | Multiply -> b * s
        | Screen -> b + s - b * s
        | Overlay -> if b <= 0.5 then 2.0*b*s else 1.0-2.0*(1.0-b)*(1.0-s)
        | Darken -> min b s
        | Lighten -> max b s
        | ColorDodge -> if b <= 0.0 then 0.0 elif s >= 1.0 then 1.0 else min 1.0 (b/(1.0-s))
        | ColorBurn -> if b >= 1.0 then 1.0 elif s <= 0.0 then 0.0 else 1.0-min 1.0 ((1.0-b)/s)
        | HardLight -> if s <= 0.5 then 2.0*b*s else 1.0-2.0*(1.0-b)*(1.0-s)
        | SoftLight ->
            if s <= 0.5 then b-(1.0-2.0*s)*b*(1.0-b)
            else let d = if b <= 0.25 then ((16.0*b-12.0)*b+4.0)*b else sqrt b
                 b+(2.0*s-1.0)*(d-b)
        | Difference -> abs (b-s)
        | Exclusion -> b+s-2.0*b*s
        | _ -> s

    let private lum r g b = 0.3*r + 0.59*g + 0.11*b
    let private sat r g b = max r (max g b) - min r (min g b)
    let private setLum r g b l =
        let d = l - lum r g b
        let r, g, b = r+d, g+d, b+d
        let lo, hi = min r (min g b), max r (max g b)
        let r, g, b = if lo < 0.0 then l+(r-l)*l/(l-lo), l+(g-l)*l/(l-lo), l+(b-l)*l/(l-lo) else r,g,b
        if hi > 1.0 then struct(l+(r-l)*(1.0-l)/(hi-l), l+(g-l)*(1.0-l)/(hi-l), l+(b-l)*(1.0-l)/(hi-l)) else struct(r,g,b)
    let private setSat r g b s =
        let lo, hi = min r (min g b), max r (max g b)
        if hi <= lo then struct(0.0,0.0,0.0)
        else struct((r-lo)*s/(hi-lo), (g-lo)*s/(hi-lo), (b-lo)*s/(hi-lo))
    let private nonseparable mode br bg bb sr sg sb =
        match mode with
        | Hue -> let struct(r,g,b) = setSat sr sg sb (sat br bg bb) in setLum r g b (lum br bg bb)
        | Saturation -> let struct(r,g,b) = setSat br bg bb (sat sr sg sb) in setLum r g b (lum br bg bb)
        | ColorMode -> setLum sr sg sb (lum br bg bb)
        | Luminosity -> setLum br bg bb (lum sr sg sb)
        | _ -> struct(sr,sg,sb)

    let private compositeNormalRange (dst: float32[]) (src: float32[]) count first last =
        for p = first to last do
            let i = p*5
            let sa = pixelAt src (i+4)
            if sa > 0.0f then
                let inv = 1.0f-sa
                for c = 0 to count-1 do
                    dst.[i+c] <- src.[i+c] + dst.[i+c]*inv
                dst.[i+4] <- sa + dst.[i+4]*inv

    // src and dst are premultiplied; complement only the inputs to the blend
    // function, never the storage or the color-space conversion callbacks.
    let private composite (dst: float32[]) (src: float32[]) count complement mode =
        if mode = Normal then
            compositeNormalRange dst src count 0 (dst.Length / 5 - 1)
        else
            let nonsep = match mode with Hue | Saturation | ColorMode | Luminosity -> true | _ -> false
            for p = 0 to dst.Length / 5 - 1 do
                let i = p*5
                let sa, da = get src (i+4), get dst (i+4)
                if sa > 0.0 || mode = SrcIn || mode = SrcOut || mode = DstAtop then
                    let br,bg,bb = complemented complement (straight dst i da), complemented complement (straight dst (i+1) da), complemented complement (straight dst (i+2) da)
                    let sr,sg,sb = complemented complement (straight src i sa), complemented complement (straight src (i+1) sa), complemented complement (straight src (i+2) sa)
                    let struct(nr,ng,nb) = if nonsep then nonseparable mode br bg bb sr sg sb else struct(0.0,0.0,0.0)
                    for c = 0 to count-1 do
                        let b, s = straight dst (i+c) da, straight src (i+c) sa
                        let v =
                            if nonsep && c < 3 then (if c=0 then nr elif c=1 then ng else nb)
                            elif nonsep && c = 3 then (if mode = Luminosity then complemented complement s else complemented complement b)
                            else blendChannel mode (complemented complement b) (complemented complement s)
                        let v = complemented complement v
                        let value =
                            match mode with
                            | SrcIn -> get src (i+c)*da
                            | SrcOut -> get src (i+c)*(1.0-da)
                            | DstOver -> get dst (i+c)+get src (i+c)*(1.0-da)
                            | DstAtop -> get dst (i+c)*sa+get src (i+c)*(1.0-da)
                            | Xor -> get dst (i+c)*(1.0-sa)+get src (i+c)*(1.0-da)
                            | _ -> (1.0-sa)*get dst (i+c) + (1.0-da)*get src (i+c) + sa*da*v
                        put dst (i+c) value
                    let outAlpha =
                        match mode with
                        | SrcIn -> sa*da
                        | SrcOut -> sa*(1.0-da)
                        | DstAtop -> sa
                        | Xor -> sa*(1.0-da)+da*(1.0-sa)
                        | _ -> sa+da*(1.0-sa)
                    put dst (i+4) outAlpha

    let private convert (source: float32[]) count (f: float -> float -> float -> float -> struct(float*float*float*float)) =
        let result = Array.zeroCreate<float32> source.Length
        for p = 0 to source.Length/5-1 do
            let i = p*5
            let a = get source (i+4)
            if a > 0.0 then
                let x = straight source i a
                let y = straight source (i+1) a
                let z = straight source (i+2) a
                let k = if count > 3 then straight source (i+3) a else 0.0
                let struct(x,y,z,k) = f x y z k
                put result i (x*a); put result (i+1) (y*a); put result (i+2) (z*a); put result (i+3) (k*a); put result (i+4) a
        result

    let private fromPixels (pixels: uint32[]) =
        let a = Array.zeroCreate<float32> (pixels.Length*5)
        for p = 0 to pixels.Length-1 do
            let v = pixels.[p]
            let i = p*5
            a.[i] <- float32 (Pixel.r v) / 255.0f
            a.[i+1] <- float32 (Pixel.g v) / 255.0f
            a.[i+2] <- float32 (Pixel.b v) / 255.0f
            a.[i+4] <- float32 (Pixel.a v) / 255.0f
        a

    let rec private renderCore (width: int) (height: int) (surface: float32[]) (raster: Cmd list -> RasterRect) (bounds: Cmd list -> Bounds) count complement command (alphaOut: float32[] option) allowRootBounds =
        let n = width*height
        if surface.Length <> n*5 then invalidArg "surface" "Incorrect surface size"
        let mutable firstCommand = true
        let rasterOne wrap cmd =
            let result = raster [wrap cmd]
            if result.X < 0 || result.Y < 0 || result.Width < 0 || result.Height < 0 ||
               result.X > width || result.Y > height || result.Width > width-result.X ||
               result.Height > height-result.Y || result.Pixels.Length <> result.Width*result.Height then
                invalidArg "raster" "Incorrect raster rectangle"
            result
        let alphaOf (v:uint32) = float (Pixel.a v)/255.0
        let mutable componentX = 0
        let mutable componentY = 0
        let mutable componentWidth = 0
        let mutable componentHeight = 0
        let clearComponent (source: float32[]) =
            for y = componentY to componentY+componentHeight-1 do
                Array.fill source ((y*width+componentX)*5) (componentWidth*5) 0.0f
        let unpack (source: float32[]) (r: RasterRect) fourth =
            for y = 0 to r.Height-1 do
                for x = 0 to r.Width-1 do
                    let v = pixelAt r.Pixels (y*r.Width+x)
                    if v <> 0u then
                        let px, py = r.X+x, r.Y+y
                        let right = max (componentX+componentWidth) (px+1)
                        let bottom = max (componentY+componentHeight) (py+1)
                        componentX <- min componentX px
                        componentY <- min componentY py
                        componentWidth <- right-componentX
                        componentHeight <- bottom-componentY
                        let i = (py*width+px)*5
                        if fourth then setPixel source (i+3) (float32 (Pixel.r v)/255.0f)
                        else
                            setPixel source i (float32 (Pixel.r v)/255.0f)
                            setPixel source (i+1) (float32 (Pixel.g v)/255.0f)
                            setPixel source (i+2) (float32 (Pixel.b v)/255.0f)
                            setPixel source (i+4) (float32 (Pixel.a v)/255.0f)
        let fullPixels (r: RasterRect) =
            if r.X = 0 && r.Y = 0 && r.Width = width && r.Height = height then r.Pixels
            else
                let result = Array.zeroCreate<uint32> n
                for y = 0 to r.Height-1 do
                    Array.blit r.Pixels (y*r.Width) result ((r.Y+y)*width+r.X) r.Width
                result
        let track (target: float32[] option) (source: float32[]) =
            match target with
            | None -> ()
            | Some alpha ->
                for p = 0 to n-1 do
                    let i = p*5+4
                    let a = get source i
                    alpha.[i] <- float32 (a + get alpha i * (1.0-a))
        let rec run (dest: float32[]) count complement wrap command (alphaOut: float32[] option) =
            let root = firstCommand
            firstCommand <- false
            let isGroup =
                match command with
                | ColorManagedGroup4 _ | ColorManagedGroup _ | TransparencyGroup _
                | Mask _ | Blend _ -> true
                | Group (_, opacity, _) when opacity < 1.0 -> true
                | _ -> false
            let b = if isGroup && (allowRootBounds || not root) then bounds [wrap command] else { X = 0; Y = 0; Width = width; Height = height }
            if b.X < 0 || b.Y < 0 || b.Width < 0 || b.Height < 0 ||
               b.X > width || b.Y > height || b.Width > width-b.X || b.Height > height-b.Y then
                invalidArg "bounds" "Incorrect bounds rectangle"
            if isGroup && (b.Width <> width || b.Height <> height) then
                if b.Width > 0 && b.Height > 0 then
                    let local = Array.zeroCreate<float32> (b.Width*b.Height*5)
                    for y = 0 to b.Height-1 do
                        Array.blit dest (((b.Y+y)*width+b.X)*5) local (y*b.Width*5) (b.Width*5)
                    let intersect x y w h =
                        let x0, y0 = min (b.X+b.Width) (max b.X x), min (b.Y+b.Height) (max b.Y y)
                        let x1, y1 = min (b.X+b.Width) (x+w), min (b.Y+b.Height) (y+h)
                        x0, y0, max 0 (x1-x0), max 0 (y1-y0)
                    let localRaster commands : RasterRect =
                        let r = raster (commands |> List.map wrap)
                        let x, y, w, h = intersect r.X r.Y r.Width r.Height
                        let data =
                            if x = r.X && y = r.Y && w = r.Width && h = r.Height then r.Pixels
                            else
                                let cropped = Array.zeroCreate<uint32> (w*h)
                                if w > 0 then
                                    for row = 0 to h-1 do
                                        Array.blit r.Pixels ((y-r.Y+row)*r.Width+x-r.X) cropped (row*w) w
                                cropped
                        { RasterRect.X = x-b.X; Y = y-b.Y; Width = w; Height = h; Pixels = data }
                    let localBounds commands : Bounds =
                        let r = bounds (commands |> List.map wrap)
                        let x, y, w, h = intersect r.X r.Y r.Width r.Height
                        { Bounds.X = x-b.X; Y = y-b.Y; Width = w; Height = h }
                    let localAlpha = alphaOut |> Option.map (fun _ -> Array.zeroCreate<float32> local.Length)
                    let result = renderCore b.Width b.Height local localRaster localBounds count complement command localAlpha false
                    for y = 0 to b.Height-1 do
                        Array.blit result (y*b.Width*5) dest (((b.Y+y)*width+b.X)*5) (b.Width*5)
                    match alphaOut, localAlpha with
                    | Some target, Some source ->
                        for y = 0 to b.Height-1 do
                            for x = 0 to b.Width-1 do
                                let si = (y*b.Width+x)*5+4
                                let di = ((b.Y+y)*width+b.X+x)*5+4
                                let a = get source si
                                target.[di] <- float32 (a + get target di * (1.0-a))
                    | _ -> ()
            else runUnbounded dest count complement wrap command alphaOut
        and runUnbounded (dest: float32[]) count complement wrap command alphaOut =
            match command with
            | ColorManagedGroup4 (conv, isolated, knockout, opacity, mode, mask, children) ->
                group dest count complement wrap conv.GroupComponents conv.Complement (Some conv.ToGroup) (Some conv.ToParent) isolated knockout opacity mode mask children alphaOut
            | ColorManagedGroup (conv, isolated, knockout, opacity, mode, mask, children) ->
                let to4 f x y z _ = let struct(a,b,c) = f x y z in struct(a,b,c,0.0)
                group dest count complement wrap 3 false (Some (to4 conv.ToGroup)) (Some (to4 conv.ToParent)) isolated knockout opacity mode mask children alphaOut
            | TransparencyGroup (isolated, knockout, opacity, mode, mask, children) ->
                group dest count complement wrap count complement None None isolated knockout opacity mode mask children alphaOut
            | Group (m, opacity, children) when opacity >= 1.0 ->
                for child in children do run dest count complement (fun c -> wrap (Group(m,1.0,[c]))) child alphaOut
            | Group (m, opacity, children) ->
                group dest count complement (fun c -> wrap (Group(m,1.0,[c]))) count complement None None true false opacity Normal None children alphaOut
            | Clip (path, rule, children) ->
                for child in children do run dest count complement (fun c -> wrap (Clip(path,rule,[c]))) child alphaOut
            | AntiAlias (samples, children) ->
                for child in children do run dest count complement (fun c -> wrap (AntiAlias(samples,[c]))) child alphaOut
            | Mask (mask, children) ->
                group dest count complement wrap count complement None None false false 1.0 Normal (Some mask) children alphaOut
            | Blend (mode, children) ->
                group dest count complement wrap count complement None None false false 1.0 mode None children alphaOut
            | WithShape (_, children) -> for child in children do run dest count complement wrap child alphaOut
            | SaveRestore children -> for child in children do run dest count complement wrap child alphaOut
            | _ ->
                let source = Array.zeroCreate<float32> (n*5)
                paint source count complement wrap command
                track alphaOut source
                composite dest source count complement Normal
        and paint (source: float32[]) count complement wrap command =
            match command with
            | Components4 (first, fourth) ->
                let a, k = rasterOne wrap first, rasterOne wrap fourth
                componentX <- width
                componentY <- height
                componentWidth <- -width
                componentHeight <- -height
                unpack source a false
                unpack source k true
            | WithShape (_, children) -> for child in children do run source count complement wrap child None
            | Blend (mode, children) ->
                for child in children do
                    let temp = Array.zeroCreate<float32> (n*5)
                    paint temp count complement wrap child
                    composite source temp count complement mode
            | ColorManagedGroup4 _ | ColorManagedGroup _ | TransparencyGroup _ | Group _ | Clip _ | AntiAlias _ | SaveRestore _ | Mask _ -> run source count complement wrap command None
            | _ ->
                let px = rasterOne wrap command
                unpack source px false
        and group (dest: float32[]) parentCount parentComplement wrap groupCount groupComplement toGroup toParent isolated knockout opacity mode mask children alphaOut =
            if opacity > 0.0 && not children.IsEmpty then
                let backdrop =
                    if isolated then Array.empty<float32>
                    else match toGroup with Some f -> convert dest parentCount f | None -> Array.copy dest
                let current = if isolated then Array.zeroCreate<float32> (n*5) else Array.copy backdrop
                let accumulated = Array.zeroCreate<float32> n
                let src = Array.zeroCreate<float32> (n*5)
                let mutable candidateScratch : float32[] option = None
                let scratch () =
                    match candidateScratch with
                    | Some value -> value
                    | None ->
                        let value = Array.zeroCreate<float32> (n*5)
                        candidateScratch <- Some value
                        value
                let rec needsBackdrop = function
                    | ColorManagedGroup4 _ | ColorManagedGroup _ | TransparencyGroup _ | Mask _ -> true
                    | WithShape (_, children) | Clip (_, _, children)
                    | AntiAlias (_, children) | SaveRestore children | Blend (_, children) ->
                        List.exists needsBackdrop children
                    | Group (_, opacity, children) when opacity >= 1.0 -> List.exists needsBackdrop children
                    | _ -> false
                let rec objects wrap command =
                    match command with
                    | Group (m, o, cs) when o >= 1.0 -> for c in cs do objects (fun x -> wrap (Group(m,1.0,[x]))) c
                    | Clip (p, r, cs) -> for c in cs do objects (fun x -> wrap (Clip(p,r,[x]))) c
                    | AntiAlias (s, cs) -> for c in cs do objects (fun x -> wrap (AntiAlias(s,[x]))) c
                    | SaveRestore cs -> for c in cs do objects wrap c
                    | _ ->
                        let rec stripBlend = function
                            | WithShape (_, [child]) -> stripBlend child
                            | Group (m, o, [child]) when o >= 1.0 ->
                                let mode, inner = stripBlend child
                                mode, Group (m, o, [inner])
                            | Clip (path, rule, [child]) ->
                                let mode, inner = stripBlend child
                                mode, Clip (path, rule, [inner])
                            | AntiAlias (samples, [child]) ->
                                let mode, inner = stripBlend child
                                mode, AntiAlias (samples, [inner])
                            | SaveRestore [child] ->
                                let mode, inner = stripBlend child
                                mode, SaveRestore [inner]
                            | Blend (m, [child]) -> m, child
                            | Blend (m, children) -> m, SaveRestore children
                            | other -> Normal, other
                        let objMode, obj = stripBlend command
                        let nested = needsBackdrop obj
                        let bounded =
                            not knockout && not nested && objMode = Normal &&
                            (match obj with Components4 _ -> true | _ -> false)
                        // A nested object returns only contribution alpha in src.
                        // Restrict the parent bookkeeping as well as the nested
                        // surface itself, otherwise each small group still scans
                        // the entire enclosing group.
                        let nestedBounds =
                            if nested && not knockout && objMode = Normal then
                                let b = bounds [wrap obj]
                                if b.X < 0 || b.Y < 0 || b.Width < 0 || b.Height < 0 ||
                                   b.X > width || b.Y > height || b.Width > width-b.X || b.Height > height-b.Y then
                                    invalidArg "bounds" "Incorrect bounds rectangle"
                                Some b
                            else None
                        if nested then
                            match nestedBounds with
                            | Some b ->
                                for y = b.Y to b.Y+b.Height-1 do
                                    for x = b.X to b.X+b.Width-1 do
                                        src.[(y*width+x)*5+4] <- 0.0f
                            | None -> for p = 0 to n-1 do src.[p*5+4] <- 0.0f
                        else
                            match obj with
                            | Components4 _ -> ()
                            | WithShape _ | Blend _ | ColorManagedGroup4 _ | ColorManagedGroup _
                            | TransparencyGroup _ | Group _ | Clip _ | AntiAlias _ | SaveRestore _ | Mask _ ->
                                Array.fill src 0 src.Length 0.0f
                            | _ -> ()
                            paint src groupCount groupComplement wrap obj
                        let shape = if knockout then Some (fullPixels (rasterOne wrap (Cmd.shapeOf command))) else None
                        // Nested groups return their actual contribution alpha
                        // while rendering against the candidate backdrop.
                        let candidate =
                            if knockout then
                                let value = scratch ()
                                if isolated then Array.fill value 0 value.Length 0.0f
                                else Array.blit backdrop 0 value 0 value.Length
                                value
                            else current
                        if nested && objMode = Normal then
                            run candidate groupCount groupComplement wrap obj (Some src)
                        elif nested then
                            // The nested group must see its actual backdrop even when
                            // an enclosing Blend changes its boundary mode.
                            let trial = if knockout then candidate else scratch ()
                            if not knockout then Array.blit current 0 trial 0 trial.Length
                            run trial groupCount groupComplement wrap obj (Some src)
                            for p = 0 to n-1 do
                                let i = p*5
                                let a = get src (i+4)
                                for c = 0 to groupCount-1 do
                                    let baseValue = if knockout then (if isolated then 0.0 else get backdrop (i+c)) else get current (i+c)
                                    put src (i+c) (get trial (i+c)-baseValue*(1.0-a))
                            if knockout then
                                if isolated then Array.fill candidate 0 candidate.Length 0.0f
                                else Array.blit backdrop 0 candidate 0 candidate.Length
                            composite candidate src groupCount groupComplement objMode
                        elif bounded then
                            for y = componentY to componentY+componentHeight-1 do
                                let first = y*width+componentX
                                compositeNormalRange candidate src groupCount first (first+componentWidth-1)
                        else composite candidate src groupCount groupComplement objMode
                        match shape with
                        | None ->
                            // With no knockout shape, a transparent source
                            // leaves the already-clamped accumulated alpha
                            // unchanged. Most object rasters are sparse.
                            if bounded then
                                for y = componentY to componentY+componentHeight-1 do
                                    for x = componentX to componentX+componentWidth-1 do
                                        let p = y*width+x
                                        let sa = float (pixelAt src (p*5+4))
                                        if sa <> 0.0 then
                                            let old = float accumulated.[p]
                                            accumulated.[p] <- float32 (clamp (sa+old*(1.0-sa)))
                            elif nestedBounds.IsSome then
                                let b = nestedBounds.Value
                                for y = b.Y to b.Y+b.Height-1 do
                                    for x = b.X to b.X+b.Width-1 do
                                        let p = y*width+x
                                        let sa = float (pixelAt src (p*5+4))
                                        if sa <> 0.0 then
                                            let old = float accumulated.[p]
                                            accumulated.[p] <- float32 (clamp (sa+old*(1.0-sa)))
                            else
                                for p = 0 to n-1 do
                                    let sa = float (pixelAt src (p*5+4))
                                    if sa <> 0.0 then
                                        let old = float accumulated.[p]
                                        accumulated.[p] <- float32 (clamp (sa+old*(1.0-sa)))
                        | Some sh ->
                            for p = 0 to n-1 do
                                let i = p*5
                                let sa = get src (i+4)
                                let inv = 1.0-alphaOf sh.[p]
                                for c = 0 to 4 do
                                    let b = if isolated then 0.0 else get backdrop (i+c)
                                    put current (i+c) (get candidate (i+c)+(get current (i+c)-b)*inv)
                                let old = float accumulated.[p]
                                accumulated.[p] <- float32 (clamp (sa+old*inv))
                        if bounded then clearComponent src
                        else
                            match nestedBounds with
                            | Some b ->
                                for y = b.Y to b.Y+b.Height-1 do
                                    Array.fill src ((y*width+b.X)*5) (b.Width*5) 0.0f
                            | None -> Array.fill src 0 src.Length 0.0f
                for child in children do objects wrap child
                let contribution = current
                for p = 0 to n-1 do
                    let i, a = p*5, float accumulated.[p]
                    if not isolated then
                        for c = 0 to groupCount-1 do
                            put contribution (i+c) (get current (i+c) - get backdrop (i+c)*(1.0-a))
                    put contribution (i+4) a
                let contribution = match toParent with Some f -> convert contribution groupCount f | None -> contribution
                let maskPixels =
                    mask |> Option.map (fun cs ->
                        let surface = Array.zeroCreate<float32> (n*5)
                        for child in cs do run surface groupCount groupComplement wrap child None
                        surface)
                if opacity < 1.0 || Option.isSome maskPixels then
                    for p = 0 to n-1 do
                        let factor = clamp opacity * (match maskPixels with Some a -> get a (p*5+4) | None -> 1.0)
                        let i = p*5
                        for c = 0 to 4 do contribution.[i+c] <- contribution.[i+c] * float32 factor
                track alphaOut contribution
                composite dest contribution parentCount parentComplement mode
        run surface count complement id command alphaOut
        surface

    /// Composite a component-managed group over premultiplied BGRA pixels.
    let render width height (pixels: uint32[]) raster bounds command =
        match command with
        | ColorManagedGroup4 _ -> ()
        | _ -> invalidArg "command" "Expected a component-managed group"
        let surface = renderCore width height (fromPixels pixels) raster bounds 3 false command None true
        for p = 0 to pixels.Length-1 do
            let i = p*5
            pixels.[p] <- Pixel.pack (toByte surface (i+2)) (toByte surface (i+1)) (toByte surface i) (toByte surface (i+4))

    /// Render into a transparent four-component surface. Both returned BGRA
    /// planes are premultiplied and share alpha; the second is grayscale K.
    let renderComponents width height raster bounds complement commands =
        let pixels = Array.zeroCreate<uint32> (width*height)
        let command = TransparencyGroup (true, false, 1.0, Normal, None, commands)
        let surface = renderCore width height (fromPixels pixels) raster bounds 4 complement command None true
        let fourth = Array.zeroCreate<uint32> pixels.Length
        for p = 0 to pixels.Length-1 do
            let i = p*5
            let a, k = toByte surface (i+4), toByte surface (i+3)
            pixels.[p] <- Pixel.pack (toByte surface (i+2)) (toByte surface (i+1)) (toByte surface i) a
            fourth.[p] <- Pixel.pack k k k a
        pixels, fourth
