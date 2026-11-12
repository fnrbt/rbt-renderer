namespace Rbt.Renderer

/// Component-space transparency. Surfaces store premultiplied straight-space
/// components followed by alpha, in a Fable-compatible Float32Array.
module ComponentCompositor =
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

    // src and dst are premultiplied; complement only the inputs to the blend
    // function, never the storage or the color-space conversion callbacks.
    let private composite (dst: float32[]) (src: float32[]) count complement mode =
        if mode = Normal then
            for p = 0 to dst.Length / 5 - 1 do
                let i = p*5
                let sa = src.[i+4]
                if sa > 0.0f then
                    let inv = 1.0f-sa
                    for c = 0 to count-1 do
                        dst.[i+c] <- src.[i+c] + dst.[i+c]*inv
                    dst.[i+4] <- sa + dst.[i+4]*inv
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

    let private renderCore (width: int) (height: int) (pixels: uint32[]) (raster: Cmd list -> uint32[]) count complement command =
        let n = width*height
        if pixels.Length <> n then invalidArg "pixels" "Incorrect surface size"
        let rasterOne wrap cmd =
            let result = raster [wrap cmd]
            if result.Length <> n then invalidArg "raster" "Incorrect surface size"
            result
        let alphaOf (v:uint32) = float (Pixel.a v)/255.0
        let track (target: float32[] option) (source: float32[]) =
            match target with
            | None -> ()
            | Some alpha ->
                for p = 0 to n-1 do
                    let i = p*5+4
                    let a = get source i
                    alpha.[i] <- float32 (a + get alpha i * (1.0-a))
        let rec run (dest: float32[]) count complement wrap command alphaOut =
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
                for p = 0 to n-1 do
                    let i, v = p*5, a.[p]
                    source.[i] <- float32 (Pixel.r v)/255.0f
                    source.[i+1] <- float32 (Pixel.g v)/255.0f
                    source.[i+2] <- float32 (Pixel.b v)/255.0f
                    source.[i+3] <- float32 (Pixel.r k.[p])/255.0f
                    source.[i+4] <- float32 (Pixel.a v)/255.0f
            | WithShape (_, children) -> for child in children do run source count complement wrap child None
            | Blend (mode, children) ->
                for child in children do
                    let temp = Array.zeroCreate<float32> (n*5)
                    paint temp count complement wrap child
                    composite source temp count complement mode
            | ColorManagedGroup4 _ | ColorManagedGroup _ | TransparencyGroup _ | Group _ | Clip _ | AntiAlias _ | SaveRestore _ | Mask _ -> run source count complement wrap command None
            | _ ->
                let px = rasterOne wrap command
                for p = 0 to n-1 do
                    let i, v = p*5, px.[p]
                    source.[i] <- float32 (Pixel.r v)/255.0f
                    source.[i+1] <- float32 (Pixel.g v)/255.0f
                    source.[i+2] <- float32 (Pixel.b v)/255.0f
                    source.[i+3] <- 0.0f
                    source.[i+4] <- float32 (Pixel.a v)/255.0f
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
                        if nested then
                            for p = 0 to n-1 do src.[p*5+4] <- 0.0f
                        else
                            match obj with
                            | Components4 _ -> ()
                            | WithShape _ | Blend _ | ColorManagedGroup4 _ | ColorManagedGroup _
                            | TransparencyGroup _ | Group _ | Clip _ | AntiAlias _ | SaveRestore _ | Mask _ ->
                                Array.fill src 0 src.Length 0.0f
                            | _ -> ()
                            paint src groupCount groupComplement wrap obj
                        let shape = if knockout then Some (rasterOne wrap (Cmd.shapeOf command)) else None
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
                        else composite candidate src groupCount groupComplement objMode
                        for p = 0 to n-1 do
                            let i = p*5
                            let sa = get src (i+4)
                            match shape with
                            | None -> ()
                            | Some sh ->
                                let inv = 1.0-alphaOf sh.[p]
                                for c = 0 to 4 do
                                    let b = if isolated then 0.0 else get backdrop (i+c)
                                    put current (i+c) (get candidate (i+c)+(get current (i+c)-b)*inv)
                            let old = float accumulated.[p]
                            let inv = match shape with Some sh -> 1.0-alphaOf sh.[p] | None -> 1.0-sa
                            accumulated.[p] <- float32 (clamp (sa+old*inv))
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
        let surface = fromPixels pixels
        run surface count complement id command None
        surface

    /// Composite a component-managed group over premultiplied BGRA pixels.
    let render width height (pixels: uint32[]) raster command =
        match command with
        | ColorManagedGroup4 _ -> ()
        | _ -> invalidArg "command" "Expected a component-managed group"
        let surface = renderCore width height pixels raster 3 false command
        for p = 0 to pixels.Length-1 do
            let i = p*5
            pixels.[p] <- Pixel.pack (toByte surface (i+2)) (toByte surface (i+1)) (toByte surface i) (toByte surface (i+4))

    /// Render into a transparent four-component surface. Both returned BGRA
    /// planes are premultiplied and share alpha; the second is grayscale K.
    let renderComponents width height raster complement commands =
        let pixels = Array.zeroCreate<uint32> (width*height)
        let command = TransparencyGroup (true, false, 1.0, Normal, None, commands)
        let surface = renderCore width height pixels raster 4 complement command
        let fourth = Array.zeroCreate<uint32> pixels.Length
        for p = 0 to pixels.Length-1 do
            let i = p*5
            let a, k = toByte surface (i+4), toByte surface (i+3)
            pixels.[p] <- Pixel.pack (toByte surface (i+2)) (toByte surface (i+1)) (toByte surface i) a
            fourth.[p] <- Pixel.pack k k k a
        pixels, fourth
