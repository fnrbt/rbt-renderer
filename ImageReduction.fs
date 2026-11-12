namespace Rbt.Renderer

/// Device-dependent box prefilter for images. Pixels are already premultiplied;
/// averaging all four channels independently preserves transparent edges.
module ImageReduction =
    let prepare (source: Framebuffer) (src: Rect) (dst: Rect) (transform: Matrix3x2) =
        let sx, sy = float src.Pos.X, float src.Pos.Y
        let sw, sh = float src.Size.X, float src.Size.Y
        let dw, dh = float dst.Size.X, float dst.Size.Y
        // Perpendicular projected lengths account for shear as well as rotation;
        // nearly parallel axes should not masquerade as magnification.
        let xAxis = sqrt (transform.M11 * transform.M11 + transform.M12 * transform.M12)
        let yAxis = sqrt (transform.M21 * transform.M21 + transform.M22 * transform.M22)
        let determinant = abs (transform.M11 * transform.M22 - transform.M12 * transform.M21)
        let xLength = if yAxis > 0.0 then abs dw * determinant / yAxis else 0.0
        let yLength = if xAxis > 0.0 then abs dh * determinant / xAxis else 0.0
        if source.W <= 0 || source.H <= 0 || sw <= 0.0 || sh <= 0.0 || xLength <= 0.0 || yLength <= 0.0 ||
           (xLength >= sw && yLength >= sh) then source, src
        else
            // Never upsample here: magnification is the backend's sampling policy.
            let w = max 1 (int (ceil (min sw xLength)))
            let h = max 1 (int (ceil (min sh yLength)))
            let result = Array.zeroCreate<uint32> (w * h)
#if FABLE_COMPILER
            let pixels = source.Px
#else
            let pixels = source.Px.Span
#endif
            let inline clamp lo hi v = max lo (min hi v)
            for y = 0 to h - 1 do
                let y0 = sy + float y * sh / float h
                let y1 = sy + float (y + 1) * sh / float h
                for x = 0 to w - 1 do
                    let x0 = sx + float x * sw / float w
                    let x1 = sx + float (x + 1) * sw / float w
                    let mutable b, g, r, a = 0.0, 0.0, 0.0, 0.0
                    for iy = int (floor y0) to int (ceil y1) - 1 do
                        let wy = min y1 (float (iy + 1)) - max y0 (float iy)
                        for ix = int (floor x0) to int (ceil x1) - 1 do
                            let weight = wy * (min x1 (float (ix + 1)) - max x0 (float ix))
                            let p = pixels.[clamp 0 (source.H - 1) iy * source.W + clamp 0 (source.W - 1) ix]
                            b <- b + float (Pixel.b p) * weight
                            g <- g + float (Pixel.g p) * weight
                            r <- r + float (Pixel.r p) * weight
                            a <- a + float (Pixel.a p) * weight
                    let area = (x1 - x0) * (y1 - y0)
                    let inline channel v = byte (clamp 0.0 255.0 (floor (v / area + 0.5)))
                    result.[y * w + x] <- Pixel.pack (channel b) (channel g) (channel r) (channel a)
            Framebuffer.ofArray result w h, rect 0.0 0.0 (float w) (float h)
