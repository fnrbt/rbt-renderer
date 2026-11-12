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
            let ctx = Rbt.Image.Codecs.DecodeContext()
#if FABLE_COMPILER
            let pixels = source.Px
#else
            let pixels: System.ReadOnlyMemory<uint32> = source.Px
#endif
            let result = Rbt.Image.Codecs.Resample.downscaleRegion ctx pixels source.W source.H sx sy sw sh w h
            Framebuffer.ofArray result w h, rect 0.0 0.0 (float w) (float h)
