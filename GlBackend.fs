namespace Rbt.Renderer

open System
open System.Runtime.InteropServices

module GlBackend =

    let vertSrc = """
attribute vec2 aPos;
uniform mat4 uMVP;
void main() {
    gl_Position = uMVP * vec4(aPos, 0.0, 1.0);
}
"""

    let fragSrc = """
precision highp float;
uniform vec4 uColor;
void main() {
    gl_FragColor = uColor;
}
"""

    type OffscreenTarget = { Fbo: int; Texture: int; StencilRb: int; Width: int; Height: int }

    type FboPool = { mutable Available: OffscreenTarget list; W: int; H: int }

    type GlState =
        { mutable W: int; mutable H: int
          mutable Egl: Egl.EglContext
          mutable Program: int
          mutable LocPos: int; mutable LocMVP: int; mutable LocColor: int
          mutable Vbo: int
          mutable Fbo: int; mutable ColorRb: int; mutable StencilRb: int
          mutable FboPool: FboPool
          mutable ActiveFbo: int
          mutable TextAtlasTex: int
          mutable TexCache: System.Collections.Generic.Dictionary<uint32[], int> }

    let ortho (w: float32) (h: float32) : float32[] =
        // Column-major orthographic projection: (0,0) top-left, (w,h) bottom-right, z=[-1,1]
        [| 2.0f/w; 0.0f; 0.0f; 0.0f
           0.0f; (-2.0f)/h; 0.0f; 0.0f
           0.0f; 0.0f; -1.0f; 0.0f
           -1.0f; 1.0f; 0.0f; 1.0f |]

    let transformToMat4 (m: Matrix3x2) (baseOrtho: float32[]) : float32[] =
        // Multiply: ortho * [m11 m12 0 0; m21 m22 0 0; 0 0 1 0; dx dy 0 1] (column-major)
        let a = Array.copy baseOrtho
        let m11 = float32 m.M11
        let m12 = float32 m.M12
        let m21 = float32 m.M21
        let m22 = float32 m.M22
        let dx  = float32 m.DX
        let dy  = float32 m.DY
        // Result = ortho * T, computed column by column
        let o = baseOrtho
        // col 0
        a.[0]  <- o.[0]*m11 + o.[4]*m12
        a.[1]  <- o.[1]*m11 + o.[5]*m12
        a.[2]  <- 0.0f
        a.[3]  <- 0.0f
        // col 1
        a.[4]  <- o.[0]*m21 + o.[4]*m22
        a.[5]  <- o.[1]*m21 + o.[5]*m22
        a.[6]  <- 0.0f
        a.[7]  <- 0.0f
        // col 2
        a.[8]  <- 0.0f
        a.[9]  <- 0.0f
        a.[10] <- -1.0f
        a.[11] <- 0.0f
        // col 3
        a.[12] <- o.[0]*dx + o.[4]*dy + o.[12]
        a.[13] <- o.[1]*dx + o.[5]*dy + o.[13]
        a.[14] <- 0.0f
        a.[15] <- 1.0f
        a

    let uploadVerts (st: GlState) (verts: float32[]) =
        Gl.glBindBuffer(Gl.GL_ARRAY_BUFFER, st.Vbo)
        let h = GCHandle.Alloc(verts, GCHandleType.Pinned)
        try Gl.glBufferData(Gl.GL_ARRAY_BUFFER, nativeint (verts.Length * 4), h.AddrOfPinnedObject(), Gl.GL_DYNAMIC_DRAW)
        finally h.Free()
        Gl.glVertexAttribPointer(st.LocPos, 2, Gl.GL_FLOAT, false, 0, nativeint 0)

    let setColor (st: GlState) (c: Color) =
        let r = float32 c.R / 255.0f
        let g = float32 c.G / 255.0f
        let b = float32 c.B / 255.0f
        let a = float32 c.A / 255.0f
        // Premultiply for GL blending
        Gl.glUniform4f(st.LocColor, r*a, g*a, b*a, a)

    let setMVP (st: GlState) (mvp: float32[]) =
        Gl.glUniformMatrix4fv(st.LocMVP, 1, false, mvp)

    let drawTriangles (st: GlState) (verts: float32[]) =
        uploadVerts st verts
        Gl.glDrawArrays(Gl.GL_TRIANGLES, 0, verts.Length / 2)

    let drawTriangleFan (st: GlState) (verts: float32[]) =
        uploadVerts st verts
        Gl.glDrawArrays(Gl.GL_TRIANGLE_FAN, 0, verts.Length / 2)

    let drawLines (st: GlState) (verts: float32[]) =
        uploadVerts st verts
        Gl.glDrawArrays(Gl.GL_LINES, 0, verts.Length / 2)

    let drawLineStrip (st: GlState) (verts: float32[]) =
        uploadVerts st verts
        Gl.glDrawArrays(Gl.GL_LINE_STRIP, 0, verts.Length / 2)

    let drawLineLoop (st: GlState) (verts: float32[]) =
        uploadVerts st verts
        Gl.glDrawArrays(Gl.GL_LINE_LOOP, 0, verts.Length / 2)

    let rectVerts (x: float) (y: float) (w: float) (h: float) : float32[] =
        let x0, y0, x1, y1 = float32 x, float32 y, float32 (x+w), float32 (y+h)
        [| x0;y0; x1;y0; x1;y1; x0;y0; x1;y1; x0;y1 |]

    let circleVerts (cx: float) (cy: float) (r: float) (n: int) : float32[] =
        let arr = Array.zeroCreate<float32> ((n + 2) * 2)
        arr.[0] <- float32 cx; arr.[1] <- float32 cy
        for i = 0 to n do
            let a = float i * 2.0 * Math.PI / float n
            arr.[(i+1)*2]   <- float32 (cx + r * cos a)
            arr.[(i+1)*2+1] <- float32 (cy + r * sin a)
        arr

    let contourBBox (contours: (float * float) list list) =
        let mutable minX, minY = Double.MaxValue, Double.MaxValue
        let mutable maxX, maxY = Double.MinValue, Double.MinValue
        for c in contours do
            for (x, y) in c do
                if x < minX then minX <- x
                if y < minY then minY <- y
                if x > maxX then maxX <- x
                if y > maxY then maxY <- y
        (minX, minY, maxX, maxY)

    // Stencil-then-cover path fill
    let stencilFillContours (st: GlState) (contours: (float * float) list list) (fillRule: FillRule) (c: Color) (mvp: float32[]) =
        if contours.IsEmpty then () else
        setMVP st mvp

        // Phase 1: write stencil
        Gl.glEnable(Gl.GL_STENCIL_TEST)
        Gl.glColorMask(false, false, false, false)
        Gl.glStencilMask(0xFFu)
        Gl.glClear(Gl.GL_STENCIL_BUFFER_BIT)

        match fillRule with
        | EvenOdd ->
            Gl.glStencilFunc(Gl.GL_ALWAYS, 0, 0xFF)
            Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_INVERT)
        | NonZero ->
            Gl.glStencilFunc(Gl.GL_ALWAYS, 0, 0xFF)
            Gl.glStencilOpSeparate(Gl.GL_FRONT, Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_INCR_WRAP)
            Gl.glStencilOpSeparate(Gl.GL_BACK, Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_DECR_WRAP)

        // Draw triangle fans for each contour
        for contour in contours do
            if contour.Length >= 3 then
                let verts = Array.zeroCreate<float32> (contour.Length * 2)
                let mutable i = 0
                for (x, y) in contour do
                    verts.[i] <- float32 x; verts.[i+1] <- float32 y
                    i <- i + 2
                drawTriangleFan st verts

        // Phase 2: cover (draw bounding box where stencil != 0)
        Gl.glColorMask(true, true, true, true)
        Gl.glStencilFunc(Gl.GL_NOTEQUAL, 0, 0xFF)
        Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_ZERO)

        setColor st c
        let (minX, minY, maxX, maxY) = contourBBox contours
        let coverVerts = rectVerts (minX - 1.0) (minY - 1.0) (maxX - minX + 2.0) (maxY - minY + 2.0)
        drawTriangles st coverVerts

        Gl.glDisable(Gl.GL_STENCIL_TEST)

    // Stencil-then-cover with a gradient shader for the cover pass
    let stencilFillContoursGradient (st: GlState) (contours: (float * float) list list) (fillRule: FillRule) (shader: GlslCompiler.CompiledShader) (mvp: float32[]) =
        if contours.IsEmpty then () else

        // Phase 1: write stencil using the solid-color program (color mask is off anyway)
        Gl.glUseProgram(st.Program)
        setMVP st mvp
        Gl.glEnable(Gl.GL_STENCIL_TEST)
        Gl.glColorMask(false, false, false, false)
        Gl.glStencilMask(0xFFu)
        Gl.glClear(Gl.GL_STENCIL_BUFFER_BIT)

        match fillRule with
        | EvenOdd ->
            Gl.glStencilFunc(Gl.GL_ALWAYS, 0, 0xFF)
            Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_INVERT)
        | NonZero ->
            Gl.glStencilFunc(Gl.GL_ALWAYS, 0, 0xFF)
            Gl.glStencilOpSeparate(Gl.GL_FRONT, Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_INCR_WRAP)
            Gl.glStencilOpSeparate(Gl.GL_BACK, Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_DECR_WRAP)

        for contour in contours do
            if contour.Length >= 3 then
                let verts = Array.zeroCreate<float32> (contour.Length * 2)
                let mutable i = 0
                for (x, y) in contour do
                    verts.[i] <- float32 x; verts.[i+1] <- float32 y
                    i <- i + 2
                drawTriangleFan st verts

        // Phase 2: cover with gradient shader
        Gl.glUseProgram(shader.Program)
        Gl.glUniformMatrix4fv(shader.LocMVP, 1, false, mvp)
        // Rebind vertex attribute for gradient program
        Gl.glEnableVertexAttribArray(shader.LocPos)

        Gl.glColorMask(true, true, true, true)
        Gl.glStencilFunc(Gl.GL_NOTEQUAL, 0, 0xFF)
        Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_ZERO)

        let (minX, minY, maxX, maxY) = contourBBox contours
        let coverVerts = rectVerts (minX - 1.0) (minY - 1.0) (maxX - minX + 2.0) (maxY - minY + 2.0)
        uploadVerts st coverVerts
        Gl.glVertexAttribPointer(shader.LocPos, 2, Gl.GL_FLOAT, false, 0, nativeint 0)
        Gl.glDrawArrays(Gl.GL_TRIANGLES, 0, coverVerts.Length / 2)

        // Restore solid-color program
        Gl.glUseProgram(st.Program)
        Gl.glEnableVertexAttribArray(st.LocPos)
        Gl.glVertexAttribPointer(st.LocPos, 2, Gl.GL_FLOAT, false, 0, nativeint 0)

        Gl.glDisable(Gl.GL_STENCIL_TEST)

    // Stencil-then-cover with image paint shader for the cover pass
    let stencilFillContoursImage (st: GlState) (contours: (float * float) list list) (fillRule: FillRule) (shader: GlslCompiler.ImagePaintShader) (tex: int) (invXform: Matrix3x2) (texW: int) (texH: int) (mvp: float32[]) =
        if contours.IsEmpty then () else

        // Phase 1: write stencil using the solid-color program
        Gl.glUseProgram(st.Program)
        setMVP st mvp
        Gl.glEnable(Gl.GL_STENCIL_TEST)
        Gl.glColorMask(false, false, false, false)
        Gl.glStencilMask(0xFFu)
        Gl.glClear(Gl.GL_STENCIL_BUFFER_BIT)

        match fillRule with
        | EvenOdd ->
            Gl.glStencilFunc(Gl.GL_ALWAYS, 0, 0xFF)
            Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_INVERT)
        | NonZero ->
            Gl.glStencilFunc(Gl.GL_ALWAYS, 0, 0xFF)
            Gl.glStencilOpSeparate(Gl.GL_FRONT, Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_INCR_WRAP)
            Gl.glStencilOpSeparate(Gl.GL_BACK, Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_DECR_WRAP)

        for contour in contours do
            if contour.Length >= 3 then
                let verts = Array.zeroCreate<float32> (contour.Length * 2)
                let mutable i = 0
                for (x, y) in contour do
                    verts.[i] <- float32 x; verts.[i+1] <- float32 y
                    i <- i + 2
                drawTriangleFan st verts

        // Phase 2: cover with image paint shader
        Gl.glUseProgram(shader.Program)
        Gl.glUniformMatrix4fv(shader.LocMVP, 1, false, mvp)
        Gl.glActiveTexture(Gl.GL_TEXTURE0)
        Gl.glBindTexture(Gl.GL_TEXTURE_2D, tex)
        if shader.LocTex >= 0 then Gl.glUniform1i(shader.LocTex, 0)
        // Pack inverse Matrix3x2 as mat4 (column-major)
        let invArr =
            [| float32 invXform.M11; float32 invXform.M12; 0.0f; 0.0f
               float32 invXform.M21; float32 invXform.M22; 0.0f; 0.0f
               0.0f; 0.0f; 1.0f; 0.0f
               float32 invXform.DX;  float32 invXform.DY;  0.0f; 1.0f |]
        if shader.LocInvXform >= 0 then Gl.glUniformMatrix4fv(shader.LocInvXform, 1, false, invArr)
        if shader.LocTexSize >= 0 then Gl.glUniform2f(shader.LocTexSize, float32 texW, float32 texH)
        Gl.glEnableVertexAttribArray(shader.LocPos)

        Gl.glColorMask(true, true, true, true)
        Gl.glStencilFunc(Gl.GL_NOTEQUAL, 0, 0xFF)
        Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_ZERO)

        let (minX, minY, maxX, maxY) = contourBBox contours
        let coverVerts = rectVerts (minX - 1.0) (minY - 1.0) (maxX - minX + 2.0) (maxY - minY + 2.0)
        uploadVerts st coverVerts
        Gl.glVertexAttribPointer(shader.LocPos, 2, Gl.GL_FLOAT, false, 0, nativeint 0)
        Gl.glDrawArrays(Gl.GL_TRIANGLES, 0, coverVerts.Length / 2)

        // Restore solid-color program
        Gl.glUseProgram(st.Program)
        Gl.glEnableVertexAttribArray(st.LocPos)
        Gl.glVertexAttribPointer(st.LocPos, 2, Gl.GL_FLOAT, false, 0, nativeint 0)
        Gl.glDisable(Gl.GL_STENCIL_TEST)

    // Stroke via line strips (GL_LINE_STRIP per contour, with width)
    let strokeContours (st: GlState) (contours: (float * float) list list) (width: float) (c: Color) (mvp: float32[]) =
        setMVP st mvp
        setColor st c
        Gl.glLineWidth(float32 (max 1.0 width))
        for contour in contours do
            let verts = Array.zeroCreate<float32> (contour.Length * 2)
            let mutable i = 0
            for (x, y) in contour do
                verts.[i] <- float32 x; verts.[i+1] <- float32 y
                i <- i + 2
            drawLineStrip st verts

    // Get a solid color from Paint, or None for complex paints
    let solidColor (paint: Paint) =
        match paint with
        | Solid c -> Some c
        | _ -> None

    // ========== Offscreen FBO infrastructure ==========

    let createOffscreenTarget (w: int) (h: int) : OffscreenTarget =
        let mutable fbo = 0
        Gl.glGenFramebuffers(1, &fbo)
        Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, fbo)
        // Create texture for color attachment
        let mutable tex = 0
        Gl.glGenTextures(1, &tex)
        Gl.glBindTexture(Gl.GL_TEXTURE_2D, tex)
        Gl.glTexImage2D(Gl.GL_TEXTURE_2D, 0, Gl.GL_RGBA, w, h, 0, Gl.GL_RGBA, Gl.GL_UNSIGNED_BYTE, nativeint 0)
        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_MIN_FILTER, Gl.GL_LINEAR)
        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_MAG_FILTER, Gl.GL_LINEAR)
        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_WRAP_S, Gl.GL_CLAMP_TO_EDGE)
        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_WRAP_T, Gl.GL_CLAMP_TO_EDGE)
        Gl.glFramebufferTexture2D(Gl.GL_FRAMEBUFFER, Gl.GL_COLOR_ATTACHMENT0, Gl.GL_TEXTURE_2D, tex, 0)
        // Create stencil renderbuffer
        let mutable stencilRb = 0
        Gl.glGenRenderbuffers(1, &stencilRb)
        Gl.glBindRenderbuffer(Gl.GL_RENDERBUFFER, stencilRb)
        Gl.glRenderbufferStorage(Gl.GL_RENDERBUFFER, Gl.GL_STENCIL_INDEX8, w, h)
        Gl.glFramebufferRenderbuffer(Gl.GL_FRAMEBUFFER, Gl.GL_STENCIL_ATTACHMENT, Gl.GL_RENDERBUFFER, stencilRb)
        let status = Gl.glCheckFramebufferStatus(Gl.GL_FRAMEBUFFER)
        if status <> Gl.GL_FRAMEBUFFER_COMPLETE then
            eprintfn "Offscreen FBO incomplete: 0x%X" status
        Gl.glBindTexture(Gl.GL_TEXTURE_2D, 0)
        { Fbo = fbo; Texture = tex; StencilRb = stencilRb; Width = w; Height = h }

    let destroyOffscreenTarget (t: OffscreenTarget) =
        let mutable fbo = t.Fbo
        let mutable tex = t.Texture
        let mutable rb = t.StencilRb
        Gl.glDeleteFramebuffers(1, &fbo)
        Gl.glDeleteTextures(1, &tex)
        Gl.glDeleteRenderbuffers(1, &rb)

    let createPool w h : FboPool = { Available = []; W = w; H = h }

    let acquireFbo (pool: FboPool) =
        match pool.Available with
        | fbo :: rest -> pool.Available <- rest; fbo
        | [] -> createOffscreenTarget pool.W pool.H

    let releaseFbo (pool: FboPool) (fbo: OffscreenTarget) =
        Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, fbo.Fbo)
        Gl.glClearColor(0.0f, 0.0f, 0.0f, 0.0f)
        Gl.glClear(Gl.GL_COLOR_BUFFER_BIT ||| Gl.GL_STENCIL_BUFFER_BIT)
        pool.Available <- fbo :: pool.Available

    let destroyPool (pool: FboPool) =
        pool.Available |> List.iter destroyOffscreenTarget
        pool.Available <- []

    let deleteTexture (tex: int) =
        let mutable t = tex
        Gl.glDeleteTextures(1, &t)

    let fullscreenQuadVerts = [| -1.0f; -1.0f; 1.0f; -1.0f; 1.0f; 1.0f; -1.0f; -1.0f; 1.0f; 1.0f; -1.0f; 1.0f |]

    let drawFullscreenQuad (shader: GlslCompiler.PostProcessShader) (tex0: int) (tex1: int) =
        Gl.glUseProgram(shader.Program)
        // Bind texture 0
        Gl.glActiveTexture(Gl.GL_TEXTURE0)
        Gl.glBindTexture(Gl.GL_TEXTURE_2D, tex0)
        if shader.LocTex0 >= 0 then Gl.glUniform1i(shader.LocTex0, 0)
        // Bind texture 1
        Gl.glActiveTexture(Gl.GL_TEXTURE1)
        Gl.glBindTexture(Gl.GL_TEXTURE_2D, tex1)
        if shader.LocTex1 >= 0 then Gl.glUniform1i(shader.LocTex1, 1)
        Gl.glActiveTexture(Gl.GL_TEXTURE0)
        // Unbind VBO so glVertexAttribPointer uses client-side pointer, not VBO offset
        Gl.glBindBuffer(Gl.GL_ARRAY_BUFFER, 0)
        // Draw fullscreen quad using pinned array
        let h = GCHandle.Alloc(fullscreenQuadVerts, GCHandleType.Pinned)
        try
            Gl.glEnableVertexAttribArray(shader.LocPos)
            Gl.glVertexAttribPointer(shader.LocPos, 2, Gl.GL_FLOAT, false, 0, h.AddrOfPinnedObject())
            Gl.glDrawArrays(Gl.GL_TRIANGLES, 0, 6)
        finally h.Free()

    let restoreMainState (st: GlState) =
        Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, st.ActiveFbo)
        Gl.glViewport(0, 0, st.W, st.H)
        Gl.glUseProgram(st.Program)
        Gl.glBindBuffer(Gl.GL_ARRAY_BUFFER, st.Vbo)
        Gl.glEnableVertexAttribArray(st.LocPos)
        Gl.glVertexAttribPointer(st.LocPos, 2, Gl.GL_FLOAT, false, 0, nativeint 0)
        Gl.glEnable(Gl.GL_BLEND)
        Gl.glBlendFunc(Gl.GL_ONE, Gl.GL_ONE_MINUS_SRC_ALPHA)
        Gl.glActiveTexture(Gl.GL_TEXTURE0)
        Gl.glBindTexture(Gl.GL_TEXTURE_2D, 0)
        Gl.glActiveTexture(Gl.GL_TEXTURE1)
        Gl.glBindTexture(Gl.GL_TEXTURE_2D, 0)
        Gl.glActiveTexture(Gl.GL_TEXTURE0)

    let uploadTexture (fb: Framebuffer) : int =
        let mutable tex = 0
        Gl.glGenTextures(1, &tex)
        Gl.glBindTexture(Gl.GL_TEXTURE_2D, tex)
        // Upload premultiplied RGBA pixels (fb.Px is packed BGRA uint32)
        let spx = fb.Px.Span
        let bytes = Array.zeroCreate<byte> (fb.W * fb.H * 4)
        for i = 0 to fb.Px.Length - 1 do
            let px = spx.[i]
            bytes.[i*4] <- Pixel.r px
            bytes.[i*4+1] <- Pixel.g px
            bytes.[i*4+2] <- Pixel.b px
            bytes.[i*4+3] <- Pixel.a px
        let h = GCHandle.Alloc(bytes, GCHandleType.Pinned)
        try Gl.glTexImage2D(Gl.GL_TEXTURE_2D, 0, Gl.GL_RGBA, fb.W, fb.H, 0, Gl.GL_RGBA, Gl.GL_UNSIGNED_BYTE, h.AddrOfPinnedObject())
        finally h.Free()
        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_MIN_FILTER, Gl.GL_LINEAR)
        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_MAG_FILTER, Gl.GL_LINEAR)
        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_WRAP_S, Gl.GL_CLAMP_TO_EDGE)
        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_WRAP_T, Gl.GL_CLAMP_TO_EDGE)
        Gl.glBindTexture(Gl.GL_TEXTURE_2D, 0)
        tex

    let getOrUploadTexture (st: GlState) (fb: Framebuffer) : int =
        // Use underlying array as cache key (image paint framebuffers are always array-backed)
        let cacheKey =
            match Framebuffer.tryGetArray fb with
            | Some arr -> arr
            | None -> null
        if cacheKey <> null then
            match st.TexCache.TryGetValue(cacheKey) with
            | true, tex -> tex
            | _ ->
                let tex = uploadTexture fb
                st.TexCache.[cacheKey] <- tex
                tex
        else uploadTexture fb

    let flushTexCache (st: GlState) =
        for kvp in st.TexCache do
            let mutable t = kvp.Value
            Gl.glDeleteTextures(1, &t)
        st.TexCache.Clear()

    let captureCurrentFbToTexture (w: int) (h: int) : int =
        let mutable tex = 0
        Gl.glGenTextures(1, &tex)
        Gl.glBindTexture(Gl.GL_TEXTURE_2D, tex)
        Gl.glCopyTexImage2D(Gl.GL_TEXTURE_2D, 0, Gl.GL_RGBA, 0, 0, w, h, 0)
        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_MIN_FILTER, Gl.GL_LINEAR)
        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_MAG_FILTER, Gl.GL_LINEAR)
        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_WRAP_S, Gl.GL_CLAMP_TO_EDGE)
        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_WRAP_T, Gl.GL_CLAMP_TO_EDGE)
        Gl.glBindTexture(Gl.GL_TEXTURE_2D, 0)
        tex

    /// Mutable flag set when softwareFallback writes pixels, cleared each frame by Init.
    /// Used by persistent GL backend's Finish to skip expensive softFb compositing when empty.
    let mutable softFbHasPixels = false

    // Software rasterizer fallback: render command to softFb, composited in Finish
    let softwareFallback (st: GlState) (cmd: Cmd) (m: Matrix3x2) (softFb: Framebuffer) =
        let tmpFb = Framebuffer.create softFb.W softFb.H { R = 0uy; G = 0uy; B = 0uy; A = 0uy }
        Raster.dispatchWithTransformAA tmpFb m 1 cmd
        let tpx = tmpFb.Px.Span
        let fpx = softFb.Px.Span
        for i = 0 to softFb.Px.Length - 1 do
            let s = tpx.[i]
            if Pixel.a s > 0uy then
                fpx.[i] <- Raster.blendPx s fpx.[i]
                softFbHasPixels <- true

    let spreadToWrapMode (spread: SpreadMode) =
        match spread with
        | Repeat -> Gl.GL_REPEAT
        | Reflect -> Gl.GL_MIRRORED_REPEAT
        | Pad -> Gl.GL_CLAMP_TO_EDGE

    // Stencil-then-cover with tiling paint shader (GL wrap modes handle tiling)
    let stencilFillContoursTiling (st: GlState) (contours: (float * float) list list) (fillRule: FillRule) (shader: GlslCompiler.ImagePaintShader) (tex: int) (invXform: Matrix3x2) (texW: int) (texH: int) (tileX: SpreadMode) (tileY: SpreadMode) (mvp: float32[]) =
        if contours.IsEmpty then () else

        // Phase 1: write stencil using the solid-color program
        Gl.glUseProgram(st.Program)
        setMVP st mvp
        Gl.glEnable(Gl.GL_STENCIL_TEST)
        Gl.glColorMask(false, false, false, false)
        Gl.glStencilMask(0xFFu)
        Gl.glClear(Gl.GL_STENCIL_BUFFER_BIT)

        match fillRule with
        | EvenOdd ->
            Gl.glStencilFunc(Gl.GL_ALWAYS, 0, 0xFF)
            Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_INVERT)
        | NonZero ->
            Gl.glStencilFunc(Gl.GL_ALWAYS, 0, 0xFF)
            Gl.glStencilOpSeparate(Gl.GL_FRONT, Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_INCR_WRAP)
            Gl.glStencilOpSeparate(Gl.GL_BACK, Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_DECR_WRAP)

        for contour in contours do
            if contour.Length >= 3 then
                let verts = Array.zeroCreate<float32> (contour.Length * 2)
                let mutable i = 0
                for (x, y) in contour do
                    verts.[i] <- float32 x; verts.[i+1] <- float32 y
                    i <- i + 2
                drawTriangleFan st verts

        // Phase 2: cover with tiling paint shader
        Gl.glUseProgram(shader.Program)
        Gl.glUniformMatrix4fv(shader.LocMVP, 1, false, mvp)
        Gl.glActiveTexture(Gl.GL_TEXTURE0)
        Gl.glBindTexture(Gl.GL_TEXTURE_2D, tex)
        // Set wrap modes per spread
        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_WRAP_S, spreadToWrapMode tileX)
        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_WRAP_T, spreadToWrapMode tileY)
        if shader.LocTex >= 0 then Gl.glUniform1i(shader.LocTex, 0)
        let invArr =
            [| float32 invXform.M11; float32 invXform.M12; 0.0f; 0.0f
               float32 invXform.M21; float32 invXform.M22; 0.0f; 0.0f
               0.0f; 0.0f; 1.0f; 0.0f
               float32 invXform.DX;  float32 invXform.DY;  0.0f; 1.0f |]
        if shader.LocInvXform >= 0 then Gl.glUniformMatrix4fv(shader.LocInvXform, 1, false, invArr)
        if shader.LocTexSize >= 0 then Gl.glUniform2f(shader.LocTexSize, float32 texW, float32 texH)
        Gl.glEnableVertexAttribArray(shader.LocPos)

        Gl.glColorMask(true, true, true, true)
        Gl.glStencilFunc(Gl.GL_NOTEQUAL, 0, 0xFF)
        Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_ZERO)

        let (minX, minY, maxX, maxY) = contourBBox contours
        let coverVerts = rectVerts (minX - 1.0) (minY - 1.0) (maxX - minX + 2.0) (maxY - minY + 2.0)
        uploadVerts st coverVerts
        Gl.glVertexAttribPointer(shader.LocPos, 2, Gl.GL_FLOAT, false, 0, nativeint 0)
        Gl.glDrawArrays(Gl.GL_TRIANGLES, 0, coverVerts.Length / 2)

        // Restore wrap modes and solid-color program
        Gl.glBindTexture(Gl.GL_TEXTURE_2D, tex)
        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_WRAP_S, Gl.GL_CLAMP_TO_EDGE)
        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_WRAP_T, Gl.GL_CLAMP_TO_EDGE)
        Gl.glUseProgram(st.Program)
        Gl.glEnableVertexAttribArray(st.LocPos)
        Gl.glVertexAttribPointer(st.LocPos, 2, Gl.GL_FLOAT, false, 0, nativeint 0)
        Gl.glDisable(Gl.GL_STENCIL_TEST)

    /// Unified paint dispatch for stencil-then-cover. Returns true if handled.
    let stencilFillContoursWithPaint (st: GlState) (contours: (float * float) list list) (fillRule: FillRule) (paint: Paint) (mvp: float32[]) : bool =
        match paint with
        | Solid c ->
            stencilFillContours st contours fillRule c mvp
            true
        | ImagePaint (src, xform) ->
            let tex = getOrUploadTexture st src
            let invXform = Matrix3x2.inverse xform
            let shader = GlslCompiler.buildImagePaintShader ()
            stencilFillContoursImage st contours fillRule shader tex invXform src.W src.H mvp
            true
        | TilingPaint (src, xform, tileX, tileY) ->
            let tex = getOrUploadTexture st src
            let invXform = Matrix3x2.inverse xform
            let shader = GlslCompiler.buildTilingPaintShader ()
            stencilFillContoursTiling st contours fillRule shader tex invXform src.W src.H tileX tileY mvp
            true
        | NoPaint -> true
        | _ ->
            match GlslCompiler.buildGradientShader paint with
            | Some shader ->
                stencilFillContoursGradient st contours fillRule shader mvp
                true
            | None -> false

    // ── Bitmap glyph atlas for pixel-perfect small text on GPU ──

    type private BitmapGlyphEntry = {
        U0: float32; V0: float32; U1: float32; V1: float32
        BearingX: int; BearingY: int; W: int; H: int
    }

    type private BitmapGlyphAtlas = {
        mutable Pixels: byte[]
        mutable AtlasW: int; mutable AtlasH: int
        mutable CursorX: int; mutable CursorY: int; mutable RowH: int
        Entries: Collections.Generic.Dictionary<int * int * int * int * int, BitmapGlyphEntry>
        mutable Dirty: bool
        mutable TexId: int
    }

    let private bitmapAtlas : BitmapGlyphAtlas =
        let w, h = 512, 512
        { Pixels = Array.zeroCreate (w * h)
          AtlasW = w; AtlasH = h
          CursorX = 0; CursorY = 0; RowH = 0
          Entries = Collections.Generic.Dictionary()
          Dirty = true; TexId = 0 }

    /// Drop the bitmap glyph atlas's GL texture so it's re-created + re-uploaded in a fresh context.
    /// The CPU-side glyph entries/pixels stay valid (only the GL texture id is per-context); marking
    /// Dirty forces the next DrawGlyphs to re-upload. Without this, the 2nd GL context in a process
    /// binds a texture id from the destroyed 1st context → garbage glyphs + aliased-texture artifacts.
    let private resetBitmapAtlasGl () =
        bitmapAtlas.TexId <- 0
        bitmapAtlas.Dirty <- true

    let private ensureBitmapGlyph (font: Text.FontData) (glyphId: int) (ppem: int) (subX: int) (subY: int) : BitmapGlyphEntry option =
        let fontHash = Text.fontId font
        let key = (fontHash, glyphId, ppem, subX, subY)
        match bitmapAtlas.Entries.TryGetValue(key) with
        | true, e -> Some e
        | _ ->
            match Raster.rasterizeGlyphBitmap font glyphId ppem subX subY 4 with
            | None -> None
            | Some bmp ->
                let atlas = bitmapAtlas
                let gw, gh = bmp.W, bmp.H
                if gw <= 0 || gh <= 0 then None
                else
                    // Shelf packing
                    if atlas.CursorX + gw > atlas.AtlasW then
                        atlas.CursorX <- 0
                        atlas.CursorY <- atlas.CursorY + atlas.RowH
                        atlas.RowH <- 0
                    if atlas.CursorY + gh > atlas.AtlasH then
                        let newH = atlas.AtlasH * 2
                        let newPixels = Array.zeroCreate (atlas.AtlasW * newH)
                        Array.Copy(atlas.Pixels, newPixels, atlas.Pixels.Length)
                        atlas.Pixels <- newPixels
                        atlas.AtlasH <- newH
                    let ax, ay = atlas.CursorX, atlas.CursorY
                    for row = 0 to gh - 1 do
                        Array.Copy(bmp.Alpha, row * gw, atlas.Pixels, (ay + row) * atlas.AtlasW + ax, gw)
                    let entry = {
                        U0 = float32 ax / float32 atlas.AtlasW
                        V0 = float32 ay / float32 atlas.AtlasH
                        U1 = float32 (ax + gw) / float32 atlas.AtlasW
                        V1 = float32 (ay + gh) / float32 atlas.AtlasH
                        BearingX = bmp.BearingX; BearingY = bmp.BearingY
                        W = gw; H = gh
                    }
                    atlas.Entries.[key] <- entry
                    atlas.CursorX <- atlas.CursorX + gw
                    atlas.RowH <- max atlas.RowH gh
                    atlas.Dirty <- true
                    Some entry

    /// Read an offscreen object into premultiplied software pixels. Group
    /// accumulation uses the same per-pixel equations as the raster backend;
    /// drawing the objects themselves still uses the GL command dispatcher.
    let private readTransparencyPixels (fb: Framebuffer) (bytes: byte[]) =
        let handle = GCHandle.Alloc(bytes, GCHandleType.Pinned)
        try Gl.glReadPixels(0, 0, fb.W, fb.H, Gl.GL_RGBA, Gl.GL_UNSIGNED_BYTE, handle.AddrOfPinnedObject())
        finally handle.Free()
        let pixels = fb.Px.Span
        for y = 0 to fb.H - 1 do
            let row = (fb.H - 1 - y) * fb.W * 4
            for x = 0 to fb.W - 1 do
                let offset = row + x * 4
                pixels.[y * fb.W + x] <- Pixel.pack bytes.[offset + 2] bytes.[offset + 1] bytes.[offset] bytes.[offset + 3]
    let private uploadTransparencyPixels (fb: Framebuffer) (bytes: byte[]) =
        let pixels = fb.Px.Span
        for y = 0 to fb.H - 1 do
            let row = (fb.H - 1 - y) * fb.W * 4
            for x = 0 to fb.W - 1 do
                let px = pixels.[y * fb.W + x]
                let offset = row + x * 4
                bytes.[offset] <- Pixel.r px
                bytes.[offset + 1] <- Pixel.g px
                bytes.[offset + 2] <- Pixel.b px
                bytes.[offset + 3] <- Pixel.a px
        let mutable texture = 0
        Gl.glGenTextures(1, &texture)
        Gl.glBindTexture(Gl.GL_TEXTURE_2D, texture)
        let handle = GCHandle.Alloc(bytes, GCHandleType.Pinned)
        try Gl.glTexImage2D(Gl.GL_TEXTURE_2D, 0, Gl.GL_RGBA, fb.W, fb.H, 0, Gl.GL_RGBA, Gl.GL_UNSIGNED_BYTE, handle.AddrOfPinnedObject())
        finally handle.Free()
        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_MIN_FILTER, Gl.GL_NEAREST)
        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_MAG_FILTER, Gl.GL_NEAREST)
        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_WRAP_S, Gl.GL_CLAMP_TO_EDGE)
        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_WRAP_T, Gl.GL_CLAMP_TO_EDGE)
        texture


    let rec renderToOffscreen (st: GlState) (orthoBase: float32[]) (m: Matrix3x2) (softFb: Framebuffer) (cmds: Cmd list) (w: int) (h: int) : OffscreenTarget =
        let savedFbo = st.ActiveFbo
        let target = acquireFbo st.FboPool
        st.ActiveFbo <- target.Fbo
        Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, target.Fbo)
        Gl.glViewport(0, 0, w, h)
        Gl.glClearColor(0.0f, 0.0f, 0.0f, 0.0f)
        Gl.glClear(Gl.GL_COLOR_BUFFER_BIT ||| Gl.GL_STENCIL_BUFFER_BIT)
        // Re-setup state for rendering into offscreen
        Gl.glUseProgram(st.Program)
        Gl.glBindBuffer(Gl.GL_ARRAY_BUFFER, st.Vbo)
        Gl.glEnableVertexAttribArray(st.LocPos)
        Gl.glVertexAttribPointer(st.LocPos, 2, Gl.GL_FLOAT, false, 0, nativeint 0)
        cmds |> List.iter (dispatch st orthoBase m softFb)
        st.ActiveFbo <- savedFbo
        target

    and dispatch (st: GlState) (orthoBase: float32[]) (m: Matrix3x2) (softFb: Framebuffer) (cmd: Cmd) =
        let mvp = transformToMat4 m orthoBase
        match cmd with
        | RetainedGlesLayer (bounds, layer) ->
            let frame =
                { Bounds = bounds
                  Transform = m
                  Projection = mvp
                  ViewportWidth = st.W
                  ViewportHeight = st.H
                  Framebuffer = st.ActiveFbo
                  ContextId = st.Egl.Context }
            try layer.Render frame
            finally
                Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, st.ActiveFbo)
                Gl.glViewport(0, 0, st.W, st.H)
                restoreMainState st
        | Clear c ->
            let pm = Color.toPremul c
            Gl.glClearColor(float32 pm.R / 255.0f, float32 pm.G / 255.0f, float32 pm.B / 255.0f, float32 pm.A / 255.0f)
            Gl.glClear(Gl.GL_COLOR_BUFFER_BIT)
            // Keep softFb transparent - only software fallback commands write to it

        | FillRect (r, c) ->
            setMVP st mvp
            setColor st c
            let verts = rectVerts (float r.Pos.X) (float r.Pos.Y) (float r.Size.X) (float r.Size.Y)
            drawTriangles st verts

        | ShaderFill (r, expr) ->
            let shader = GlslCompiler.buildShaderFill expr
            Gl.glUseProgram(shader.Program)
            Gl.glUniformMatrix4fv(shader.LocMVP, 1, false, mvp)
            Gl.glEnableVertexAttribArray(shader.LocPos)
            let verts = rectVerts (float r.Pos.X) (float r.Pos.Y) (float r.Size.X) (float r.Size.Y)
            uploadVerts st verts
            Gl.glVertexAttribPointer(shader.LocPos, 2, Gl.GL_FLOAT, false, 0, nativeint 0)
            Gl.glDrawArrays(Gl.GL_TRIANGLES, 0, verts.Length / 2)
            // Restore solid-color program
            Gl.glUseProgram(st.Program)
            Gl.glEnableVertexAttribArray(st.LocPos)
            Gl.glVertexAttribPointer(st.LocPos, 2, Gl.GL_FLOAT, false, 0, nativeint 0)

        | StrokeRect (r, c, w) ->
            setMVP st mvp
            setColor st c
            Gl.glLineWidth(float32 (max 1.0 (float w)))
            let x0, y0 = float32 (float r.Pos.X), float32 (float r.Pos.Y)
            let x1, y1 = x0 + float32 (float r.Size.X), y0 + float32 (float r.Size.Y)
            drawLineLoop st [| x0;y0; x1;y0; x1;y1; x0;y1 |]

        | FillCircle (center, radius, c) ->
            // Use polygon path + stencil fill to match software rasterizer's circleToPath
            let n = 64
            let step = 2.0 * Math.PI / float n
            let cx, cy, r = float center.X, float center.Y, float radius
            let segs =
                [ yield MoveTo (vec2 (cx + r) cy)
                  for i = 1 to n - 1 do
                      let a = float i * step
                      yield LineTo (vec2 (cx + r * cos a) (cy + r * sin a))
                  yield Close ]
            let contours = Raster.flattenPath segs
            stencilFillContours st contours EvenOdd c mvp

        | FillTri (a, b, c, col) ->
            setMVP st mvp
            setColor st col
            let verts = [| float32 (float a.X); float32 (float a.Y)
                           float32 (float b.X); float32 (float b.Y)
                           float32 (float c.X); float32 (float c.Y) |]
            drawTriangles st verts

        | Line (a, b, c, w) ->
            setMVP st mvp
            setColor st c
            Gl.glLineWidth(float32 (max 1.0 (float w)))
            let verts = [| float32 (float a.X); float32 (float a.Y)
                           float32 (float b.X); float32 (float b.Y) |]
            drawLines st verts

        | FillPath (segs, paint, fillRule) ->
            let contours = Raster.flattenPath segs
            if not (stencilFillContoursWithPaint st contours fillRule paint mvp) then
                softwareFallback st cmd m softFb

        | StrokePath (segs, style) ->
            let contours = Raster.flattenStrokePath segs
            let allOutline = ResizeArray<PathSeg>()
            for contour, closed in contours do
                let dashed = Raster.strokeDashes contour closed (style.DashArray |> List.map float) (float style.DashOffset)
                for sub, subClosed in dashed do allOutline.AddRange(Raster.expandStroke sub subClosed style)
            let outlineContours = Raster.flattenPath (Seq.toList allOutline)
            if not (stencilFillContoursWithPaint st outlineContours EvenOdd style.Paint mvp) then
                softwareFallback st cmd m softFb

        | Group (xform, opacity, children) ->
            if opacity < 1.0 / 255.0 then () else
            let combined = Matrix3x2.multiply xform m
            if opacity >= 1.0 - 1e-6 then
                children |> List.iter (dispatch st orthoBase combined softFb)
            else
                // Render children to offscreen, then composite with opacity shader
                let target = renderToOffscreen st orthoBase combined softFb children st.W st.H
                Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, st.ActiveFbo)
                Gl.glViewport(0, 0, st.W, st.H)
                let opacityShader = GlslCompiler.buildOpacityShader opacity
                drawFullscreenQuad opacityShader target.Texture 0
                releaseFbo st.FboPool target
                restoreMainState st
        | TransparencyGroup (isolated, knockout, opacity, mode, maskCmds, children) ->
            if opacity > 0.0 && not children.IsEmpty then
                // Keep the unmodified parent backdrop throughout object composition.
                // In particular, a nonisolated group cannot be rendered on transparent
                // and subsequently blended onto the parent's changed framebuffer.
                Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, st.ActiveFbo)
                let pixels = Array.zeroCreate<byte> (st.W * st.H * 4)
                let backdrop = Framebuffer.create st.W st.H Color.black
                readTransparencyPixels backdrop pixels
                let clipActive = Gl.glIsEnabled(Gl.GL_STENCIL_TEST) <> 0uy
                // A preceding software-fallback draw is logically part of this
                // group's initial backdrop, even though the GL backend normally
                // defers those pixels until Finish. Flush it before processing
                // objects so ordering and nonisolation remain correct.
                if softFbHasPixels then
                    let pending = softFb.Px.Span
                    let backdropPx = backdrop.Px.Span
                    let mutable anyPending = false
                    for i = 0 to softFb.Px.Length - 1 do
                        if Pixel.a pending.[i] > 0uy then
                            backdropPx.[i] <- Raster.blendPx pending.[i] backdropPx.[i]
                            anyPending <- true
                    if anyPending then
                        pending.Clear()
                        Gl.glDisable(Gl.GL_STENCIL_TEST)
                        Gl.glDisable(Gl.GL_BLEND)
                        let texture = uploadTransparencyPixels backdrop pixels
                        drawFullscreenQuad (GlslCompiler.buildPassthroughShader ()) texture 0
                        deleteTexture texture
                        restoreMainState st
                    softFbHasPixels <- false

                let group = Framebuffer.create st.W st.H (if isolated then Color.rgba 0uy 0uy 0uy 0uy else Color.black)
                if not isolated then backdrop.Px.Span.CopyTo(group.Px.Span)
                let alpha = Framebuffer.create st.W st.H (Color.rgba 0uy 0uy 0uy 0uy)
                let source = Framebuffer.create st.W st.H (Color.rgba 0uy 0uy 0uy 0uy)
                let shape =
                    if knockout then Some (Framebuffer.create st.W st.H (Color.rgba 0uy 0uy 0uy 0uy))
                    else None
                let scratch = Framebuffer.create st.W st.H (Color.rgba 0uy 0uy 0uy 0uy)
                // The parent clip stencil belongs to the parent FBO; offscreen
                // children must begin with an independent, empty stencil.
                Gl.glDisable(Gl.GL_STENCIL_TEST)
                let renderObject commands destination =
                    scratch.Px.Span.Clear()
                    let target = renderToOffscreen st orthoBase m scratch commands st.W st.H
                    Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, target.Fbo)
                    readTransparencyPixels destination pixels
                    let dest = destination.Px.Span
                    let fallback = scratch.Px.Span
                    for i = 0 to destination.Px.Length - 1 do
                        if Pixel.a fallback.[i] > 0uy then
                            dest.[i] <- Raster.blendPx fallback.[i] dest.[i]
                    releaseFbo st.FboPool target
                    Gl.glDisable(Gl.GL_STENCIL_TEST)
                    restoreMainState st
                for child in children do
                    for obj in Raster.transparencyObjects child do
                        let sourceCmd, objectMode = Raster.transparencyObjectBlend obj
                        renderObject [sourceCmd] source
                        match shape with
                        | Some shapeFb -> renderObject [Raster.transparencyShapeCmd obj] shapeFb
                        | None -> ()
                        Raster.accumulateTransparencyObject group alpha backdrop source shape isolated objectMode
                let mask =
                    match maskCmds with
                    | Some commands ->
                        let maskFb = Framebuffer.create st.W st.H (Color.rgba 0uy 0uy 0uy 0uy)
                        // Mask commands form a single transparency image.
                        renderObject commands maskFb
                        Some maskFb
                    | None -> None
                Raster.finishTransparencyGroup backdrop group alpha mask isolated opacity mode
                // Replace the parent with its original backdrop plus exactly one
                // group composite (not one composite per child).
                Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, st.ActiveFbo)
                if clipActive then
                    Gl.glEnable(Gl.GL_STENCIL_TEST)
                    Gl.glStencilFunc(Gl.GL_NOTEQUAL, 0, 0xFF)
                    Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_KEEP)
                let texture = uploadTransparencyPixels backdrop pixels
                Gl.glDisable(Gl.GL_BLEND)
                drawFullscreenQuad (GlslCompiler.buildPassthroughShader ()) texture 0
                deleteTexture texture
                restoreMainState st


        | FillRoundedRect (r, cr, c) ->
            // Build path with ArcTo segments, flatten, stencil fill
            let x0, y0 = float r.Pos.X, float r.Pos.Y
            let w, h = float r.Size.X, float r.Size.Y
            let cr = float cr |> min (w/2.0) |> min (h/2.0) |> max 0.0
            let segs =
                [ MoveTo (vec2 (x0 + cr) y0)
                  LineTo (vec2 (x0 + w - cr) y0)
                  ArcTo (cr * 1.0<px>, cr * 1.0<px>, 0.0, false, true, vec2 (x0 + w) (y0 + cr))
                  LineTo (vec2 (x0 + w) (y0 + h - cr))
                  ArcTo (cr * 1.0<px>, cr * 1.0<px>, 0.0, false, true, vec2 (x0 + w - cr) (y0 + h))
                  LineTo (vec2 (x0 + cr) (y0 + h))
                  ArcTo (cr * 1.0<px>, cr * 1.0<px>, 0.0, false, true, vec2 x0 (y0 + h - cr))
                  LineTo (vec2 x0 (y0 + cr))
                  ArcTo (cr * 1.0<px>, cr * 1.0<px>, 0.0, false, true, vec2 (x0 + cr) y0)
                  Close ]
            let contours = Raster.flattenPath segs
            stencilFillContours st contours EvenOdd c mvp

        | StrokeRoundedRect (r, cr, style) ->
            let x0, y0 = float r.Pos.X, float r.Pos.Y
            let w, h = float r.Size.X, float r.Size.Y
            let cr = float cr |> min (w/2.0) |> min (h/2.0) |> max 0.0
            let segs =
                [ MoveTo (vec2 (x0 + cr) y0)
                  LineTo (vec2 (x0 + w - cr) y0)
                  ArcTo (cr * 1.0<px>, cr * 1.0<px>, 0.0, false, true, vec2 (x0 + w) (y0 + cr))
                  LineTo (vec2 (x0 + w) (y0 + h - cr))
                  ArcTo (cr * 1.0<px>, cr * 1.0<px>, 0.0, false, true, vec2 (x0 + w - cr) (y0 + h))
                  LineTo (vec2 (x0 + cr) (y0 + h))
                  ArcTo (cr * 1.0<px>, cr * 1.0<px>, 0.0, false, true, vec2 x0 (y0 + h - cr))
                  LineTo (vec2 x0 (y0 + cr))
                  ArcTo (cr * 1.0<px>, cr * 1.0<px>, 0.0, false, true, vec2 (x0 + cr) y0)
                  Close ]
            let contours = Raster.flattenStrokePath segs
            let allOutline = ResizeArray<PathSeg>()
            for contour, closed in contours do
                let dashed = Raster.strokeDashes contour closed (style.DashArray |> List.map float) (float style.DashOffset)
                for sub, subClosed in dashed do allOutline.AddRange(Raster.expandStroke sub subClosed style)
            let outlineContours = Raster.flattenPath (Seq.toList allOutline)
            if not (stencilFillContoursWithPaint st outlineContours EvenOdd style.Paint mvp) then
                softwareFallback st cmd m softFb

        | FillRoundedRect4 (r, tl, tr, br, bl, c) ->
            let segs = Raster.roundedRectPath4 r tl tr br bl
            let contours = Raster.flattenPath segs
            stencilFillContours st contours EvenOdd c mvp

        | StrokeRoundedRect4 (r, tl, tr, br, bl, style) ->
            let segs = Raster.roundedRectPath4 r tl tr br bl
            let contours = Raster.flattenStrokePath segs
            let allOutline = ResizeArray<PathSeg>()
            for contour, closed in contours do
                let dashed = Raster.strokeDashes contour closed (style.DashArray |> List.map float) (float style.DashOffset)
                for sub, subClosed in dashed do allOutline.AddRange(Raster.expandStroke sub subClosed style)
            let outlineContours = Raster.flattenPath (Seq.toList allOutline)
            if not (stencilFillContoursWithPaint st outlineContours EvenOdd style.Paint mvp) then
                softwareFallback st cmd m softFb

        | DrawImage (src, srcRect, dstRect, smooth) ->
            let tex = getOrUploadTexture st src
            Gl.glBindTexture(Gl.GL_TEXTURE_2D, tex)
            let filter = if smooth then Gl.GL_LINEAR else Gl.GL_NEAREST
            Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_MIN_FILTER, filter)
            Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_MAG_FILTER, filter)
            // Compute UV coords from srcRect (normalized to source dimensions)
            let u0 = float32 (float srcRect.Pos.X / float src.W)
            let v0 = float32 (float srcRect.Pos.Y / float src.H)
            let u1 = float32 ((float srcRect.Pos.X + float srcRect.Size.X) / float src.W)
            let v1 = float32 ((float srcRect.Pos.Y + float srcRect.Size.Y) / float src.H)
            // Vertex positions from dstRect
            let x0 = float32 (float dstRect.Pos.X)
            let y0 = float32 (float dstRect.Pos.Y)
            let x1 = float32 (float dstRect.Pos.X + float dstRect.Size.X)
            let y1 = float32 (float dstRect.Pos.Y + float dstRect.Size.Y)
            let quadVerts = [| x0;y0;u0;v0; x1;y0;u1;v0; x1;y1;u1;v1; x0;y0;u0;v0; x1;y1;u1;v1; x0;y1;u0;v1 |]
            let texShader = GlslCompiler.buildTexturedQuadShader ()
            Gl.glUseProgram(texShader.Program)
            Gl.glUniformMatrix4fv(texShader.LocMVP, 1, false, mvp)
            Gl.glActiveTexture(Gl.GL_TEXTURE0)
            Gl.glBindTexture(Gl.GL_TEXTURE_2D, tex)
            if texShader.LocTex0 >= 0 then Gl.glUniform1i(texShader.LocTex0, 0)
            if texShader.LocOpacity >= 0 then Gl.glUniform1f(texShader.LocOpacity, 1.0f)
            Gl.glBindBuffer(Gl.GL_ARRAY_BUFFER, st.Vbo)
            let handle = GCHandle.Alloc(quadVerts, GCHandleType.Pinned)
            try Gl.glBufferData(Gl.GL_ARRAY_BUFFER, nativeint (quadVerts.Length * 4), handle.AddrOfPinnedObject(), Gl.GL_DYNAMIC_DRAW)
            finally handle.Free()
            Gl.glEnableVertexAttribArray(texShader.LocPos)
            if texShader.LocUV >= 0 then Gl.glEnableVertexAttribArray(texShader.LocUV)
            Gl.glVertexAttribPointer(texShader.LocPos, 2, Gl.GL_FLOAT, false, 16, nativeint 0)
            if texShader.LocUV >= 0 then Gl.glVertexAttribPointer(texShader.LocUV, 2, Gl.GL_FLOAT, false, 16, nativeint 8)
            Gl.glDrawArrays(Gl.GL_TRIANGLES, 0, 6)
            if texShader.LocUV >= 0 then Gl.glDisableVertexAttribArray(texShader.LocUV)
            restoreMainState st

        | BoxShadow (r, offsetX, offsetY, blurRadius, spread, color, inset, cornerRadius) ->
            let spreadF = float spread
            // Compute shadow rect (expand/contract by spread, pre-apply offset)
            let shadowRect =
                if inset then
                    rect (float r.Pos.X + spreadF) (float r.Pos.Y + spreadF) (float r.Size.X - 2.0 * spreadF) (float r.Size.Y - 2.0 * spreadF)
                else
                    rect (float r.Pos.X - spreadF + offsetX) (float r.Pos.Y - spreadF + offsetY) (float r.Size.X + 2.0 * spreadF) (float r.Size.Y + 2.0 * spreadF)
            let shadowCr =
                match cornerRadius with
                | Some cr ->
                    let adj = if inset then float cr - spreadF else float cr + spreadF
                    max 0.0 adj
                | None -> 0.0
            // Build shadow shape path
            let shadowSegs =
                if shadowCr > 0.0 then Raster.roundedRectPath shadowRect (shadowCr * 1.0<px>)
                else
                    let sx0 = float shadowRect.Pos.X
                    let sy0 = float shadowRect.Pos.Y
                    let sw = float shadowRect.Size.X
                    let sh = float shadowRect.Size.Y
                    [ MoveTo (vec2 sx0 sy0); LineTo (vec2 (sx0 + sw) sy0); LineTo (vec2 (sx0 + sw) (sy0 + sh)); LineTo (vec2 sx0 (sy0 + sh)); Close ]
            let shadowContours = Raster.flattenPath shadowSegs
            // Step 1: Render solid color fill to offscreen FBO
            let savedFbo = st.ActiveFbo
            let shadowTarget = acquireFbo st.FboPool
            st.ActiveFbo <- shadowTarget.Fbo
            Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, shadowTarget.Fbo)
            Gl.glViewport(0, 0, st.W, st.H)
            Gl.glClearColor(0.0f, 0.0f, 0.0f, 0.0f)
            Gl.glClear(Gl.GL_COLOR_BUFFER_BIT ||| Gl.GL_STENCIL_BUFFER_BIT)
            Gl.glUseProgram(st.Program)
            Gl.glBindBuffer(Gl.GL_ARRAY_BUFFER, st.Vbo)
            Gl.glEnableVertexAttribArray(st.LocPos)
            Gl.glVertexAttribPointer(st.LocPos, 2, Gl.GL_FLOAT, false, 0, nativeint 0)
            // For inset, apply offset to the shadow shape via transform
            let shadowMvp =
                if inset then
                    let offsetM = { Matrix3x2.identity with DX = offsetX; DY = offsetY }
                    let combined = Matrix3x2.multiply offsetM m
                    transformToMat4 combined orthoBase
                else
                    mvp
            stencilFillContours st shadowContours EvenOdd color shadowMvp
            // Step 2: Apply 6-pass separable blur
            let mutable currentTarget = shadowTarget
            let mutable tempTargets = [shadowTarget]
            if blurRadius > 0 then
                let hShader = GlslCompiler.buildHBlurShader blurRadius st.W
                let vShader = GlslCompiler.buildVBlurShader blurRadius st.H
                for _ = 1 to 3 do
                    let hT = acquireFbo st.FboPool
                    Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, hT.Fbo)
                    Gl.glViewport(0, 0, st.W, st.H)
                    Gl.glDisable(Gl.GL_BLEND)
                    drawFullscreenQuad hShader currentTarget.Texture 0
                    Gl.glEnable(Gl.GL_BLEND)
                    currentTarget <- hT
                    tempTargets <- hT :: tempTargets
                    let vT = acquireFbo st.FboPool
                    Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, vT.Fbo)
                    Gl.glViewport(0, 0, st.W, st.H)
                    Gl.glDisable(Gl.GL_BLEND)
                    drawFullscreenQuad vShader currentTarget.Texture 0
                    Gl.glEnable(Gl.GL_BLEND)
                    currentTarget <- vT
                    tempTargets <- vT :: tempTargets
            // Step 3: Stencil-mask with original rect path
            st.ActiveFbo <- savedFbo
            Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, savedFbo)
            // Build original rect path for clip mask
            let origSegs =
                match cornerRadius with
                | Some cr -> Raster.roundedRectPath r cr
                | None ->
                    let ox0 = float r.Pos.X
                    let oy0 = float r.Pos.Y
                    let ow = float r.Size.X
                    let oh = float r.Size.Y
                    [ MoveTo (vec2 ox0 oy0); LineTo (vec2 (ox0 + ow) oy0); LineTo (vec2 (ox0 + ow) (oy0 + oh)); LineTo (vec2 ox0 (oy0 + oh)); Close ]
            let origContours = Raster.flattenPath origSegs
            // Transform to screen space for stencil
            let transformedOrigContours =
                origContours |> List.map (fun c ->
                    c |> List.map (fun (x, y) ->
                        let tx = m.M11 * x + m.M21 * y + m.DX
                        let ty = m.M12 * x + m.M22 * y + m.DY
                        (tx, ty)))
            // Restore main FBO for compositing
            Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, st.ActiveFbo)
            Gl.glViewport(0, 0, st.W, st.H)
            Gl.glUseProgram(st.Program)
            Gl.glBindBuffer(Gl.GL_ARRAY_BUFFER, st.Vbo)
            Gl.glEnableVertexAttribArray(st.LocPos)
            Gl.glVertexAttribPointer(st.LocPos, 2, Gl.GL_FLOAT, false, 0, nativeint 0)
            // Write original rect to stencil
            let identityMvp = orthoBase
            setMVP st identityMvp
            Gl.glEnable(Gl.GL_STENCIL_TEST)
            Gl.glColorMask(false, false, false, false)
            Gl.glStencilMask(0xFFu)
            Gl.glClear(Gl.GL_STENCIL_BUFFER_BIT)
            Gl.glStencilFunc(Gl.GL_ALWAYS, 0, 0xFF)
            Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_INVERT)
            for contour in transformedOrigContours do
                if contour.Length >= 3 then
                    let verts = Array.zeroCreate<float32> (contour.Length * 2)
                    let mutable i = 0
                    for (x, y) in contour do verts.[i] <- float32 x; verts.[i+1] <- float32 y; i <- i + 2
                    drawTriangleFan st verts
            // Step 4: Composite blurred shadow where stencil test passes
            Gl.glColorMask(true, true, true, true)
            // Outset: draw outside original (stencil == 0), Inset: draw inside (stencil != 0)
            if inset then
                Gl.glStencilFunc(Gl.GL_NOTEQUAL, 0, 0xFF)
            else
                Gl.glStencilFunc(Gl.GL_EQUAL, 0, 0xFF)
            Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_ZERO)
            let passthrough = GlslCompiler.buildPassthroughShader ()
            drawFullscreenQuad passthrough currentTarget.Texture 0
            Gl.glDisable(Gl.GL_STENCIL_TEST)
            // Cleanup
            for t in tempTargets do releaseFbo st.FboPool t
            restoreMainState st

        | DrawBorder (r, top, right, bottom, left, tl, tr, br, bl) ->
            // For solid borders, render natively via stencil. Others fall back to software.
            let allSolid =
                (top.Style = NoBorder || top.Style = BorderSolid) &&
                (right.Style = NoBorder || right.Style = BorderSolid) &&
                (bottom.Style = NoBorder || bottom.Style = BorderSolid) &&
                (left.Style = NoBorder || left.Style = BorderSolid)
            if allSolid then
                let x0 = float r.Pos.X
                let y0 = float r.Pos.Y
                let w = float r.Size.X
                let h = float r.Size.Y
                let tw = float top.Width
                let rw = float right.Width
                let bw = float bottom.Width
                let lw = float left.Width
                // Outer path
                let outerSegs = Raster.roundedRectPath4 r tl tr br bl
                let outerContours = Raster.flattenPath outerSegs
                // Inner path
                let ftl = float tl * min 1.0 (min (w / (max 1e-10 (float tl + float tr))) (h / (max 1e-10 (float tl + float bl))))
                let ftr = float tr * min 1.0 (min (w / (max 1e-10 (float tl + float tr))) (h / (max 1e-10 (float tr + float br))))
                let fbr = float br * min 1.0 (min (w / (max 1e-10 (float bl + float br))) (h / (max 1e-10 (float tr + float br))))
                let fbl = float bl * min 1.0 (min (w / (max 1e-10 (float bl + float br))) (h / (max 1e-10 (float tl + float bl))))
                let innerTl = max 0.0 (ftl - max tw lw)
                let innerTr = max 0.0 (ftr - max tw rw)
                let innerBr = max 0.0 (fbr - max bw rw)
                let innerBl = max 0.0 (fbl - max bw lw)
                let innerRect = rect (x0 + lw) (y0 + tw) (w - lw - rw) (h - tw - bw)
                let innerSegs = Raster.roundedRectPath4 innerRect (innerTl * 1.0<px>) (innerTr * 1.0<px>) (innerBr * 1.0<px>) (innerBl * 1.0<px>)
                let innerContours = Raster.flattenPath innerSegs
                // If all sides same color, render as a single stencil fill of outer minus inner
                let allSameColor = top.Color = right.Color && right.Color = bottom.Color && bottom.Color = left.Color
                if allSameColor && top.Style <> NoBorder then
                    // Stencil: outer (increment) then inner (decrement), cover where stencil=1
                    setMVP st mvp
                    Gl.glEnable(Gl.GL_STENCIL_TEST)
                    Gl.glColorMask(false, false, false, false)
                    Gl.glStencilMask(0xFFu)
                    Gl.glClear(Gl.GL_STENCIL_BUFFER_BIT)
                    Gl.glStencilFunc(Gl.GL_ALWAYS, 0, 0xFF)
                    Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_INVERT)
                    // Draw outer contours
                    for contour in outerContours do
                        if contour.Length >= 3 then
                            let verts = Array.zeroCreate<float32> (contour.Length * 2)
                            let mutable i = 0
                            for (x, y) in contour do verts.[i] <- float32 x; verts.[i+1] <- float32 y; i <- i + 2
                            drawTriangleFan st verts
                    // Draw inner contours (inverts back to 0 inside inner)
                    for contour in innerContours do
                        if contour.Length >= 3 then
                            let verts = Array.zeroCreate<float32> (contour.Length * 2)
                            let mutable i = 0
                            for (x, y) in contour do verts.[i] <- float32 x; verts.[i+1] <- float32 y; i <- i + 2
                            drawTriangleFan st verts
                    // Cover
                    Gl.glColorMask(true, true, true, true)
                    Gl.glStencilFunc(Gl.GL_NOTEQUAL, 0, 0xFF)
                    Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_ZERO)
                    setColor st top.Color
                    let coverVerts = rectVerts x0 y0 w h
                    drawTriangles st coverVerts
                    Gl.glDisable(Gl.GL_STENCIL_TEST)
                else
                    // Per-side colors: render each side separately using stencil
                    let cx = x0 + w / 2.0
                    let cy = y0 + h / 2.0
                    let x1 = x0 + w
                    let y1 = y0 + h
                    // Write border ring to stencil (outer XOR inner)
                    let writeBorderRingToStencil () =
                        Gl.glEnable(Gl.GL_STENCIL_TEST)
                        Gl.glColorMask(false, false, false, false)
                        Gl.glStencilMask(0xFFu)
                        Gl.glClear(Gl.GL_STENCIL_BUFFER_BIT)
                        Gl.glStencilFunc(Gl.GL_ALWAYS, 0, 0xFF)
                        Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_INVERT)
                        setMVP st mvp
                        for contour in outerContours do
                            if contour.Length >= 3 then
                                let verts = Array.zeroCreate<float32> (contour.Length * 2)
                                let mutable i = 0
                                for (px, py) in contour do verts.[i] <- float32 px; verts.[i+1] <- float32 py; i <- i + 2
                                drawTriangleFan st verts
                        for contour in innerContours do
                            if contour.Length >= 3 then
                                let verts = Array.zeroCreate<float32> (contour.Length * 2)
                                let mutable i = 0
                                for (px, py) in contour do verts.[i] <- float32 px; verts.[i+1] <- float32 py; i <- i + 2
                                drawTriangleFan st verts
                    // Render one side: clip to triangle region, cover with color
                    let renderSide (side: BorderSide) (triVerts: float32[]) =
                        if side.Style <> NoBorder && side.Width > 0.0<px> then
                            writeBorderRingToStencil ()
                            // AND with triangle clip: write triangle to stencil bit 0x80, then test ring AND triangle
                            // Simpler approach: just render border ring within triangle clip
                            // Use stencil value 1 from ring, then cover only within triangle by using the triangle as geometry
                            Gl.glColorMask(true, true, true, true)
                            Gl.glStencilFunc(Gl.GL_NOTEQUAL, 0, 0xFF)
                            Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_ZERO)
                            // Enable scissor-like clipping via the triangle: just draw the triangle with stencil test
                            setColor st side.Color
                            drawTriangles st triVerts
                            Gl.glDisable(Gl.GL_STENCIL_TEST)
                    // Top side: triangle from center to top-left and top-right corners
                    renderSide top [| float32 cx; float32 cy; float32 x0; float32 y0; float32 x1; float32 y0 |]
                    // Right side: triangle from center to top-right and bottom-right corners
                    renderSide right [| float32 cx; float32 cy; float32 x1; float32 y0; float32 x1; float32 y1 |]
                    // Bottom side: triangle from center to bottom-right and bottom-left corners
                    renderSide bottom [| float32 cx; float32 cy; float32 x1; float32 y1; float32 x0; float32 y1 |]
                    // Left side: triangle from center to bottom-left and top-left corners
                    renderSide left [| float32 cx; float32 cy; float32 x0; float32 y1; float32 x0; float32 y0 |]
            else
                // Non-solid border styles (dashed, dotted, double)
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
                let cx = x0 + w / 2.0
                let cy = y0 + h / 2.0
                // Outer/inner contours
                let outerSegs = Raster.roundedRectPath4 r tl tr br bl
                let outerContours = Raster.flattenPath outerSegs
                let ftl = float tl * min 1.0 (min (w / (max 1e-10 (float tl + float tr))) (h / (max 1e-10 (float tl + float bl))))
                let ftr = float tr * min 1.0 (min (w / (max 1e-10 (float tl + float tr))) (h / (max 1e-10 (float tr + float br))))
                let fbr = float br * min 1.0 (min (w / (max 1e-10 (float bl + float br))) (h / (max 1e-10 (float tr + float br))))
                let fbl = float bl * min 1.0 (min (w / (max 1e-10 (float bl + float br))) (h / (max 1e-10 (float tl + float bl))))
                let innerTl = max 0.0 (ftl - max tw lw)
                let innerTr = max 0.0 (ftr - max tw rw)
                let innerBr = max 0.0 (fbr - max bw rw)
                let innerBl = max 0.0 (fbl - max bw lw)
                let innerRect = rect (x0 + lw) (y0 + tw) (w - lw - rw) (h - tw - bw)
                let innerSegs = Raster.roundedRectPath4 innerRect (innerTl * 1.0<px>) (innerTr * 1.0<px>) (innerBr * 1.0<px>) (innerBl * 1.0<px>)
                let innerContours = Raster.flattenPath innerSegs
                // Helper: write border ring to stencil
                let writeBorderRing () =
                    Gl.glEnable(Gl.GL_STENCIL_TEST)
                    Gl.glColorMask(false, false, false, false)
                    Gl.glStencilMask(0xFFu)
                    Gl.glClear(Gl.GL_STENCIL_BUFFER_BIT)
                    Gl.glStencilFunc(Gl.GL_ALWAYS, 0, 0xFF)
                    Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_INVERT)
                    setMVP st mvp
                    for contour in outerContours do
                        if contour.Length >= 3 then
                            let verts = Array.zeroCreate<float32> (contour.Length * 2)
                            let mutable vi = 0
                            for (px, py) in contour do verts.[vi] <- float32 px; verts.[vi+1] <- float32 py; vi <- vi + 2
                            drawTriangleFan st verts
                    for contour in innerContours do
                        if contour.Length >= 3 then
                            let verts = Array.zeroCreate<float32> (contour.Length * 2)
                            let mutable vi = 0
                            for (px, py) in contour do verts.[vi] <- float32 px; verts.[vi+1] <- float32 py; vi <- vi + 2
                            drawTriangleFan st verts
                // Render one styled side
                let renderStyledSide (side: BorderSide) (sideTriVerts: float32[]) (isHoriz: bool) (edgeStart: float) (edgeLen: float) (depthStart: float) (depthEnd: float) =
                    if side.Style = NoBorder || side.Width <= 0.0<px> then () else
                    let sw = float side.Width
                    match side.Style with
                    | BorderSolid ->
                        writeBorderRing ()
                        Gl.glColorMask(true, true, true, true)
                        Gl.glStencilFunc(Gl.GL_NOTEQUAL, 0, 0xFF)
                        Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_ZERO)
                        setColor st side.Color
                        drawTriangles st sideTriVerts
                        Gl.glDisable(Gl.GL_STENCIL_TEST)
                    | BorderDashed ->
                        // Generate dash rectangles along the edge, stencil-fill within border ring
                        let dashLen = sw * 3.0
                        let mutable pos = 0.0
                        let dashRects = ResizeArray<float32[]>()
                        while pos < edgeLen do
                            let dEnd = min (pos + dashLen) edgeLen
                            let rv =
                                if isHoriz then
                                    rectVerts (edgeStart + pos) depthStart (dEnd - pos) (depthEnd - depthStart)
                                else
                                    rectVerts depthStart (edgeStart + pos) (depthEnd - depthStart) (dEnd - pos)
                            dashRects.Add(rv)
                            pos <- pos + dashLen * 2.0
                        writeBorderRing ()
                        Gl.glColorMask(true, true, true, true)
                        Gl.glStencilFunc(Gl.GL_NOTEQUAL, 0, 0xFF)
                        Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_ZERO)
                        setColor st side.Color
                        for rv in dashRects do drawTriangles st rv
                        Gl.glDisable(Gl.GL_STENCIL_TEST)
                    | BorderDotted ->
                        let dotSpacing = sw
                        let mutable pos = 0.0
                        let dotRects = ResizeArray<float32[]>()
                        while pos < edgeLen do
                            let dEnd = min (pos + dotSpacing) edgeLen
                            let rv =
                                if isHoriz then
                                    rectVerts (edgeStart + pos) depthStart (dEnd - pos) (depthEnd - depthStart)
                                else
                                    rectVerts depthStart (edgeStart + pos) (depthEnd - depthStart) (dEnd - pos)
                            dotRects.Add(rv)
                            pos <- pos + dotSpacing * 2.0
                        writeBorderRing ()
                        Gl.glColorMask(true, true, true, true)
                        Gl.glStencilFunc(Gl.GL_NOTEQUAL, 0, 0xFF)
                        Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_ZERO)
                        setColor st side.Color
                        for rv in dotRects do drawTriangles st rv
                        Gl.glDisable(Gl.GL_STENCIL_TEST)
                    | BorderDouble ->
                        // Outer third and inner third of the border ring
                        let third = sw / 3.0
                        // Outer third rect
                        let outerThirdRect =
                            if isHoriz then
                                if depthEnd > depthStart then // top
                                    rectVerts edgeStart depthStart edgeLen third
                                else // bottom
                                    rectVerts edgeStart (depthEnd + (depthStart - depthEnd - third)) edgeLen third
                            else
                                if depthEnd > depthStart then // left
                                    rectVerts depthStart edgeStart third edgeLen
                                else // right
                                    rectVerts (depthEnd + (depthStart - depthEnd - third)) edgeStart third edgeLen
                        // Inner third rect
                        let innerThirdRect =
                            if isHoriz then
                                if depthEnd > depthStart then // top
                                    rectVerts edgeStart (depthStart + sw - third) edgeLen third
                                else // bottom
                                    rectVerts edgeStart depthEnd edgeLen third
                            else
                                if depthEnd > depthStart then // left
                                    rectVerts (depthStart + sw - third) edgeStart third edgeLen
                                else // right
                                    rectVerts depthEnd edgeStart third edgeLen
                        writeBorderRing ()
                        Gl.glColorMask(true, true, true, true)
                        Gl.glStencilFunc(Gl.GL_NOTEQUAL, 0, 0xFF)
                        Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_ZERO)
                        setColor st side.Color
                        drawTriangles st outerThirdRect
                        // Need to re-write stencil for second band
                        writeBorderRing ()
                        Gl.glColorMask(true, true, true, true)
                        Gl.glStencilFunc(Gl.GL_NOTEQUAL, 0, 0xFF)
                        Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_ZERO)
                        setColor st side.Color
                        drawTriangles st innerThirdRect
                        Gl.glDisable(Gl.GL_STENCIL_TEST)
                    | NoBorder -> ()
                // Top side
                let topTri = [| float32 cx; float32 cy; float32 x0; float32 y0; float32 x1; float32 y0 |]
                renderStyledSide top topTri true x0 w y0 (y0 + tw)
                // Right side
                let rightTri = [| float32 cx; float32 cy; float32 x1; float32 y0; float32 x1; float32 y1 |]
                renderStyledSide right rightTri false y0 h x1 (x1 - rw)
                // Bottom side
                let bottomTri = [| float32 cx; float32 cy; float32 x1; float32 y1; float32 x0; float32 y1 |]
                renderStyledSide bottom bottomTri true x0 w y1 (y1 - bw)
                // Left side
                let leftTri = [| float32 cx; float32 cy; float32 x0; float32 y1; float32 x0; float32 y0 |]
                renderStyledSide left leftTri false y0 h x0 (x0 + lw)

        | BackdropFilter (clipPath, fillRule, filters, children) ->
            // 1) Capture current framebuffer as a texture
            let dstTex = captureCurrentFbToTexture st.W st.H
            // 2) Apply filter chain to the captured texture
            let mutable currentTex = dstTex
            let mutable tempTargets = []
            for filter in filters do
                match filter with
                | Blur radius ->
                    let hShader = GlslCompiler.buildHBlurShader radius st.W
                    let vShader = GlslCompiler.buildVBlurShader radius st.H
                    for _ = 1 to 3 do
                        let hT = acquireFbo st.FboPool
                        Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, hT.Fbo)
                        Gl.glViewport(0, 0, st.W, st.H)
                        Gl.glDisable(Gl.GL_BLEND)
                        drawFullscreenQuad hShader currentTex 0
                        Gl.glEnable(Gl.GL_BLEND)
                        currentTex <- hT.Texture
                        tempTargets <- hT :: tempTargets
                        let vT = acquireFbo st.FboPool
                        Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, vT.Fbo)
                        Gl.glViewport(0, 0, st.W, st.H)
                        Gl.glDisable(Gl.GL_BLEND)
                        drawFullscreenQuad vShader currentTex 0
                        Gl.glEnable(Gl.GL_BLEND)
                        currentTex <- vT.Texture
                        tempTargets <- vT :: tempTargets
                | _ ->
                    let shader =
                        match filter with
                        | Brightness f -> Some (GlslCompiler.buildBrightnessShader f)
                        | Contrast f -> Some (GlslCompiler.buildContrastShader f)
                        | Saturate f -> Some (GlslCompiler.buildSaturateShader f)
                        | Grayscale f -> Some (GlslCompiler.buildGrayscaleShader f)
                        | Sepia f -> Some (GlslCompiler.buildSepiaShader f)
                        | Invert f -> Some (GlslCompiler.buildInvertShader f)
                        | HueRotate d -> Some (GlslCompiler.buildHueRotateShader d)
                        | OpacityFilter f -> Some (GlslCompiler.buildOpacityShader f)
                        | Custom _ -> invalidOp "Custom pixel filters require the software renderer"
                        | _ -> None
                    match shader with
                    | Some s ->
                        let next = acquireFbo st.FboPool
                        Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, next.Fbo)
                        Gl.glViewport(0, 0, st.W, st.H)
                        Gl.glDisable(Gl.GL_BLEND)
                        drawFullscreenQuad s currentTex 0
                        Gl.glEnable(Gl.GL_BLEND)
                        currentTex <- next.Texture
                        tempTargets <- next :: tempTargets
                    | None -> ()
            // 3) Composite filtered backdrop within clip region using stencil
            Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, st.ActiveFbo)
            Gl.glViewport(0, 0, st.W, st.H)
            // Write clip path to stencil
            let contours = Raster.flattenPath clipPath
            let transformedContours =
                contours |> List.map (fun c ->
                    c |> List.map (fun (x, y) ->
                        let tx = m.M11 * x + m.M21 * y + m.DX
                        let ty = m.M12 * x + m.M22 * y + m.DY
                        (tx, ty)))
            Gl.glUseProgram(st.Program)
            setMVP st (ortho (float32 st.W) (float32 st.H))
            Gl.glEnable(Gl.GL_STENCIL_TEST)
            Gl.glColorMask(false, false, false, false)
            Gl.glStencilMask(0xFFu)
            Gl.glClear(Gl.GL_STENCIL_BUFFER_BIT)
            match fillRule with
            | EvenOdd ->
                Gl.glStencilFunc(Gl.GL_ALWAYS, 0, 0xFF)
                Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_INVERT)
            | NonZero ->
                Gl.glStencilFunc(Gl.GL_ALWAYS, 0, 0xFF)
                Gl.glStencilOpSeparate(Gl.GL_FRONT, Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_INCR_WRAP)
                Gl.glStencilOpSeparate(Gl.GL_BACK, Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_DECR_WRAP)
            for contour in transformedContours do
                if contour.Length >= 3 then
                    let verts = Array.zeroCreate<float32> (contour.Length * 2)
                    let mutable i = 0
                    for (x, y) in contour do verts.[i] <- float32 x; verts.[i+1] <- float32 y; i <- i + 2
                    drawTriangleFan st verts
            // Draw filtered texture where stencil != 0, replacing destination pixels
            Gl.glColorMask(true, true, true, true)
            Gl.glStencilFunc(Gl.GL_NOTEQUAL, 0, 0xFF)
            Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_ZERO)
            Gl.glDisable(Gl.GL_BLEND)
            let passthrough = GlslCompiler.buildPassthroughShader ()
            drawFullscreenQuad passthrough currentTex 0
            Gl.glEnable(Gl.GL_BLEND)
            Gl.glDisable(Gl.GL_STENCIL_TEST)
            // Cleanup
            for t in tempTargets do releaseFbo st.FboPool t
            deleteTexture dstTex
            restoreMainState st
            // 4) Draw children on top
            children |> List.iter (dispatch st orthoBase m softFb)

        | Group3D (xform, opacity, children) ->
            if opacity < 1.0 / 255.0 then () else
            // Render children to offscreen FBO
            let target = renderToOffscreen st orthoBase m softFb children st.W st.H
            Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, st.ActiveFbo)
            Gl.glViewport(0, 0, st.W, st.H)
            // Build MVP: ortho * Matrix3x2(m) * Matrix4x4(xform) — but for the cover quad we use
            // the ortho * 4x4 directly since children are rendered in identity 2D space
            let m4 = Matrix4x4.multiply (Matrix4x4.fromMatrix3x2 m) xform
            let ortho4 =
                let o = orthoBase
                { M11 = float o.[0]; M12 = float o.[1]; M13 = float o.[2]; M14 = float o.[3]
                  M21 = float o.[4]; M22 = float o.[5]; M23 = float o.[6]; M24 = float o.[7]
                  M31 = float o.[8]; M32 = float o.[9]; M33 = float o.[10]; M34 = float o.[11]
                  M41 = float o.[12]; M42 = float o.[13]; M43 = float o.[14]; M44 = float o.[15] }
            let finalMvp = Matrix4x4.multiply ortho4 m4
            let finalArr = Matrix4x4.toFloat32Array finalMvp
            // Draw textured quad with the 3D transform using a textured-quad shader
            let texShader = GlslCompiler.buildTexturedQuadShader ()
            Gl.glUseProgram(texShader.Program)
            Gl.glUniformMatrix4fv(texShader.LocMVP, 1, false, finalArr)
            Gl.glActiveTexture(Gl.GL_TEXTURE0)
            Gl.glBindTexture(Gl.GL_TEXTURE_2D, target.Texture)
            if texShader.LocTex0 >= 0 then Gl.glUniform1i(texShader.LocTex0, 0)
            if texShader.LocOpacity >= 0 then Gl.glUniform1f(texShader.LocOpacity, float32 opacity)
            // Full-screen quad in pixel coordinates: (0,0)-(W,H)
            let w = float32 st.W
            let h = float32 st.H
            let quadVerts = [| 0.0f; 0.0f; 0.0f; 0.0f
                               w; 0.0f; 1.0f; 0.0f
                               w; h; 1.0f; 1.0f
                               0.0f; 0.0f; 0.0f; 0.0f
                               w; h; 1.0f; 1.0f
                               0.0f; h; 0.0f; 1.0f |]
            Gl.glBindBuffer(Gl.GL_ARRAY_BUFFER, st.Vbo)
            let handle = GCHandle.Alloc(quadVerts, GCHandleType.Pinned)
            try Gl.glBufferData(Gl.GL_ARRAY_BUFFER, nativeint (quadVerts.Length * 4), handle.AddrOfPinnedObject(), Gl.GL_DYNAMIC_DRAW)
            finally handle.Free()
            Gl.glEnableVertexAttribArray(texShader.LocPos)
            if texShader.LocUV >= 0 then Gl.glEnableVertexAttribArray(texShader.LocUV)
            Gl.glVertexAttribPointer(texShader.LocPos, 2, Gl.GL_FLOAT, false, 16, nativeint 0)
            if texShader.LocUV >= 0 then Gl.glVertexAttribPointer(texShader.LocUV, 2, Gl.GL_FLOAT, false, 16, nativeint 8)
            Gl.glDrawArrays(Gl.GL_TRIANGLES, 0, 6)
            if texShader.LocUV >= 0 then Gl.glDisableVertexAttribArray(texShader.LocUV)
            // Cleanup
            releaseFbo st.FboPool target
            restoreMainState st

        | Filtered (filters, children) ->
            // Render children to offscreen FBO
            let mutable current = renderToOffscreen st orthoBase m softFb children st.W st.H
            let mutable targets = [current]
            for filter in filters do
                match filter with
                | Brightness factor ->
                    let shader = GlslCompiler.buildBrightnessShader factor
                    let next = acquireFbo st.FboPool
                    Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, next.Fbo)
                    Gl.glViewport(0, 0, st.W, st.H)
                    Gl.glDisable(Gl.GL_BLEND)
                    drawFullscreenQuad shader current.Texture 0
                    Gl.glEnable(Gl.GL_BLEND)
                    current <- next
                    targets <- next :: targets
                | Contrast factor ->
                    let shader = GlslCompiler.buildContrastShader factor
                    let next = acquireFbo st.FboPool
                    Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, next.Fbo)
                    Gl.glViewport(0, 0, st.W, st.H)
                    Gl.glDisable(Gl.GL_BLEND)
                    drawFullscreenQuad shader current.Texture 0
                    Gl.glEnable(Gl.GL_BLEND)
                    current <- next
                    targets <- next :: targets
                | Saturate factor ->
                    let shader = GlslCompiler.buildSaturateShader factor
                    let next = acquireFbo st.FboPool
                    Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, next.Fbo)
                    Gl.glViewport(0, 0, st.W, st.H)
                    Gl.glDisable(Gl.GL_BLEND)
                    drawFullscreenQuad shader current.Texture 0
                    Gl.glEnable(Gl.GL_BLEND)
                    current <- next
                    targets <- next :: targets
                | Grayscale factor ->
                    let shader = GlslCompiler.buildGrayscaleShader factor
                    let next = acquireFbo st.FboPool
                    Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, next.Fbo)
                    Gl.glViewport(0, 0, st.W, st.H)
                    Gl.glDisable(Gl.GL_BLEND)
                    drawFullscreenQuad shader current.Texture 0
                    Gl.glEnable(Gl.GL_BLEND)
                    current <- next
                    targets <- next :: targets
                | Sepia factor ->
                    let shader = GlslCompiler.buildSepiaShader factor
                    let next = acquireFbo st.FboPool
                    Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, next.Fbo)
                    Gl.glViewport(0, 0, st.W, st.H)
                    Gl.glDisable(Gl.GL_BLEND)
                    drawFullscreenQuad shader current.Texture 0
                    Gl.glEnable(Gl.GL_BLEND)
                    current <- next
                    targets <- next :: targets
                | Invert factor ->
                    let shader = GlslCompiler.buildInvertShader factor
                    let next = acquireFbo st.FboPool
                    Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, next.Fbo)
                    Gl.glViewport(0, 0, st.W, st.H)
                    Gl.glDisable(Gl.GL_BLEND)
                    drawFullscreenQuad shader current.Texture 0
                    Gl.glEnable(Gl.GL_BLEND)
                    current <- next
                    targets <- next :: targets
                | HueRotate degrees ->
                    let shader = GlslCompiler.buildHueRotateShader degrees
                    let next = acquireFbo st.FboPool
                    Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, next.Fbo)
                    Gl.glViewport(0, 0, st.W, st.H)
                    Gl.glDisable(Gl.GL_BLEND)
                    drawFullscreenQuad shader current.Texture 0
                    Gl.glEnable(Gl.GL_BLEND)
                    current <- next
                    targets <- next :: targets
                | OpacityFilter factor ->
                    let shader = GlslCompiler.buildOpacityShader factor
                    let next = acquireFbo st.FboPool
                    Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, next.Fbo)
                    Gl.glViewport(0, 0, st.W, st.H)
                    Gl.glDisable(Gl.GL_BLEND)
                    drawFullscreenQuad shader current.Texture 0
                    Gl.glEnable(Gl.GL_BLEND)
                    current <- next
                    targets <- next :: targets
                | Custom _ ->
                    invalidOp "Custom pixel filters require the software renderer"
                | Blur radius ->
                    // 6-pass separable box blur (H→V→H→V→H→V) matching software's 3× box blur
                    let hShader = GlslCompiler.buildHBlurShader radius st.W
                    let vShader = GlslCompiler.buildVBlurShader radius st.H
                    for _ = 1 to 3 do
                        // Horizontal pass
                        let hTarget = acquireFbo st.FboPool
                        Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, hTarget.Fbo)
                        Gl.glViewport(0, 0, st.W, st.H)
                        Gl.glDisable(Gl.GL_BLEND)
                        drawFullscreenQuad hShader current.Texture 0
                        Gl.glEnable(Gl.GL_BLEND)
                        current <- hTarget
                        targets <- hTarget :: targets
                        // Vertical pass
                        let vTarget = acquireFbo st.FboPool
                        Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, vTarget.Fbo)
                        Gl.glViewport(0, 0, st.W, st.H)
                        Gl.glDisable(Gl.GL_BLEND)
                        drawFullscreenQuad vShader current.Texture 0
                        Gl.glEnable(Gl.GL_BLEND)
                        current <- vTarget
                        targets <- vTarget :: targets
                | DropShadow (dx, dy, blurRadius, color) ->
                    // Step 1: Offset + colorize the source
                    let colorizeShader = GlslCompiler.buildDropShadowColorizeShader dx dy color st.W st.H
                    let shadowTarget = acquireFbo st.FboPool
                    Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, shadowTarget.Fbo)
                    Gl.glViewport(0, 0, st.W, st.H)
                    Gl.glDisable(Gl.GL_BLEND)
                    drawFullscreenQuad colorizeShader current.Texture 0
                    Gl.glEnable(Gl.GL_BLEND)
                    targets <- shadowTarget :: targets
                    // Step 2: Blur the shadow (6 passes)
                    let mutable shadowCur = shadowTarget
                    if blurRadius > 0 then
                        let hShader = GlslCompiler.buildHBlurShader blurRadius st.W
                        let vShader = GlslCompiler.buildVBlurShader blurRadius st.H
                        for _ = 1 to 3 do
                            let hT = acquireFbo st.FboPool
                            Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, hT.Fbo)
                            Gl.glViewport(0, 0, st.W, st.H)
                            Gl.glDisable(Gl.GL_BLEND)
                            drawFullscreenQuad hShader shadowCur.Texture 0
                            Gl.glEnable(Gl.GL_BLEND)
                            shadowCur <- hT
                            targets <- hT :: targets
                            let vT = acquireFbo st.FboPool
                            Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, vT.Fbo)
                            Gl.glViewport(0, 0, st.W, st.H)
                            Gl.glDisable(Gl.GL_BLEND)
                            drawFullscreenQuad vShader shadowCur.Texture 0
                            Gl.glEnable(Gl.GL_BLEND)
                            shadowCur <- vT
                            targets <- vT :: targets
                    // Step 3: Draw blurred shadow to parent FBO, then original on top
                    Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, st.ActiveFbo)
                    Gl.glViewport(0, 0, st.W, st.H)
                    let passthrough = GlslCompiler.buildPassthroughShader ()
                    // Draw shadow (with blending)
                    drawFullscreenQuad passthrough shadowCur.Texture 0
                    // Draw original on top (with blending)
                    drawFullscreenQuad passthrough current.Texture 0
                    // Cleanup and restore, skip final composite below
                    for t in targets do releaseFbo st.FboPool t
                    restoreMainState st
                    // Return early—we already composited to main FBO
                    current <- { Fbo = -1; Texture = 0; StencilRb = 0; Width = 0; Height = 0 }
                    targets <- []

            // Final composite: draw result texture to parent FBO with blending
            if current.Fbo <> -1 then
                Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, st.ActiveFbo)
                Gl.glViewport(0, 0, st.W, st.H)
                let passthrough = GlslCompiler.buildPassthroughShader ()
                drawFullscreenQuad passthrough current.Texture 0
                for t in targets do releaseFbo st.FboPool t
                restoreMainState st

        | Blend (mode, children) ->
            // Capture current framebuffer as dst texture
            let dstTex = captureCurrentFbToTexture st.W st.H
            // Render blend children to offscreen FBO
            let srcTarget = renderToOffscreen st orthoBase m softFb children st.W st.H
            // Draw blended result to parent FBO
            Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, st.ActiveFbo)
            Gl.glViewport(0, 0, st.W, st.H)
            let blendShader = GlslCompiler.buildBlendShader mode
            Gl.glDisable(Gl.GL_BLEND)
            drawFullscreenQuad blendShader srcTarget.Texture dstTex
            Gl.glEnable(Gl.GL_BLEND)
            // Cleanup
            releaseFbo st.FboPool srcTarget
            deleteTexture dstTex
            restoreMainState st

        | Clip (clipPath, fillRule, children) ->
            let contours = Raster.flattenPath clipPath
            if contours.IsEmpty then () else
            let transformedContours =
                contours |> List.map (fun c ->
                    c |> List.map (fun (x, y) ->
                        let tx = m.M11 * x + m.M21 * y + m.DX
                        let ty = m.M12 * x + m.M22 * y + m.DY
                        (tx, ty)))

            // Phase 1: Write stencil from clip path
            Gl.glEnable(Gl.GL_STENCIL_TEST)
            Gl.glColorMask(false, false, false, false)
            Gl.glStencilMask(0xFFu)
            Gl.glClear(Gl.GL_STENCIL_BUFFER_BIT)

            let identityMvp = orthoBase
            setMVP st identityMvp

            match fillRule with
            | EvenOdd ->
                Gl.glStencilFunc(Gl.GL_ALWAYS, 0, 0xFF)
                Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_INVERT)
            | NonZero ->
                Gl.glStencilFunc(Gl.GL_ALWAYS, 0, 0xFF)
                Gl.glStencilOpSeparate(Gl.GL_FRONT, Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_INCR_WRAP)
                Gl.glStencilOpSeparate(Gl.GL_BACK, Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_DECR_WRAP)

            for contour in transformedContours do
                if contour.Length >= 3 then
                    let verts = Array.zeroCreate<float32> (contour.Length * 2)
                    let mutable i = 0
                    for (x, y) in contour do
                        verts.[i] <- float32 x; verts.[i+1] <- float32 y
                        i <- i + 2
                    drawTriangleFan st verts

            // Phase 2: Render children with stencil test active
            Gl.glColorMask(true, true, true, true)
            Gl.glStencilFunc(Gl.GL_NOTEQUAL, 0, 0xFF)
            Gl.glStencilOp(Gl.GL_KEEP, Gl.GL_KEEP, Gl.GL_KEEP)
            children |> List.iter (dispatch st orthoBase m softFb)

            Gl.glDisable(Gl.GL_STENCIL_TEST)

        | Mask (maskCmds, contentCmds) ->
            let maskTarget = renderToOffscreen st orthoBase m softFb maskCmds st.W st.H
            let contentTarget = renderToOffscreen st orthoBase m softFb contentCmds st.W st.H
            Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, st.ActiveFbo)
            Gl.glViewport(0, 0, st.W, st.H)
            let maskShader = GlslCompiler.buildMaskShader ()
            drawFullscreenQuad maskShader contentTarget.Texture maskTarget.Texture
            releaseFbo st.FboPool maskTarget
            releaseFbo st.FboPool contentTarget
            restoreMainState st

        | StrokeToFill (segs, style, paint, fillRule) ->
            let contours = Raster.flattenStrokePath segs
            let allOutline = ResizeArray<PathSeg>()
            for contour, closed in contours do
                let dashed = Raster.strokeDashes contour closed (style.DashArray |> List.map float) (float style.DashOffset)
                for sub, subClosed in dashed do allOutline.AddRange(Raster.expandStroke sub subClosed style)
            let outlineContours = Raster.flattenPath (Seq.toList allOutline)
            if not (stencilFillContoursWithPaint st outlineContours fillRule paint mvp) then
                softwareFallback st cmd m softFb

        | PathBool (op, segsA, segsB, paint, fillRule) ->
            let resultSegs = Raster.pathBoolOp op segsA segsB
            let contours = Raster.flattenPath resultSegs
            if not (stencilFillContoursWithPaint st contours fillRule paint mvp) then
                softwareFallback st cmd m softFb

        | SaveRestore cmds ->
            let target = renderToOffscreen st orthoBase m softFb cmds st.W st.H
            Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, st.ActiveFbo)
            Gl.glViewport(0, 0, st.W, st.H)
            let passthrough = GlslCompiler.buildPassthroughShader ()
            drawFullscreenQuad passthrough target.Texture 0
            releaseFbo st.FboPool target
            restoreMainState st

        | AntiAlias (_samples, cmds) ->
            cmds |> List.iter (dispatch st orthoBase m softFb)

        | DrawGlyphs (origin, runs, paint) ->
            let ox, oy = float origin.X, float origin.Y
            for run in runs do
                let fontSize = run.FontSize
                let ppem = max 1 (int (round fontSize))
                if Text.isBitmapFont run.Font || ppem <= 48 then
                    // Bitmap glyph atlas path — pixel-perfect for small text
                    let entries =
                        run.Glyphs |> Array.choose (fun g ->
                            // Bake the sub-pixel offset into the cached bitmap (as the raster path does),
                            // so every glyph in the run shares one baseline instead of rounding per-glyph.
                            let wx = ox + g.X
                            let wy = oy + g.Y
                            let dstX = int (floor wx)
                            let dstY = int (floor wy)
                            let subX = int (floor ((wx - float dstX) * 4.0)) |> max 0 |> min 3
                            let subY = int (floor ((wy - float dstY) * 4.0)) |> max 0 |> min 3
                            match ensureBitmapGlyph run.Font g.GlyphId ppem subX subY with
                            | Some entry -> Some (dstX, dstY, entry)
                            | None -> None)
                    // Upload bitmap atlas texture if dirty
                    let atlas = bitmapAtlas
                    if atlas.Dirty then
                        if atlas.TexId = 0 then
                            let mutable tex = 0
                            Gl.glGenTextures(1, &tex)
                            atlas.TexId <- tex
                        Gl.glActiveTexture(Gl.GL_TEXTURE0)
                        Gl.glBindTexture(Gl.GL_TEXTURE_2D, atlas.TexId)
                        let h = GCHandle.Alloc(atlas.Pixels, GCHandleType.Pinned)
                        try Gl.glTexImage2D(Gl.GL_TEXTURE_2D, 0, Gl.GL_LUMINANCE, atlas.AtlasW, atlas.AtlasH, 0, Gl.GL_LUMINANCE, Gl.GL_UNSIGNED_BYTE, h.AddrOfPinnedObject())
                        finally h.Free()
                        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_MIN_FILTER, Gl.GL_NEAREST)
                        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_MAG_FILTER, Gl.GL_NEAREST)
                        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_WRAP_S, Gl.GL_CLAMP_TO_EDGE)
                        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_WRAP_T, Gl.GL_CLAMP_TO_EDGE)
                        atlas.Dirty <- false
                    // Build vertex array — each glyph positioned at pixel coords with bitmap bearing
                    let verts = Array.zeroCreate<float32> (entries.Length * 6 * 4)
                    let mutable vi = 0
                    for (dstX, dstY, entry) in entries do
                        let x0 = float32 (dstX + entry.BearingX)
                        let y0 = float32 (dstY + entry.BearingY)
                        let x1 = x0 + float32 entry.W
                        let y1 = y0 + float32 entry.H
                        verts.[vi] <- x0; verts.[vi+1] <- y0; verts.[vi+2] <- entry.U0; verts.[vi+3] <- entry.V0; vi <- vi + 4
                        verts.[vi] <- x1; verts.[vi+1] <- y0; verts.[vi+2] <- entry.U1; verts.[vi+3] <- entry.V0; vi <- vi + 4
                        verts.[vi] <- x1; verts.[vi+1] <- y1; verts.[vi+2] <- entry.U1; verts.[vi+3] <- entry.V1; vi <- vi + 4
                        verts.[vi] <- x0; verts.[vi+1] <- y0; verts.[vi+2] <- entry.U0; verts.[vi+3] <- entry.V0; vi <- vi + 4
                        verts.[vi] <- x1; verts.[vi+1] <- y1; verts.[vi+2] <- entry.U1; verts.[vi+3] <- entry.V1; vi <- vi + 4
                        verts.[vi] <- x0; verts.[vi+1] <- y1; verts.[vi+2] <- entry.U0; verts.[vi+3] <- entry.V1; vi <- vi + 4
                    // Draw with bitmap text shader (direct alpha, no SDF smoothstep)
                    let ts = BitmapTextShader.getShader ()
                    if entries.Length > 0 && vi > 0 then
                        Gl.glUseProgram(ts.Program)
                        Gl.glUniformMatrix4fv(ts.LocMVP, 1, false, mvp)
                        let (cr, cg, cb, ca) =
                            match paint with
                            | Solid c ->
                                let a = float32 c.A / 255.0f
                                (float32 c.R / 255.0f * a, float32 c.G / 255.0f * a, float32 c.B / 255.0f * a, a)
                            | _ -> (1.0f, 1.0f, 1.0f, 1.0f)
                        Gl.glUniform4f(ts.LocColor, cr, cg, cb, ca)
                        Gl.glActiveTexture(Gl.GL_TEXTURE0)
                        Gl.glBindTexture(Gl.GL_TEXTURE_2D, atlas.TexId)
                        Gl.glUniform1i(ts.LocAtlas, 0)
                        Gl.glBindBuffer(Gl.GL_ARRAY_BUFFER, st.Vbo)
                        let handle = GCHandle.Alloc(verts, GCHandleType.Pinned)
                        try Gl.glBufferData(Gl.GL_ARRAY_BUFFER, nativeint (verts.Length * 4), handle.AddrOfPinnedObject(), Gl.GL_DYNAMIC_DRAW)
                        finally handle.Free()
                        Gl.glEnableVertexAttribArray(ts.LocPos)
                        Gl.glEnableVertexAttribArray(ts.LocUV)
                        Gl.glVertexAttribPointer(ts.LocPos, 2, Gl.GL_FLOAT, false, 16, nativeint 0)
                        Gl.glVertexAttribPointer(ts.LocUV, 2, Gl.GL_FLOAT, false, 16, nativeint 8)
                        Gl.glDrawArrays(Gl.GL_TRIANGLES, 0, entries.Length * 6)
                        Gl.glDisableVertexAttribArray(ts.LocUV)
                        restoreMainState st
                elif fontSize <= 80.0 then
                    // SDF atlas path (with mipmaps for proper minification filtering)
                    let atlas = SdfAtlas.getAtlas run.Font
                    let entries =
                        run.Glyphs |> Array.map (fun g -> (g, SdfAtlas.ensureGlyph atlas run.Font g.GlyphId))
                    if atlas.Dirty then
                        if atlas.TexId = 0 then
                            let mutable tex = 0
                            Gl.glGenTextures(1, &tex)
                            atlas.TexId <- tex
                        Gl.glActiveTexture(Gl.GL_TEXTURE0)
                        Gl.glBindTexture(Gl.GL_TEXTURE_2D, atlas.TexId)
                        let h = GCHandle.Alloc(atlas.Pixels, GCHandleType.Pinned)
                        try Gl.glTexImage2D(Gl.GL_TEXTURE_2D, 0, Gl.GL_LUMINANCE, atlas.W, atlas.H, 0, Gl.GL_LUMINANCE, Gl.GL_UNSIGNED_BYTE, h.AddrOfPinnedObject())
                        finally h.Free()
                        Gl.glGenerateMipmap(Gl.GL_TEXTURE_2D)
                        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_MIN_FILTER, Gl.GL_LINEAR_MIPMAP_LINEAR)
                        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_MAG_FILTER, Gl.GL_LINEAR)
                        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_WRAP_S, Gl.GL_CLAMP_TO_EDGE)
                        Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_WRAP_T, Gl.GL_CLAMP_TO_EDGE)
                        atlas.Dirty <- false
                        st.TextAtlasTex <- atlas.TexId
                    let verts = Array.zeroCreate<float32> (entries.Length * 6 * 4)
                    let mutable vi = 0
                    for (g, entry) in entries do
                        let gx = float32 (ox + g.X + entry.BearingX * fontSize)
                        let gy = float32 (oy + g.Y - (entry.BearingY + entry.GlyphH) * fontSize)
                        let gw = float32 (entry.GlyphW * fontSize)
                        let gh = float32 (entry.GlyphH * fontSize)
                        let maxDim = max gw gh
                        let cellSize =
                            if maxDim > 0.1f then maxDim * float32 SdfAtlas.SdfSize / float32 (SdfAtlas.SdfSize - 2 * SdfAtlas.SdfPadding)
                            else float32 fontSize * 0.3f
                        let padX = (cellSize - gw) / 2.0f
                        let padY = (cellSize - gh) / 2.0f
                        let x0 = gx - padX
                        let y0 = gy - padY
                        let x1 = gx + gw + padX
                        let y1 = gy + gh + padY
                        verts.[vi] <- x0; verts.[vi+1] <- y0; verts.[vi+2] <- entry.U0; verts.[vi+3] <- entry.V0; vi <- vi + 4
                        verts.[vi] <- x1; verts.[vi+1] <- y0; verts.[vi+2] <- entry.U1; verts.[vi+3] <- entry.V0; vi <- vi + 4
                        verts.[vi] <- x1; verts.[vi+1] <- y1; verts.[vi+2] <- entry.U1; verts.[vi+3] <- entry.V1; vi <- vi + 4
                        verts.[vi] <- x0; verts.[vi+1] <- y0; verts.[vi+2] <- entry.U0; verts.[vi+3] <- entry.V0; vi <- vi + 4
                        verts.[vi] <- x1; verts.[vi+1] <- y1; verts.[vi+2] <- entry.U1; verts.[vi+3] <- entry.V1; vi <- vi + 4
                        verts.[vi] <- x0; verts.[vi+1] <- y1; verts.[vi+2] <- entry.U0; verts.[vi+3] <- entry.V1; vi <- vi + 4
                    let ts = TextShader.getTextShader ()
                    if entries.Length > 0 && vi > 0 then
                        Gl.glUseProgram(ts.Program)
                        Gl.glUniformMatrix4fv(ts.LocMVP, 1, false, mvp)
                        let (cr, cg, cb, ca) =
                            match paint with
                            | Solid c ->
                                let a = float32 c.A / 255.0f
                                (float32 c.R / 255.0f * a, float32 c.G / 255.0f * a, float32 c.B / 255.0f * a, a)
                            | _ -> (1.0f, 1.0f, 1.0f, 1.0f)
                        Gl.glUniform4f(ts.LocColor, cr, cg, cb, ca)
                        Gl.glActiveTexture(Gl.GL_TEXTURE0)
                        Gl.glBindTexture(Gl.GL_TEXTURE_2D, atlas.TexId)
                        Gl.glUniform1i(ts.LocAtlas, 0)
                        Gl.glBindBuffer(Gl.GL_ARRAY_BUFFER, st.Vbo)
                        let handle = GCHandle.Alloc(verts, GCHandleType.Pinned)
                        try Gl.glBufferData(Gl.GL_ARRAY_BUFFER, nativeint (verts.Length * 4), handle.AddrOfPinnedObject(), Gl.GL_DYNAMIC_DRAW)
                        finally handle.Free()
                        Gl.glEnableVertexAttribArray(ts.LocPos)
                        Gl.glEnableVertexAttribArray(ts.LocUV)
                        Gl.glVertexAttribPointer(ts.LocPos, 2, Gl.GL_FLOAT, false, 16, nativeint 0)
                        Gl.glVertexAttribPointer(ts.LocUV, 2, Gl.GL_FLOAT, false, 16, nativeint 8)
                        Gl.glDrawArrays(Gl.GL_TRIANGLES, 0, entries.Length * 6)
                        Gl.glDisableVertexAttribArray(ts.LocUV)
                        restoreMainState st
                else
                    // Vector path for large text: outline → stencil fill
                    let s = fontSize / Text.unitsPerEm run.Font
                    for g in run.Glyphs do
                        let outline = Text.glyphOutline run.Font g.GlyphId
                        if not outline.IsEmpty then
                            let positioned = outline |> List.map (Text.translatePathSeg (ox + g.X) (oy + g.Y) s)
                            let contours = Raster.flattenPath positioned
                            if not (stencilFillContoursWithPaint st contours EvenOdd paint mvp) then
                                softwareFallback st cmd m softFb

        | Blit (src, dstPos, opacity) ->
            let tex = getOrUploadTexture st src
            let x0 = float32 (float dstPos.X)
            let y0 = float32 (float dstPos.Y)
            let x1 = x0 + float32 src.W
            let y1 = y0 + float32 src.H
            let quadVerts = [| x0;y0;0.0f;0.0f; x1;y0;1.0f;0.0f; x1;y1;1.0f;1.0f; x0;y0;0.0f;0.0f; x1;y1;1.0f;1.0f; x0;y1;0.0f;1.0f |]
            let texShader = GlslCompiler.buildTexturedQuadShader ()
            Gl.glUseProgram(texShader.Program)
            Gl.glUniformMatrix4fv(texShader.LocMVP, 1, false, mvp)
            Gl.glActiveTexture(Gl.GL_TEXTURE0)
            Gl.glBindTexture(Gl.GL_TEXTURE_2D, tex)
            if texShader.LocTex0 >= 0 then Gl.glUniform1i(texShader.LocTex0, 0)
            if texShader.LocOpacity >= 0 then Gl.glUniform1f(texShader.LocOpacity, float32 opacity)
            Gl.glBindBuffer(Gl.GL_ARRAY_BUFFER, st.Vbo)
            let handle = GCHandle.Alloc(quadVerts, GCHandleType.Pinned)
            try Gl.glBufferData(Gl.GL_ARRAY_BUFFER, nativeint (quadVerts.Length * 4), handle.AddrOfPinnedObject(), Gl.GL_DYNAMIC_DRAW)
            finally handle.Free()
            Gl.glEnableVertexAttribArray(texShader.LocPos)
            if texShader.LocUV >= 0 then Gl.glEnableVertexAttribArray(texShader.LocUV)
            Gl.glVertexAttribPointer(texShader.LocPos, 2, Gl.GL_FLOAT, false, 16, nativeint 0)
            if texShader.LocUV >= 0 then Gl.glVertexAttribPointer(texShader.LocUV, 2, Gl.GL_FLOAT, false, 16, nativeint 8)
            Gl.glDrawArrays(Gl.GL_TRIANGLES, 0, 6)
            if texShader.LocUV >= 0 then Gl.glDisableVertexAttribArray(texShader.LocUV)
            restoreMainState st

        | BlitExternalTexture (texId, dstPos, w, h, opacity) ->
            // Draw an EGLImage-from-dmabuf texture (already bound to EXTERNAL_OES by
            // the compositor) — same quad as Blit but with the OES sampler shader.
            let x0 = float32 (float dstPos.X)
            let y0 = float32 (float dstPos.Y)
            let x1 = x0 + float32 w
            let y1 = y0 + float32 h
            let quadVerts = [| x0;y0;0.0f;0.0f; x1;y0;1.0f;0.0f; x1;y1;1.0f;1.0f; x0;y0;0.0f;0.0f; x1;y1;1.0f;1.0f; x0;y1;0.0f;1.0f |]
            let texShader = GlslCompiler.buildExternalTexturedQuadShader ()
            Gl.glUseProgram(texShader.Program)
            Gl.glUniformMatrix4fv(texShader.LocMVP, 1, false, mvp)
            Gl.glActiveTexture(Gl.GL_TEXTURE0)
            Gl.glBindTexture(Gl.GL_TEXTURE_EXTERNAL_OES, int texId)
            if texShader.LocTex0 >= 0 then Gl.glUniform1i(texShader.LocTex0, 0)
            if texShader.LocOpacity >= 0 then Gl.glUniform1f(texShader.LocOpacity, float32 opacity)
            Gl.glBindBuffer(Gl.GL_ARRAY_BUFFER, st.Vbo)
            let handle = GCHandle.Alloc(quadVerts, GCHandleType.Pinned)
            try Gl.glBufferData(Gl.GL_ARRAY_BUFFER, nativeint (quadVerts.Length * 4), handle.AddrOfPinnedObject(), Gl.GL_DYNAMIC_DRAW)
            finally handle.Free()
            Gl.glEnableVertexAttribArray(texShader.LocPos)
            if texShader.LocUV >= 0 then Gl.glEnableVertexAttribArray(texShader.LocUV)
            Gl.glVertexAttribPointer(texShader.LocPos, 2, Gl.GL_FLOAT, false, 16, nativeint 0)
            if texShader.LocUV >= 0 then Gl.glVertexAttribPointer(texShader.LocUV, 2, Gl.GL_FLOAT, false, 16, nativeint 8)
            Gl.glDrawArrays(Gl.GL_TRIANGLES, 0, 6)
            if texShader.LocUV >= 0 then Gl.glDisableVertexAttribArray(texShader.LocUV)
            Gl.glBindTexture(Gl.GL_TEXTURE_EXTERNAL_OES, 0)
            restoreMainState st

        | BlitTexture (texId, dstPos, w, h, opacity) ->
            // Draw a persistent compositor-managed GL_TEXTURE_2D (e.g. an SHM client
            // buffer uploaded once on commit). Identical to Blit but the texture is
            // supplied directly instead of uploaded from CPU pixels each frame.
            let x0 = float32 (float dstPos.X)
            let y0 = float32 (float dstPos.Y)
            let x1 = x0 + float32 w
            let y1 = y0 + float32 h
            let quadVerts = [| x0;y0;0.0f;0.0f; x1;y0;1.0f;0.0f; x1;y1;1.0f;1.0f; x0;y0;0.0f;0.0f; x1;y1;1.0f;1.0f; x0;y1;0.0f;1.0f |]
            let texShader = GlslCompiler.buildTexturedQuadShader ()
            Gl.glUseProgram(texShader.Program)
            Gl.glUniformMatrix4fv(texShader.LocMVP, 1, false, mvp)
            Gl.glActiveTexture(Gl.GL_TEXTURE0)
            Gl.glBindTexture(Gl.GL_TEXTURE_2D, int texId)
            if texShader.LocTex0 >= 0 then Gl.glUniform1i(texShader.LocTex0, 0)
            if texShader.LocOpacity >= 0 then Gl.glUniform1f(texShader.LocOpacity, float32 opacity)
            Gl.glBindBuffer(Gl.GL_ARRAY_BUFFER, st.Vbo)
            let handle = GCHandle.Alloc(quadVerts, GCHandleType.Pinned)
            try Gl.glBufferData(Gl.GL_ARRAY_BUFFER, nativeint (quadVerts.Length * 4), handle.AddrOfPinnedObject(), Gl.GL_DYNAMIC_DRAW)
            finally handle.Free()
            Gl.glEnableVertexAttribArray(texShader.LocPos)
            if texShader.LocUV >= 0 then Gl.glEnableVertexAttribArray(texShader.LocUV)
            Gl.glVertexAttribPointer(texShader.LocPos, 2, Gl.GL_FLOAT, false, 16, nativeint 0)
            if texShader.LocUV >= 0 then Gl.glVertexAttribPointer(texShader.LocUV, 2, Gl.GL_FLOAT, false, 16, nativeint 8)
            Gl.glDrawArrays(Gl.GL_TRIANGLES, 0, 6)
            if texShader.LocUV >= 0 then Gl.glDisableVertexAttribArray(texShader.LocUV)
            Gl.glBindTexture(Gl.GL_TEXTURE_2D, 0)
            restoreMainState st

    let create () : Backend =
        let st : GlState =
            { W = 1; H = 1; Egl = { Egl.Display = nativeint 0; Surface = nativeint 0; Context = nativeint 0 }
              Program = 0; LocPos = 0; LocMVP = 0; LocColor = 0; Vbo = 0
              Fbo = 0; ColorRb = 0; StencilRb = 0
              FboPool = createPool 1 1; ActiveFbo = 0; TextAtlasTex = 0
              TexCache = System.Collections.Generic.Dictionary<uint32[], int>() }
        let mutable softFb = Framebuffer.create 1 1 Color.black
        let mutable orthoBase = ortho 1.0f 1.0f

        { Init = fun w h ->
            st.W <- w; st.H <- h
            // Tear down previous context if any
            if st.Egl.Display <> nativeint 0 then Egl.teardown st.Egl
            st.Egl <- Egl.init w h
            flushTexCache st
            GlslCompiler.clearCaches ()
            TextShader.clearCache ()
            BitmapTextShader.clearCache ()
            SdfAtlas.clearCaches ()
            resetBitmapAtlasGl ()
            softFb <- Framebuffer.create w h { R = 0uy; G = 0uy; B = 0uy; A = 0uy }
            orthoBase <- ortho (float32 w) (float32 h)
            destroyPool st.FboPool
            st.FboPool <- createPool w h

            // Render directly to the EGL pbuffer surface (FBO 0)
            // The pbuffer already has RGBA8 + stencil8 from the EGL config
            Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, 0)
            Gl.glViewport(0, 0, w, h)

            // Compile shaders
            let vs = Gl.compileShader Gl.GL_VERTEX_SHADER vertSrc
            let fs = Gl.compileShader Gl.GL_FRAGMENT_SHADER fragSrc
            st.Program <- Gl.linkProgram vs fs
            Gl.glUseProgram(st.Program)

            st.LocPos <- Gl.glGetAttribLocation(st.Program, "aPos")
            st.LocMVP <- Gl.glGetUniformLocation(st.Program, "uMVP")
            st.LocColor <- Gl.glGetUniformLocation(st.Program, "uColor")

            // VBO
            let mutable vbo = 0
            Gl.glGenBuffers(1, &vbo); st.Vbo <- vbo

            Gl.glEnableVertexAttribArray(st.LocPos)

            // Premultiplied alpha blending
            Gl.glEnable(Gl.GL_BLEND)
            Gl.glBlendFunc(Gl.GL_ONE, Gl.GL_ONE_MINUS_SRC_ALPHA)

          InitInto = fun _target -> failwith "GlBackend does not support InitInto"

          Submit = fun cmds ->
            cmds |> List.iter (dispatch st orthoBase Matrix3x2.identity softFb)

          Finish = fun () ->
            Gl.glFinish()

            // Read GL pixels
            let fb = Framebuffer.create st.W st.H Color.black
            let pixelBytes = Array.zeroCreate<byte> (st.W * st.H * 4)
            let h = GCHandle.Alloc(pixelBytes, GCHandleType.Pinned)
            try Gl.glReadPixels(0, 0, st.W, st.H, Gl.GL_RGBA, Gl.GL_UNSIGNED_BYTE, h.AddrOfPinnedObject())
            finally h.Free()

            // GL reads bottom-up; flip and merge with software fallback
            let fpx = fb.Px.Span
            let spx = softFb.Px.Span
            for y = 0 to st.H - 1 do
                let srcRow = (st.H - 1 - y) * st.W * 4
                for x = 0 to st.W - 1 do
                    let si = srcRow + x * 4
                    let glPx = Pixel.pack pixelBytes.[si+2] pixelBytes.[si+1] pixelBytes.[si] pixelBytes.[si+3]
                    let swPx = spx.[y * st.W + x]
                    fpx.[y * st.W + x] <- Raster.blendPx swPx glPx

            flushTexCache st
            destroyPool st.FboPool
            Egl.teardown st.Egl
            st.Egl <- { Egl.Display = nativeint 0; Surface = nativeint 0; Context = nativeint 0 }
            fb }

    /// Persistent result: Backend + accessors for the FBO color texture and readback
    type PersistentGlBackend =
        { Backend: Backend
          /// Get the GL texture ID of the FBO color attachment (for CUDA interop)
          GetColorTexture: unit -> int
          /// Blit the main FBO to a Y-flipped texture and return its GL texture ID.
          /// Must be called after Finish(), with EGL context current.
          GetFlippedTexture: unit -> int
          /// Copy the main FBO to the EGL window surface (FBO 0) and swap buffers.
          /// Only meaningful when the backend was created on a window surface
          /// (createPersistentOn with a GBM-backed EGL surface). Returns the
          /// eglSwapBuffers result.
          Present: unit -> bool
          /// Read back the FBO contents as a Framebuffer (software fallback)
          Readback: unit -> Framebuffer
          /// The EGL display this backend renders on (for dmabuf import / device query).
          /// Read live (valid once the context is initialized).
          EglDisplay: unit -> nativeint
          /// Clean up all GPU resources
          Destroy: unit -> unit }

    /// Create a persistent GL backend that keeps the EGL context alive across frames.
    /// Renders to an FBO with a color texture attachment (not renderbuffer) so the
    /// texture is accessible for CUDA-GL interop.
    /// When externalEgl is given, that context is used (and not torn down on Destroy);
    /// it must already be current-capable (e.g. created on a GBM surface for DRM scanout).
    let createPersistentImpl (externalEgl: Egl.EglContext option) (w: int) (h: int) : PersistentGlBackend =
        let st : GlState =
            { W = w; H = h; Egl = { Egl.Display = nativeint 0; Surface = nativeint 0; Context = nativeint 0 }
              Program = 0; LocPos = 0; LocMVP = 0; LocColor = 0; Vbo = 0
              Fbo = 0; ColorRb = 0; StencilRb = 0
              FboPool = createPool w h; ActiveFbo = 0; TextAtlasTex = 0
              TexCache = System.Collections.Generic.Dictionary<uint32[], int>() }
        let mutable softFb = Framebuffer.create w h { R = 0uy; G = 0uy; B = 0uy; A = 0uy }
        let mutable orthoBase = ortho (float32 w) (float32 h)
        let mutable colorTex = 0
        let mutable mainFbo = 0
        let mutable stencilRb = 0
        let mutable flippedTex = 0
        let mutable flippedFbo = 0
        let mutable softFbDirty = false
        let mutable initialized = false

        let initContext () =
            if initialized then ()
            else
            // Create EGL context (persists across frames), or adopt the caller's
            st.Egl <-
                match externalEgl with
                | Some egl ->
                    if not (Egl.eglMakeCurrent(egl.Display, egl.Surface, egl.Surface, egl.Context)) then
                        failwith $"eglMakeCurrent on external context failed (error 0x%X{Egl.eglGetError()})"
                    egl
                | None -> Egl.init w h
            st.W <- w; st.H <- h
            softFb <- Framebuffer.create w h { R = 0uy; G = 0uy; B = 0uy; A = 0uy }
            orthoBase <- ortho (float32 w) (float32 h)
            st.FboPool <- createPool w h

            // Create main FBO with color TEXTURE attachment (not renderbuffer)
            let mutable fbo = 0
            Gl.glGenFramebuffers(1, &fbo)
            mainFbo <- fbo
            Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, fbo)

            // Color attachment as texture (for CUDA interop)
            let mutable tex = 0
            Gl.glGenTextures(1, &tex)
            colorTex <- tex
            Gl.glBindTexture(Gl.GL_TEXTURE_2D, tex)
            Gl.glTexImage2D(Gl.GL_TEXTURE_2D, 0, Gl.GL_RGBA, w, h, 0, Gl.GL_RGBA, Gl.GL_UNSIGNED_BYTE, nativeint 0)
            Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_MIN_FILTER, Gl.GL_LINEAR)
            Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_MAG_FILTER, Gl.GL_LINEAR)
            Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_WRAP_S, Gl.GL_CLAMP_TO_EDGE)
            Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_WRAP_T, Gl.GL_CLAMP_TO_EDGE)
            Gl.glFramebufferTexture2D(Gl.GL_FRAMEBUFFER, Gl.GL_COLOR_ATTACHMENT0, Gl.GL_TEXTURE_2D, tex, 0)

            // Stencil attachment (for path rendering)
            let mutable srb = 0
            Gl.glGenRenderbuffers(1, &srb)
            stencilRb <- srb
            Gl.glBindRenderbuffer(Gl.GL_RENDERBUFFER, srb)
            Gl.glRenderbufferStorage(Gl.GL_RENDERBUFFER, Gl.GL_STENCIL_INDEX8, w, h)
            Gl.glFramebufferRenderbuffer(Gl.GL_FRAMEBUFFER, Gl.GL_STENCIL_ATTACHMENT, Gl.GL_RENDERBUFFER, srb)

            let status = Gl.glCheckFramebufferStatus(Gl.GL_FRAMEBUFFER)
            if status <> Gl.GL_FRAMEBUFFER_COMPLETE then
                eprintfn "[gl-persistent] FBO incomplete: 0x%X" status

            // Create a second FBO+texture for Y-flipped output (for NVENC GL-direct path)
            let mutable fbo2 = 0
            Gl.glGenFramebuffers(1, &fbo2)
            flippedFbo <- fbo2
            Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, fbo2)
            let mutable tex2 = 0
            Gl.glGenTextures(1, &tex2)
            flippedTex <- tex2
            Gl.glBindTexture(Gl.GL_TEXTURE_2D, tex2)
            Gl.glTexImage2D(Gl.GL_TEXTURE_2D, 0, Gl.GL_RGBA, w, h, 0, Gl.GL_RGBA, Gl.GL_UNSIGNED_BYTE, nativeint 0)
            Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_MIN_FILTER, Gl.GL_NEAREST)
            Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_MAG_FILTER, Gl.GL_NEAREST)
            Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_WRAP_S, Gl.GL_CLAMP_TO_EDGE)
            Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_WRAP_T, Gl.GL_CLAMP_TO_EDGE)
            Gl.glFramebufferTexture2D(Gl.GL_FRAMEBUFFER, Gl.GL_COLOR_ATTACHMENT0, Gl.GL_TEXTURE_2D, tex2, 0)
            let status2 = Gl.glCheckFramebufferStatus(Gl.GL_FRAMEBUFFER)
            if status2 <> Gl.GL_FRAMEBUFFER_COMPLETE then
                eprintfn "[gl-persistent] Flipped FBO incomplete: 0x%X" status2

            Gl.glBindTexture(Gl.GL_TEXTURE_2D, 0)
            Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, mainFbo)
            Gl.glViewport(0, 0, w, h)

            // Compile shaders
            let vs = Gl.compileShader Gl.GL_VERTEX_SHADER vertSrc
            let fs = Gl.compileShader Gl.GL_FRAGMENT_SHADER fragSrc
            st.Program <- Gl.linkProgram vs fs
            Gl.glUseProgram(st.Program)

            st.LocPos <- Gl.glGetAttribLocation(st.Program, "aPos")
            st.LocMVP <- Gl.glGetUniformLocation(st.Program, "uMVP")
            st.LocColor <- Gl.glGetUniformLocation(st.Program, "uColor")

            // VBO
            let mutable vbo = 0
            Gl.glGenBuffers(1, &vbo); st.Vbo <- vbo
            Gl.glEnableVertexAttribArray(st.LocPos)

            // Premultiplied alpha blending
            Gl.glEnable(Gl.GL_BLEND)
            Gl.glBlendFunc(Gl.GL_ONE, Gl.GL_ONE_MINUS_SRC_ALPHA)

            st.Fbo <- mainFbo
            st.ActiveFbo <- mainFbo
            initialized <- true

        let getFlippedTexture () =
            // Blit main FBO → flipped FBO with Y-flip (src top-to-bottom → dst bottom-to-top)
            Gl.glBindFramebuffer(Gl.GL_READ_FRAMEBUFFER, mainFbo)
            Gl.glBindFramebuffer(Gl.GL_DRAW_FRAMEBUFFER, flippedFbo)
            Gl.glBlitFramebuffer(0, 0, st.W, st.H, 0, st.H, st.W, 0, Gl.GL_COLOR_BUFFER_BIT, Gl.GL_NEAREST)
            // Ensure blit completes before NVENC reads the texture via CUDA-GL interop
            Gl.glFinish()
            Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, mainFbo)
            flippedTex

        let present () =
            // Copy the composited scene (mainFbo texture) to the window surface and swap.
            // A 1:1 blit is orientation-correct: the winsys framebuffer is y-flipped by
            // the driver, which exactly undoes the FBO's top-left-origin scene layout.
            Gl.glBindFramebuffer(Gl.GL_READ_FRAMEBUFFER, mainFbo)
            Gl.glBindFramebuffer(Gl.GL_DRAW_FRAMEBUFFER, 0)
            Gl.glBlitFramebuffer(0, 0, st.W, st.H, 0, 0, st.W, st.H, Gl.GL_COLOR_BUFFER_BIT, Gl.GL_NEAREST)
            let blitErr = Gl.glGetError()
            if blitErr <> 0 then
                // ES2-only context (no glBlitFramebuffer): draw the FBO texture as a quad
                Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, 0)
                Gl.glViewport(0, 0, st.W, st.H)
                Gl.glDisable(Gl.GL_BLEND)
                let passthrough = GlslCompiler.buildPassthroughShader ()
                drawFullscreenQuad passthrough colorTex 0
                Gl.glEnable(Gl.GL_BLEND)
                Gl.glBlendFunc(Gl.GL_ONE, Gl.GL_ONE_MINUS_SRC_ALPHA)
            Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, mainFbo)
            Egl.eglSwapBuffers(st.Egl.Display, st.Egl.Surface)

        let readback () =
            Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, mainFbo)
            Gl.glFinish()

            let fb = Framebuffer.create st.W st.H Color.black
            let pixelBytes = Array.zeroCreate<byte> (st.W * st.H * 4)
            let h = GCHandle.Alloc(pixelBytes, GCHandleType.Pinned)
            try Gl.glReadPixels(0, 0, st.W, st.H, Gl.GL_RGBA, Gl.GL_UNSIGNED_BYTE, h.AddrOfPinnedObject())
            finally h.Free()

            // GL reads bottom-up; flip and merge with software fallback
            let fpx = fb.Px.Span
            let spx = softFb.Px.Span
            for y = 0 to st.H - 1 do
                let srcRow = (st.H - 1 - y) * st.W * 4
                for x = 0 to st.W - 1 do
                    let si = srcRow + x * 4
                    let glPx = Pixel.pack pixelBytes.[si+2] pixelBytes.[si+1] pixelBytes.[si] pixelBytes.[si+3]
                    let swPx = spx.[y * st.W + x]
                    fpx.[y * st.W + x] <- Raster.blendPx swPx glPx
            fb

        let destroy () =
            if initialized then
                flushTexCache st
                destroyPool st.FboPool
                let mutable f = mainFbo
                let mutable t = colorTex
                let mutable s = stencilRb
                let mutable f2 = flippedFbo
                let mutable t2 = flippedTex
                Gl.glDeleteFramebuffers(1, &f)
                Gl.glDeleteTextures(1, &t)
                Gl.glDeleteRenderbuffers(1, &s)
                Gl.glDeleteFramebuffers(1, &f2)
                Gl.glDeleteTextures(1, &t2)
                // External contexts are owned (and torn down) by the caller
                if externalEgl.IsNone then Egl.teardown st.Egl
                st.Egl <- { Egl.Display = nativeint 0; Surface = nativeint 0; Context = nativeint 0 }
                initialized <- false

        // Initialize immediately
        initContext ()

        let backend =
            { Init = fun _w _h ->
                // Context already initialized; just clear the software fallback and reset state
                softFbHasPixels <- false
                Raster.clearFb softFb { R = 0uy; G = 0uy; B = 0uy; A = 0uy }
                // Ensure EGL context is current
                Egl.eglMakeCurrent(st.Egl.Display, st.Egl.Surface, st.Egl.Surface, st.Egl.Context) |> ignore
                // Flush texture cache so Wayland surface blits re-upload current pixel data
                // (managedFbCache reuses the same arrays with new content each frame)
                flushTexCache st
                // Bind main FBO and clear
                Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, mainFbo)
                st.ActiveFbo <- mainFbo
                Gl.glViewport(0, 0, st.W, st.H)
                Gl.glClearColor(0.0f, 0.0f, 0.0f, 0.0f)
                Gl.glClear(Gl.GL_COLOR_BUFFER_BIT ||| Gl.GL_STENCIL_BUFFER_BIT)
                Gl.glUseProgram(st.Program)

              InitInto = fun _target -> failwith "Persistent GlBackend does not support InitInto"

              Submit = fun cmds ->
                cmds |> List.iter (dispatch st orthoBase Matrix3x2.identity softFb)

              Finish = fun () ->
                // Composite software fallback pixels onto the GL FBO before finishing.
                // softFb is in screen coordinates (y=0 at top, BGRA packed uint32).
                // GL FBO has origin at bottom-left, so we upload rows in reverse order.
                if softFbHasPixels then
                    let spx = softFb.Px.Span
                    // Convert BGRA→RGBA and flip Y for GL upload
                    let w, h = softFb.W, softFb.H
                    let bytes = Array.zeroCreate<byte> (w * h * 4)
                    for y = 0 to h - 1 do
                        let srcRow = y * w
                        let dstRow = (h - 1 - y) * w * 4
                        for x = 0 to w - 1 do
                            let px = spx.[srcRow + x]
                            let di = dstRow + x * 4
                            bytes.[di] <- Pixel.r px
                            bytes.[di+1] <- Pixel.g px
                            bytes.[di+2] <- Pixel.b px
                            bytes.[di+3] <- Pixel.a px
                    let mutable tex = 0
                    Gl.glGenTextures(1, &tex)
                    Gl.glBindTexture(Gl.GL_TEXTURE_2D, tex)
                    let pin = GCHandle.Alloc(bytes, GCHandleType.Pinned)
                    try Gl.glTexImage2D(Gl.GL_TEXTURE_2D, 0, Gl.GL_RGBA, w, h, 0, Gl.GL_RGBA, Gl.GL_UNSIGNED_BYTE, pin.AddrOfPinnedObject())
                    finally pin.Free()
                    Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_MIN_FILTER, Gl.GL_NEAREST)
                    Gl.glTexParameteri(Gl.GL_TEXTURE_2D, Gl.GL_TEXTURE_MAG_FILTER, Gl.GL_NEAREST)
                    Gl.glBindTexture(Gl.GL_TEXTURE_2D, 0)
                    // Draw onto main FBO with alpha blending (premultiplied)
                    Gl.glBindFramebuffer(Gl.GL_FRAMEBUFFER, mainFbo)
                    Gl.glViewport(0, 0, w, h)
                    let passthrough = GlslCompiler.buildPassthroughShader ()
                    drawFullscreenQuad passthrough tex 0
                    Gl.glDeleteTextures(1, &tex)
                    restoreMainState st

                // glFinish to ensure all commands complete, but do NOT tear down EGL
                Gl.glFinish()
                // Return empty framebuffer (use Readback for actual pixels)
                Framebuffer.create 1 1 Color.black }

        { Backend = backend
          GetColorTexture = fun () -> colorTex
          GetFlippedTexture = getFlippedTexture
          Present = present
          Readback = readback
          EglDisplay = fun () -> st.Egl.Display
          Destroy = destroy }

    /// Persistent GL backend with its own pbuffer EGL context (headless compositing)
    let createPersistent (w: int) (h: int) : PersistentGlBackend =
        createPersistentImpl None w h

    /// Persistent GL backend on a caller-owned EGL context/surface (e.g. GBM window
    /// surface for DRM scanout). Present() then pushes frames to that surface.
    let createPersistentOn (egl: Egl.EglContext) (w: int) (h: int) : PersistentGlBackend =
        createPersistentImpl (Some egl) w h
