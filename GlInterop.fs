namespace Rbt.Renderer

open System
open System.Runtime.InteropServices

module Egl =
    let [<Literal>] LibEGL = "libEGL.so.1"

    let EGL_OPENGL_ES_API        = 0x30A0
    let EGL_OPENGL_ES2_BIT       = 0x0004
    let EGL_SURFACE_TYPE         = 0x3033
    let EGL_PBUFFER_BIT          = 0x0001
    let EGL_RENDERABLE_TYPE      = 0x3040
    let EGL_ALPHA_SIZE           = 0x3021
    let EGL_BLUE_SIZE            = 0x3022
    let EGL_GREEN_SIZE           = 0x3023
    let EGL_RED_SIZE             = 0x3024
    let EGL_DEPTH_SIZE           = 0x3025
    let EGL_STENCIL_SIZE         = 0x3026
    let EGL_NONE                 = 0x3038
    let EGL_CONTEXT_CLIENT_VERSION = 0x3098
    let EGL_WIDTH                = 0x3057
    let EGL_HEIGHT               = 0x3056
    let EGL_DEFAULT_DISPLAY      = nativeint 0
    let EGL_NO_CONTEXT           = nativeint 0
    let EGL_NO_SURFACE           = nativeint 0

    [<DllImport(LibEGL)>] extern nativeint eglGetDisplay(nativeint displayId)
    [<DllImport(LibEGL)>] extern bool eglInitialize(nativeint display, int& major, int& minor)
    [<DllImport(LibEGL)>] extern bool eglChooseConfig(nativeint display, int[] attribs, nativeint[] configs, int configSize, int& numConfig)
    [<DllImport(LibEGL)>] extern nativeint eglCreatePbufferSurface(nativeint display, nativeint config, int[] attribs)
    [<DllImport(LibEGL)>] extern bool eglBindAPI(int api)
    [<DllImport(LibEGL)>] extern nativeint eglCreateContext(nativeint display, nativeint config, nativeint shareContext, int[] attribs)
    [<DllImport(LibEGL)>] extern bool eglMakeCurrent(nativeint display, nativeint draw, nativeint read, nativeint ctx)
    [<DllImport(LibEGL)>] extern nativeint eglGetProcAddress(string name)
    [<DllImport(LibEGL)>] extern bool eglSwapBuffers(nativeint display, nativeint surface)
    [<DllImport(LibEGL)>] extern bool eglDestroySurface(nativeint display, nativeint surface)
    [<DllImport(LibEGL)>] extern bool eglDestroyContext(nativeint display, nativeint ctx)
    [<DllImport(LibEGL)>] extern bool eglTerminate(nativeint display)
    [<DllImport(LibEGL)>] extern int eglGetError()

    type EglContext =
        { Display: nativeint; Surface: nativeint; Context: nativeint }

    // The default EGL display is a process-wide singleton. Initialize it ONCE and never
    // eglTerminate it: terminating the shared display corrupts subsequent eglInitialize on
    // Mesa/llvmpipe, which produced a garbage quadrant on the 2nd headless GL render per process.
    let mutable sharedDisplay : nativeint = nativeint 0
    let private ensureDisplay () : nativeint =
        if sharedDisplay <> nativeint 0 then sharedDisplay
        else
            let display = eglGetDisplay(EGL_DEFAULT_DISPLAY)
            if display = nativeint 0 then failwith $"eglGetDisplay failed (error 0x%X{eglGetError()})"
            let mutable major, minor = 0, 0
            if not (eglInitialize(display, &major, &minor)) then
                failwith $"eglInitialize failed (error 0x%X{eglGetError()})"
            sharedDisplay <- display
            display

    let init (w: int) (h: int) : EglContext =
        let display = ensureDisplay ()
        let configAttribs = [|
            EGL_SURFACE_TYPE; EGL_PBUFFER_BIT
            EGL_RENDERABLE_TYPE; EGL_OPENGL_ES2_BIT
            EGL_RED_SIZE; 8; EGL_GREEN_SIZE; 8; EGL_BLUE_SIZE; 8; EGL_ALPHA_SIZE; 8
            EGL_STENCIL_SIZE; 8
            EGL_NONE |]
        let configs = Array.zeroCreate<nativeint> 1
        let mutable numConfig = 0
        if not (eglChooseConfig(display, configAttribs, configs, 1, &numConfig)) || numConfig = 0 then
            failwith $"eglChooseConfig failed (error 0x%X{eglGetError()})"
        let config = configs.[0]
        eglBindAPI(EGL_OPENGL_ES_API) |> ignore
        let ctxAttribs = [| EGL_CONTEXT_CLIENT_VERSION; 2; EGL_NONE |]
        let ctx = eglCreateContext(display, config, EGL_NO_CONTEXT, ctxAttribs)
        if ctx = EGL_NO_CONTEXT then failwith $"eglCreateContext failed (error 0x%X{eglGetError()})"
        let surfAttribs = [| EGL_WIDTH; w; EGL_HEIGHT; h; EGL_NONE |]
        let surface = eglCreatePbufferSurface(display, config, surfAttribs)
        if surface = EGL_NO_SURFACE then failwith $"eglCreatePbufferSurface failed (error 0x%X{eglGetError()})"
        if not (eglMakeCurrent(display, surface, surface, ctx)) then
            failwith $"eglMakeCurrent failed (error 0x%X{eglGetError()})"
        { Display = display; Surface = surface; Context = ctx }

    let teardown (e: EglContext) =
        // Release + destroy only the per-render surface/context. Do NOT eglTerminate the
        // shared display (see ensureDisplay) — that breaks the next eglInitialize in-process.
        eglMakeCurrent(e.Display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT) |> ignore
        eglDestroySurface(e.Display, e.Surface) |> ignore
        eglDestroyContext(e.Display, e.Context) |> ignore

module Gl =
    let [<Literal>] LibGLES = "libGLESv2.so.2"

    // Constants
    let GL_COLOR_BUFFER_BIT   = 0x00004000
    let GL_STENCIL_BUFFER_BIT = 0x00000400
    let GL_DEPTH_BUFFER_BIT   = 0x00000100
    let GL_TRIANGLES          = 0x0004
    let GL_TRIANGLE_FAN       = 0x0006
    let GL_TRIANGLE_STRIP     = 0x0005
    let GL_LINES              = 0x0001
    let GL_LINE_STRIP         = 0x0003
    let GL_LINE_LOOP          = 0x0002
    let GL_FLOAT              = 0x1406
    let GL_UNSIGNED_BYTE      = 0x1401
    let GL_RGBA               = 0x1908
    let GL_VERTEX_SHADER      = 0x8B31
    let GL_FRAGMENT_SHADER    = 0x8B30
    let GL_COMPILE_STATUS     = 0x8B81
    let GL_LINK_STATUS        = 0x8B82
    let GL_FALSE              = 0
    let GL_TRUE               = 1
    let GL_STENCIL_TEST       = 0x0B90
    let GL_ALWAYS             = 0x0207
    let GL_EQUAL              = 0x0202
    let GL_NOTEQUAL           = 0x0205
    let GL_KEEP               = 0x1E00
    let GL_REPLACE            = 0x1E01
    let GL_INCR               = 0x1E02
    let GL_DECR               = 0x1E03
    let GL_INCR_WRAP          = 0x8507
    let GL_DECR_WRAP          = 0x8508
    let GL_ZERO               = 0
    let GL_INVERT             = 0x150A
    let GL_FRONT              = 0x0404
    let GL_BACK               = 0x0405
    let GL_CW                 = 0x0900
    let GL_CCW                = 0x0901
    let GL_BLEND              = 0x0BE2
    let GL_SRC_ALPHA          = 0x0302
    let GL_ONE_MINUS_SRC_ALPHA = 0x0303
    let GL_ONE                = 1
    let GL_FRAMEBUFFER        = 0x8D40
    let GL_RENDERBUFFER       = 0x8D41
    let GL_COLOR_ATTACHMENT0  = 0x8CE0
    let GL_STENCIL_ATTACHMENT = 0x8D20
    let GL_RGBA8              = 0x8058
    let GL_STENCIL_INDEX8     = 0x8D48
    let GL_FRAMEBUFFER_COMPLETE = 0x8CD5
    let GL_ARRAY_BUFFER       = 0x8892
    let GL_DYNAMIC_DRAW       = 0x88E8
    let GL_COLOR_WRITEMASK    = 0x0C23
    let GL_TEXTURE_2D         = 0x0DE1
    let GL_TEXTURE_EXTERNAL_OES = 0x8D65  // OES_EGL_image_external (dmabuf import)
    let GL_TEXTURE0           = 0x84C0
    let GL_TEXTURE1           = 0x84C1
    let GL_TEXTURE_MIN_FILTER = 0x2801
    let GL_TEXTURE_MAG_FILTER = 0x2800
    let GL_LINEAR             = 0x2601
    let GL_CLAMP_TO_EDGE      = 0x812F
    let GL_TEXTURE_WRAP_S     = 0x2802
    let GL_TEXTURE_WRAP_T     = 0x2803
    let GL_NEAREST            = 0x2600
    let GL_LUMINANCE          = 0x1909
    let GL_REPEAT             = 0x2901
    let GL_MIRRORED_REPEAT    = 0x8370

    // Core GL ES 2.0 functions
    [<DllImport(LibGLES)>] extern void glClearColor(float32 r, float32 g, float32 b, float32 a)
    [<DllImport(LibGLES)>] extern void glClear(int mask)
    [<DllImport(LibGLES)>] extern void glViewport(int x, int y, int w, int h)
    [<DllImport(LibGLES)>] extern void glEnable(int cap)
    [<DllImport(LibGLES)>] extern void glDisable(int cap)
    [<DllImport(LibGLES)>] extern byte glIsEnabled(int cap)
    [<DllImport(LibGLES)>] extern void glBlendFunc(int sfactor, int dfactor)
    [<DllImport(LibGLES)>] extern void glStencilFunc(int func, int ref', int mask)
    [<DllImport(LibGLES)>] extern void glStencilOp(int sfail, int dpfail, int dppass)
    [<DllImport(LibGLES)>] extern void glStencilOpSeparate(int face, int sfail, int dpfail, int dppass)
    [<DllImport(LibGLES)>] extern void glStencilMask(uint32 mask)
    [<DllImport(LibGLES)>] extern void glColorMask(bool r, bool g, bool b, bool a)
    [<DllImport(LibGLES)>] extern void glLineWidth(float32 w)
    [<DllImport(LibGLES)>] extern void glFinish()
    [<DllImport(LibGLES)>] extern void glReadPixels(int x, int y, int w, int h, int format, int typ, nativeint data)
    [<DllImport(LibGLES)>] extern void glDrawArrays(int mode, int first, int count)
    [<DllImport(LibGLES)>] extern int glGetError()

    // Shader functions
    [<DllImport(LibGLES)>] extern int glCreateShader(int typ)
    [<DllImport(LibGLES)>] extern void glShaderSource(int shader, int count, string[] source, nativeint lengths)
    [<DllImport(LibGLES)>] extern void glCompileShader(int shader)
    [<DllImport(LibGLES)>] extern void glGetShaderiv(int shader, int pname, int& param)
    [<DllImport(LibGLES)>] extern void glGetShaderInfoLog(int shader, int maxLen, int& len, byte[] infoLog)
    [<DllImport(LibGLES)>] extern int glCreateProgram()
    [<DllImport(LibGLES)>] extern void glAttachShader(int program, int shader)
    [<DllImport(LibGLES)>] extern void glLinkProgram(int program)
    [<DllImport(LibGLES)>] extern void glGetProgramiv(int program, int pname, int& param)
    [<DllImport(LibGLES)>] extern void glGetProgramInfoLog(int program, int maxLen, int& len, byte[] infoLog)
    [<DllImport(LibGLES)>] extern void glUseProgram(int program)
    [<DllImport(LibGLES)>] extern void glDeleteShader(int shader)
    [<DllImport(LibGLES)>] extern int glGetUniformLocation(int program, string name)
    [<DllImport(LibGLES)>] extern void glUniform1f(int loc, float32 v)
    [<DllImport(LibGLES)>] extern void glUniform2f(int loc, float32 v0, float32 v1)
    [<DllImport(LibGLES)>] extern void glUniform1i(int loc, int v)
    [<DllImport(LibGLES)>] extern void glUniform4f(int loc, float32 v0, float32 v1, float32 v2, float32 v3)
    [<DllImport(LibGLES)>] extern void glUniformMatrix4fv(int loc, int count, bool transpose, float32[] value)

    // Vertex attribute functions
    [<DllImport(LibGLES)>] extern int glGetAttribLocation(int program, string name)
    [<DllImport(LibGLES)>] extern void glEnableVertexAttribArray(int index)
    [<DllImport(LibGLES)>] extern void glDisableVertexAttribArray(int index)
    [<DllImport(LibGLES)>] extern void glVertexAttribPointer(int index, int size, int typ, bool normalized, int stride, nativeint pointer)

    // Buffer functions
    [<DllImport(LibGLES)>] extern void glGenBuffers(int n, int& buffers)
    [<DllImport(LibGLES)>] extern void glBindBuffer(int target, int buffer)
    [<DllImport(LibGLES)>] extern void glBufferData(int target, nativeint size, nativeint data, int usage)
    [<DllImport(LibGLES)>] extern void glDeleteBuffers(int n, int& buffers)

    // FBO functions
    [<DllImport(LibGLES)>] extern void glGenFramebuffers(int n, int& framebuffers)
    [<DllImport(LibGLES)>] extern void glBindFramebuffer(int target, int framebuffer)
    [<DllImport(LibGLES)>] extern void glGenRenderbuffers(int n, int& renderbuffers)
    [<DllImport(LibGLES)>] extern void glBindRenderbuffer(int target, int renderbuffer)
    [<DllImport(LibGLES)>] extern void glRenderbufferStorage(int target, int internalFormat, int w, int h)
    [<DllImport(LibGLES)>] extern void glFramebufferRenderbuffer(int target, int attachment, int renderbufferTarget, int renderbuffer)
    [<DllImport(LibGLES)>] extern int glCheckFramebufferStatus(int target)
    [<DllImport(LibGLES)>] extern void glDeleteFramebuffers(int n, int& framebuffers)
    [<DllImport(LibGLES)>] extern void glDeleteRenderbuffers(int n, int& renderbuffers)

    // Texture functions
    [<DllImport(LibGLES)>] extern void glGenTextures(int n, int& textures)
    [<DllImport(LibGLES)>] extern void glDeleteTextures(int n, int& textures)
    [<DllImport(LibGLES)>] extern void glBindTexture(int target, int texture)
    [<DllImport(LibGLES)>] extern void glTexImage2D(int target, int level, int internalformat, int w, int h, int border, int format, int typ, nativeint data)
    [<DllImport(LibGLES)>] extern void glTexParameteri(int target, int pname, int param)
    [<DllImport(LibGLES)>] extern void glActiveTexture(int texture)
    [<DllImport(LibGLES)>] extern void glFramebufferTexture2D(int target, int attachment, int textarget, int texture, int level)
    [<DllImport(LibGLES)>] extern void glCopyTexImage2D(int target, int level, int internalformat, int x, int y, int w, int h, int border)
    [<DllImport(LibGLES)>] extern void glTexSubImage2D(int target, int level, int xoff, int yoff, int w, int h, int format, int typ, nativeint data)

    // Blit (GLES 3.0)
    let GL_READ_FRAMEBUFFER   = 0x8CA8
    let GL_DRAW_FRAMEBUFFER   = 0x8CA9
    let GL_NEAREST_MIPMAP_NEAREST = 0x2700
    let GL_LINEAR_MIPMAP_NEAREST  = 0x2701
    let GL_NEAREST_MIPMAP_LINEAR  = 0x2702
    let GL_LINEAR_MIPMAP_LINEAR   = 0x2703
    [<DllImport(LibGLES)>] extern void glGenerateMipmap(int target)
    [<DllImport(LibGLES)>] extern void glBlitFramebuffer(int srcX0, int srcY0, int srcX1, int srcY1, int dstX0, int dstY0, int dstX1, int dstY1, int mask, int filter)

    // Helpers
    let compileShader (typ: int) (src: string) =
        let id = glCreateShader(typ)
        glShaderSource(id, 1, [| src |], nativeint 0)
        glCompileShader(id)
        let mutable status = 0
        glGetShaderiv(id, GL_COMPILE_STATUS, &status)
        if status = GL_FALSE then
            let buf = Array.zeroCreate<byte> 1024
            let mutable len = 0
            glGetShaderInfoLog(id, 1024, &len, buf)
            let msg = System.Text.Encoding.UTF8.GetString(buf, 0, len)
            failwith $"Shader compile error: {msg}"
        id

    let linkProgram (vs: int) (fs: int) =
        let prog = glCreateProgram()
        glAttachShader(prog, vs)
        glAttachShader(prog, fs)
        glLinkProgram(prog)
        let mutable status = 0
        glGetProgramiv(prog, GL_LINK_STATUS, &status)
        if status = GL_FALSE then
            let buf = Array.zeroCreate<byte> 1024
            let mutable len = 0
            glGetProgramInfoLog(prog, 1024, &len, buf)
            let msg = System.Text.Encoding.UTF8.GetString(buf, 0, len)
            failwith $"Program link error: {msg}"
        glDeleteShader(vs)
        glDeleteShader(fs)
        prog
