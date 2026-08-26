namespace Rbt.Renderer

module TextShader =

    let textVertSrc = """
attribute vec2 aPos;
attribute vec2 aUV;
uniform mat4 uMVP;
varying vec2 vUV;
void main() {
    vUV = aUV;
    gl_Position = uMVP * vec4(aPos, 0.0, 1.0);
}
"""

    let textFragSrc = """
#ifdef GL_OES_standard_derivatives
#extension GL_OES_standard_derivatives : enable
#endif
precision highp float;
uniform sampler2D uAtlas;
uniform vec4 uColor;
varying vec2 vUV;
void main() {
    float d = texture2D(uAtlas, vUV).r;
    float w = 0.35 * fwidth(d);
    float alpha = smoothstep(0.5 - w, 0.5 + w, d) * uColor.a;
    gl_FragColor = vec4(uColor.rgb * alpha, alpha);
}
"""

    type TextShaderProgram = {
        Program: int
        LocPos: int
        LocUV: int
        LocMVP: int
        LocAtlas: int
        LocColor: int
    }

    let mutable private cached : TextShaderProgram option = None

    let getTextShader () : TextShaderProgram =
        match cached with
        | Some s -> s
        | None ->
            let vs = Gl.compileShader Gl.GL_VERTEX_SHADER textVertSrc
            let fs = Gl.compileShader Gl.GL_FRAGMENT_SHADER textFragSrc
            let prog = Gl.linkProgram vs fs
            let s =
                { Program = prog
                  LocPos = Gl.glGetAttribLocation(prog, "aPos")
                  LocUV = Gl.glGetAttribLocation(prog, "aUV")
                  LocMVP = Gl.glGetUniformLocation(prog, "uMVP")
                  LocAtlas = Gl.glGetUniformLocation(prog, "uAtlas")
                  LocColor = Gl.glGetUniformLocation(prog, "uColor") }
            cached <- Some s
            s

    let clearCache () =
        cached <- None

/// Bitmap text shader — direct alpha sampling, no SDF smoothstep
module BitmapTextShader =

    let private fragSrc = """
precision highp float;
uniform sampler2D uAtlas;
uniform vec4 uColor;
varying vec2 vUV;
void main() {
    float alpha = texture2D(uAtlas, vUV).r * uColor.a;
    gl_FragColor = vec4(uColor.rgb * alpha, alpha);
}
"""

    let mutable private cached : TextShader.TextShaderProgram option = None

    let getShader () : TextShader.TextShaderProgram =
        match cached with
        | Some s -> s
        | None ->
            let vs = Gl.compileShader Gl.GL_VERTEX_SHADER TextShader.textVertSrc
            let fs = Gl.compileShader Gl.GL_FRAGMENT_SHADER fragSrc
            let prog = Gl.linkProgram vs fs
            let s : TextShader.TextShaderProgram =
                { Program = prog
                  LocPos = Gl.glGetAttribLocation(prog, "aPos")
                  LocUV = Gl.glGetAttribLocation(prog, "aUV")
                  LocMVP = Gl.glGetUniformLocation(prog, "uMVP")
                  LocAtlas = Gl.glGetUniformLocation(prog, "uAtlas")
                  LocColor = Gl.glGetUniformLocation(prog, "uColor") }
            cached <- Some s
            s

    let clearCache () =
        cached <- None
