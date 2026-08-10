namespace Renderer

type Cmd =
    | Clear of Color
    | Line of p0: Vec2 * p1: Vec2 * color: Color * width: float<px>
    | FillRect of Rect * Color
    | StrokeRect of Rect * Color * width: float<px>
    | FillCircle of center: Vec2 * radius: float<px> * Color
    | FillTri of Vec2 * Vec2 * Vec2 * Color
    | FillPath of PathSeg list * Paint * FillRule
    | StrokePath of PathSeg list * StrokeStyle
    | Group of xform: Matrix3x2 * opacity: float * cmds: Cmd list
    | Clip of clipPath: PathSeg list * fillRule: FillRule * cmds: Cmd list
    | Filtered of filters: Filter list * cmds: Cmd list
    | FillRoundedRect of Rect * cornerRadius: float<px> * Color
    | StrokeRoundedRect of Rect * cornerRadius: float<px> * StrokeStyle
    | FillRoundedRect4 of Rect * tl: float<px> * tr: float<px> * br: float<px> * bl: float<px> * Color
    | StrokeRoundedRect4 of Rect * tl: float<px> * tr: float<px> * br: float<px> * bl: float<px> * StrokeStyle
    | DrawImage of source: Framebuffer * srcRect: Rect * dstRect: Rect * smooth: bool
    | BoxShadow of rect: Rect * offsetX: float * offsetY: float * blur: int * spread: float<px> * color: Color * inset: bool * cornerRadius: float<px> option
    | PathBool of op: BoolOp * PathSeg list * PathSeg list * Paint * FillRule
    | Blit of source: Framebuffer * dstPos: Vec2 * opacity: float
    /// Draw a pre-imported external GL texture (an EGLImage from a client dmabuf,
    /// bound to GL_TEXTURE_EXTERNAL_OES) at dstPos. GL backend only; the software
    /// raster backend has no GL context and skips it.
    | BlitExternalTexture of texId: uint32 * dstPos: Vec2 * width: int * height: int * opacity: float
    /// Draw a persistent GL_TEXTURE_2D the compositor uploaded once (e.g. a client
    /// SHM buffer uploaded on commit, reused across frames — no per-frame re-upload).
    /// GL backend only; the software raster backend skips it (uses Blit instead).
    | BlitTexture of texId: uint32 * dstPos: Vec2 * width: int * height: int * opacity: float
    | Mask of maskCmds: Cmd list * cmds: Cmd list
    | StrokeToFill of PathSeg list * StrokeStyle * Paint * FillRule
    | SaveRestore of cmds: Cmd list
    | Blend of mode: BlendMode * cmds: Cmd list
    | AntiAlias of samples: int * cmds: Cmd list
    | DrawGlyphs of origin: Vec2 * runs: Text.GlyphRun list * paint: Paint
    | DrawBorder of rect: Rect * top: BorderSide * right: BorderSide * bottom: BorderSide * left: BorderSide * tl: float<px> * tr: float<px> * br: float<px> * bl: float<px>
    | BackdropFilter of clipPath: PathSeg list * fillRule: FillRule * filters: Filter list * cmds: Cmd list
    | Group3D of xform: Matrix4x4 * opacity: float * cmds: Cmd list
    /// Fill a rect with a custom fragment shader. The quotation must be
    /// `fun (vPos: Gpu.vec2) -> Gpu.vec4` — vPos receives pixel-space coords.
    | ShaderFill of rect: Rect * fragmentShader: Microsoft.FSharp.Quotations.Expr
