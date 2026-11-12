namespace Rbt.Renderer

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
    /// Composite children as independent objects, then composite the group once.
    /// The optional mask modulates the group's contribution, not its backdrop.
    | TransparencyGroup of isolated: bool * knockout: bool * opacity: float * mode: BlendMode * mask: Cmd list option * cmds: Cmd list
    /// One object with independently supplied intrinsic coverage. The color commands
    /// execute directly in the current context; shape is evaluated only on demand.
    | WithShape of shape: Lazy<Cmd list> * cmds: Cmd list
    /// Children are already encoded in the group's working space. Convert only
    /// the group's source contribution at its parent-space compositing boundary.
    | ColorManagedGroup of conversion: ColorSpaceConversion * isolated: bool * knockout: bool * opacity: float * mode: BlendMode * mask: Cmd list option * cmds: Cmd list
    | Clip of clipPath: PathSeg list * fillRule: FillRule * cmds: Cmd list
    | Filtered of filters: Filter list * cmds: Cmd list
    | FillRoundedRect of Rect * cornerRadius: float<px> * Color
    | StrokeRoundedRect of Rect * cornerRadius: float<px> * StrokeStyle
    | FillRoundedRect4 of Rect * tl: float<px> * tr: float<px> * br: float<px> * bl: float<px> * Color
    | StrokeRoundedRect4 of Rect * tl: float<px> * tr: float<px> * br: float<px> * bl: float<px> * StrokeStyle
    | DrawImage of source: Framebuffer * srcRect: Rect * dstRect: Rect * smooth: bool
    /// Box-filter reductions in device space; smooth controls magnification only.
    | DrawImageReduced of source: Framebuffer * srcRect: Rect * dstRect: Rect * smooth: bool
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
#if !FABLE_COMPILER
    /// One retained imperative GLES layer. GL backends invoke it in scene order;
    /// software backends skip it. The stable layer object owns its GPU resources.
    | RetainedGlesLayer of bounds: Rect * layer: IGlesLayer
    /// Fill a rect with a custom fragment shader. The quotation must be
    /// `fun (vPos: Gpu.vec2) -> Gpu.vec4` — vPos receives pixel-space coords.
    | ShaderFill of rect: Rect * fragmentShader: Microsoft.FSharp.Quotations.Expr

and GlesLayerFrame =
    { Bounds: Rect
      Transform: Matrix3x2
      Projection: float32[]
      ViewportWidth: int
      ViewportHeight: int
      Framebuffer: int
      ContextId: nativeint }

and IGlesLayer =
    abstract Render: GlesLayerFrame -> unit
    abstract ContextLost: unit -> unit
    abstract Dispose: unit -> unit
    abstract SoftwareFallback: bounds:Rect -> Cmd list
#endif

module Cmd =
    /// The knockout shape is geometric coverage, not the painted object's opacity.
    /// Keep image alpha (it is part of an image's shape), but remove paint alpha and
    /// wrapper opacity while retaining transforms, clipping and antialiasing.
    let rec shapeOf (cmd: Cmd) : Cmd =
        let opaque (c: Color) = { c with A = 255uy }
        let shapePaint = function
            | NoPaint -> NoPaint
            | ImagePaint _ as paint -> paint
            | TilingPaint _ as paint -> paint
            | _ -> Solid Color.white
        let shapeStyle (style: StrokeStyle) = { style with Paint = shapePaint style.Paint }
        let shapeChildren children = List.map shapeOf children
        match cmd with
        | WithShape (shape, _) -> Group (Matrix3x2.identity, 1.0, shape.Value)
        | Clear _ -> Clear Color.white
        | Line (a, b, _, w) -> Line (a, b, Color.white, w)
        | FillRect (r, _) -> FillRect (r, Color.white)
        | StrokeRect (r, _, w) -> StrokeRect (r, Color.white, w)
        | FillCircle (center, radius, _) -> FillCircle (center, radius, Color.white)
        | FillTri (a, b, c, _) -> FillTri (a, b, c, Color.white)
        | FillPath (segs, paint, rule) -> FillPath (segs, shapePaint paint, rule)
        | StrokePath (segs, style) -> StrokePath (segs, shapeStyle style)
        | Group (xform, _, children) -> Group (xform, 1.0, shapeChildren children)
        | TransparencyGroup (_, _, _, _, _, children) ->
            Group (Matrix3x2.identity, 1.0, shapeChildren children)
        | ColorManagedGroup (_, _, _, _, _, _, children) ->
            Group (Matrix3x2.identity, 1.0, shapeChildren children)
        | Clip (path, rule, children) -> Clip (path, rule, shapeChildren children)
        | Filtered (filters, children) -> Filtered (filters, shapeChildren children)
        | FillRoundedRect (r, radius, _) -> FillRoundedRect (r, radius, Color.white)
        | StrokeRoundedRect (r, radius, style) -> StrokeRoundedRect (r, radius, shapeStyle style)
        | FillRoundedRect4 (r, tl, tr, br, bl, _) -> FillRoundedRect4 (r, tl, tr, br, bl, Color.white)
        | StrokeRoundedRect4 (r, tl, tr, br, bl, style) -> StrokeRoundedRect4 (r, tl, tr, br, bl, shapeStyle style)
        | DrawImage _ | DrawImageReduced _ -> cmd
        | BoxShadow (r, dx, dy, blur, spread, color, inset, radius) ->
            BoxShadow (r, dx, dy, blur, spread, opaque color, inset, radius)
        | PathBool (op, a, b, paint, rule) -> PathBool (op, a, b, shapePaint paint, rule)
        | Blit (src, pos, _) -> Blit (src, pos, 1.0)
        | BlitExternalTexture (tex, pos, w, h, _) -> BlitExternalTexture (tex, pos, w, h, 1.0)
        | BlitTexture (tex, pos, w, h, _) -> BlitTexture (tex, pos, w, h, 1.0)
        | Mask (mask, children) -> Mask (mask, shapeChildren children)
        | StrokeToFill (segs, style, paint, rule) ->
            StrokeToFill (segs, shapeStyle style, shapePaint paint, rule)
        | SaveRestore children -> SaveRestore (shapeChildren children)
        | Blend (_, children) -> SaveRestore (shapeChildren children)
        | AntiAlias (samples, children) -> AntiAlias (samples, shapeChildren children)
        | DrawGlyphs (origin, runs, paint) -> DrawGlyphs (origin, runs, shapePaint paint)
        | DrawBorder (r, top, right, bottom, left, tl, tr, br, bl) ->
            let side s = { s with Color = opaque s.Color }
            DrawBorder (r, side top, side right, side bottom, side left, tl, tr, br, bl)
        | BackdropFilter (path, rule, filters, children) ->
            BackdropFilter (path, rule, filters, shapeChildren children)
        | Group3D (xform, _, children) -> Group3D (xform, 1.0, shapeChildren children)
#if !FABLE_COMPILER
        | ShaderFill (r, _) -> FillRect (r, Color.white)
        | RetainedGlesLayer _ -> cmd
#endif
