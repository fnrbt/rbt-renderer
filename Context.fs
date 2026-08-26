namespace Rbt.Renderer

type ContextState =
    { Transform: Matrix3x2
      FillPaint: Paint
      StrokeStyle: StrokeStyle
      FillRule: FillRule
      TextFonts: Text.FontData list
      TextSize: float }

type DrawContext() =
    let cmds = ResizeArray<Cmd>()
    let mutable xform = Matrix3x2.identity
    let path = ResizeArray<PathSeg>()
    let mutable fillPaint : Paint = Solid Color.black
    let mutable strokeStyle : StrokeStyle =
        { Width = 1.0<px>; Cap = Butt; Join = Miter; Paint = Solid Color.black
          DashArray = []; DashOffset = 0.0<px>; MiterLimit = 4.0 }
    let mutable fillRule = EvenOdd
    let mutable textFonts : Text.FontData list = []
    let mutable textSize : float = 14.0
    let stateStack = System.Collections.Generic.Stack<ContextState>()

    let isIdentity (m: Matrix3x2) =
        m.M11 = 1.0 && m.M12 = 0.0 && m.M21 = 0.0 && m.M22 = 1.0 && m.DX = 0.0 && m.DY = 0.0

    let wrap cmd =
        if isIdentity xform then cmd
        else Group(xform, 1.0, [cmd])

    member _.Save() =
        stateStack.Push {
            Transform = xform
            FillPaint = fillPaint
            StrokeStyle = strokeStyle
            FillRule = fillRule
            TextFonts = textFonts
            TextSize = textSize
        }

    member _.Restore() =
        if stateStack.Count > 0 then
            let s = stateStack.Pop()
            xform <- s.Transform
            fillPaint <- s.FillPaint
            strokeStyle <- s.StrokeStyle
            fillRule <- s.FillRule
            textFonts <- s.TextFonts
            textSize <- s.TextSize

    member _.Translate(dx, dy) = xform <- Matrix3x2.multiply xform (Matrix3x2.translate dx dy)
    member _.Rotate(angle) = xform <- Matrix3x2.multiply xform (Matrix3x2.rotate angle)
    member _.Scale(sx, sy) = xform <- Matrix3x2.multiply xform (Matrix3x2.scale sx sy)
    member _.ResetTransform() = xform <- Matrix3x2.identity

    member _.MoveTo(x, y) = path.Add(MoveTo(vec2 x y))
    member _.LineTo(x, y) = path.Add(LineTo(vec2 x y))
    member _.QuadTo(cpx, cpy, x, y) = path.Add(QuadTo(vec2 cpx cpy, vec2 x y))
    member _.CubicTo(c1x, c1y, c2x, c2y, x, y) = path.Add(CubicTo(vec2 c1x c1y, vec2 c2x c2y, vec2 x y))
    member _.ArcTo(rx, ry, rot, la, sw, x, y) = path.Add(ArcTo(rx, ry, rot, la, sw, vec2 x y))
    member _.ClosePath() = path.Add(Close)
    member _.ClearPath() = path.Clear()

    member _.Fill() =
        if path.Count > 0 then
            cmds.Add(wrap (FillPath(Seq.toList path, fillPaint, fillRule)))
            path.Clear()

    member _.Stroke() =
        if path.Count > 0 then
            cmds.Add(wrap (StrokePath(Seq.toList path, strokeStyle)))
            path.Clear()

    member _.FillRect(x, y, w, h) =
        let p = [ MoveTo(vec2 x y); LineTo(vec2 (x+w) y); LineTo(vec2 (x+w) (y+h)); LineTo(vec2 x (y+h)); Close ]
        cmds.Add(wrap (FillPath(p, fillPaint, fillRule)))

    member _.StrokeRect(x, y, w, h) =
        let p = [ MoveTo(vec2 x y); LineTo(vec2 (x+w) y); LineTo(vec2 (x+w) (y+h)); LineTo(vec2 x (y+h)); Close ]
        cmds.Add(wrap (StrokePath(p, strokeStyle)))

    member _.FillCircle(cx, cy, r) =
        cmds.Add(wrap (Cmd.FillCircle(vec2 cx cy, r * 1.0<px>, match fillPaint with Solid c -> c | _ -> Color.black)))

    member _.Clear(c) = cmds.Add(Clear c)

    member _.Emit(cmd) = cmds.Add(wrap cmd)

    member _.SetFill(paint) = fillPaint <- paint
    member _.SetStrokeStyle(style) = strokeStyle <- style
    member _.SetStrokeWidth(w) = strokeStyle <- { strokeStyle with Width = w * 1.0<px> }
    member _.SetStrokePaint(paint) = strokeStyle <- { strokeStyle with Paint = paint }
    member _.SetStrokeCap(cap) = strokeStyle <- { strokeStyle with Cap = cap }
    member _.SetStrokeJoin(join) = strokeStyle <- { strokeStyle with Join = join }
    member _.SetDash(arr, offset) = strokeStyle <- { strokeStyle with DashArray = arr; DashOffset = offset }
    member _.SetFillRule(rule) = fillRule <- rule
    member _.SetFont(font, size) = textFonts <- [ font ]; textSize <- size
    member _.SetFonts(fonts, size) = textFonts <- fonts; textSize <- size
    member _.FillText(x, y, str) =
        match textFonts with
        | font :: [] ->
            let runs = Text.layout font textSize str
            cmds.Add(wrap (DrawGlyphs(vec2 x y, runs, fillPaint)))
        | _ :: _ ->
            let runs = Text.layoutWithFallback textFonts textSize str
            cmds.Add(wrap (DrawGlyphs(vec2 x y, runs, fillPaint)))
        | [] -> ()

    member _.ToCmds() : Cmd list = Seq.toList cmds

    member this.Render(backend: Backend, w, h) =
        backend.Init w h
        backend.Submit (this.ToCmds())
        backend.Finish()

[<AutoOpen>]
module Ctx =
    let ctx () = DrawContext()

module Context =
    let save (c: DrawContext) = c.Save(); c
    let restore (c: DrawContext) = c.Restore(); c
    let translate dx dy (c: DrawContext) = c.Translate(dx, dy); c
    let rotate angle (c: DrawContext) = c.Rotate(angle); c
    let scale sx sy (c: DrawContext) = c.Scale(sx, sy); c
    let resetTransform (c: DrawContext) = c.ResetTransform(); c
    let moveTo x y (c: DrawContext) = c.MoveTo(x, y); c
    let lineTo x y (c: DrawContext) = c.LineTo(x, y); c
    let quadTo cpx cpy x y (c: DrawContext) = c.QuadTo(cpx, cpy, x, y); c
    let cubicTo c1x c1y c2x c2y x y (c: DrawContext) = c.CubicTo(c1x, c1y, c2x, c2y, x, y); c
    let arcTo rx ry rot la sw x y (c: DrawContext) = c.ArcTo(rx, ry, rot, la, sw, x, y); c
    let closePath (c: DrawContext) = c.ClosePath(); c
    let clearPath (c: DrawContext) = c.ClearPath(); c
    let fill (c: DrawContext) = c.Fill(); c
    let stroke (c: DrawContext) = c.Stroke(); c
    let fillRect x y w h (c: DrawContext) = c.FillRect(x, y, w, h); c
    let strokeRect x y w h (c: DrawContext) = c.StrokeRect(x, y, w, h); c
    let fillCircle cx cy r (c: DrawContext) = c.FillCircle(cx, cy, r); c
    let clear col (c: DrawContext) = c.Clear(col); c
    let emit cmd (c: DrawContext) = c.Emit(cmd); c
    let setFill paint (c: DrawContext) = c.SetFill(paint); c
    let setStrokeStyle style (c: DrawContext) = c.SetStrokeStyle(style); c
    let setStrokeWidth w (c: DrawContext) = c.SetStrokeWidth(w); c
    let setStrokePaint paint (c: DrawContext) = c.SetStrokePaint(paint); c
    let setStrokeCap cap (c: DrawContext) = c.SetStrokeCap(cap); c
    let setStrokeJoin join (c: DrawContext) = c.SetStrokeJoin(join); c
    let setDash arr offset (c: DrawContext) = c.SetDash(arr, offset); c
    let setFillRule rule (c: DrawContext) = c.SetFillRule(rule); c
    let setFont font size (c: DrawContext) = c.SetFont(font, size); c
    let setFonts fonts size (c: DrawContext) = c.SetFonts(fonts, size); c
    let fillText x y str (c: DrawContext) = c.FillText(x, y, str); c
    let toCmds (c: DrawContext) = c.ToCmds()
    let render backend w h (c: DrawContext) = c.Render(backend, w, h)
