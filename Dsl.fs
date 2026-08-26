namespace Rbt.Renderer

// ── V1: Computation Expression ──

type DrawBuilder() =
    member _.Yield(_) = [] : Cmd list
    member _.Combine(a: Cmd list, b: Cmd list) = a @ b
    member _.Delay(f: unit -> Cmd list) = f ()
    [<CustomOperation("clear")>]
    member _.Clear(cmds, c) = cmds @ [ Clear c ]
    [<CustomOperation("line")>]
    member _.Line(cmds, p0, p1, c, w: float) = cmds @ [ Line(p0, p1, c, w * 1.0<px>) ]
    [<CustomOperation("fillRect")>]
    member _.FillRect(cmds, r, c) = cmds @ [ Cmd.FillRect(r, c) ]
    [<CustomOperation("strokeRect")>]
    member _.StrokeRect(cmds, r, c, w: float) = cmds @ [ StrokeRect(r, c, w * 1.0<px>) ]
    [<CustomOperation("fillCircle")>]
    member _.FillCircle(cmds, center, radius: float, c) = cmds @ [ Cmd.FillCircle(center, radius * 1.0<px>, c) ]
    [<CustomOperation("fillTri")>]
    member _.FillTri(cmds, a, b, c, col) = cmds @ [ Cmd.FillTri(a, b, c, col) ]
    [<CustomOperation("path")>]
    member _.Path(cmds, segs, paint) = cmds @ [ Cmd.FillPath(segs, paint, EvenOdd) ]
    [<CustomOperation("pathNonZero")>]
    member _.PathNonZero(cmds, segs, paint) = cmds @ [ Cmd.FillPath(segs, paint, NonZero) ]
    [<CustomOperation("strokePath")>]
    member _.StrokePath(cmds, segs, style) = cmds @ [ Cmd.StrokePath(segs, style) ]
    [<CustomOperation("fillRoundedRect")>]
    member _.FillRoundedRect(cmds, r, cr: float, c) = cmds @ [ Cmd.FillRoundedRect(r, cr * 1.0<px>, c) ]
    [<CustomOperation("strokeRoundedRect")>]
    member _.StrokeRoundedRect(cmds, r, cr: float, style) = cmds @ [ Cmd.StrokeRoundedRect(r, cr * 1.0<px>, style) ]
    [<CustomOperation("fillRoundedRect4")>]
    member _.FillRoundedRect4(cmds, r, tl: float, tr: float, br: float, bl: float, c) =
        cmds @ [ Cmd.FillRoundedRect4(r, tl * 1.0<px>, tr * 1.0<px>, br * 1.0<px>, bl * 1.0<px>, c) ]
    [<CustomOperation("strokeRoundedRect4")>]
    member _.StrokeRoundedRect4(cmds, r, tl: float, tr: float, br: float, bl: float, style) =
        cmds @ [ Cmd.StrokeRoundedRect4(r, tl * 1.0<px>, tr * 1.0<px>, br * 1.0<px>, bl * 1.0<px>, style) ]
    [<CustomOperation("fillEllipse")>]
    member _.FillEllipse(cmds, cx: float, cy: float, rx: float, ry: float, paint) =
        cmds @ [ Cmd.FillPath(Raster.ellipsePath cx cy rx ry, paint, EvenOdd) ]
    [<CustomOperation("strokeEllipse")>]
    member _.StrokeEllipse(cmds, cx: float, cy: float, rx: float, ry: float, style) =
        cmds @ [ Cmd.StrokePath(Raster.ellipsePath cx cy rx ry, style) ]
    [<CustomOperation("drawImage")>]
    member _.DrawImage(cmds, src, srcRect, dstRect, smooth) =
        cmds @ [ Cmd.DrawImage(src, srcRect, dstRect, smooth) ]
    [<CustomOperation("boxShadow")>]
    member _.BoxShadow(cmds, r, offsetX, offsetY, blur, spread: float, color, inset, cornerRadius) =
        cmds @ [ Cmd.BoxShadow(r, offsetX, offsetY, blur, spread * 1.0<px>, color, inset, cornerRadius |> Option.map (fun (cr: float) -> cr * 1.0<px>)) ]
    [<CustomOperation("group")>]
    member _.Group(cmds, xform, opacity, children: Cmd list) = cmds @ [ Cmd.Group(xform, opacity, children) ]
    [<CustomOperation("clip")>]
    member _.Clip(cmds, clipPath, children: Cmd list) = cmds @ [ Cmd.Clip(clipPath, EvenOdd, children) ]
    [<CustomOperation("clipNonZero")>]
    member _.ClipNonZero(cmds, clipPath, children: Cmd list) = cmds @ [ Cmd.Clip(clipPath, NonZero, children) ]
    [<CustomOperation("pathBool")>]
    member _.PathBool(cmds, op, segsA, segsB, paint) = cmds @ [ Cmd.PathBool(op, segsA, segsB, paint, EvenOdd) ]
    [<CustomOperation("blit")>]
    member _.Blit(cmds, src, dstPos, opacity) = cmds @ [ Cmd.Blit(src, dstPos, opacity) ]
    [<CustomOperation("mask")>]
    member _.Mask(cmds, maskCmds: Cmd list, children: Cmd list) = cmds @ [ Cmd.Mask(maskCmds, children) ]
    [<CustomOperation("strokeToFill")>]
    member _.StrokeToFill(cmds, segs, style, paint) = cmds @ [ Cmd.StrokeToFill(segs, style, paint, EvenOdd) ]
    [<CustomOperation("saveRestore")>]
    member _.SaveRestore(cmds, children: Cmd list) = cmds @ [ Cmd.SaveRestore(children) ]
    [<CustomOperation("filtered")>]
    member _.Filtered(cmds, filters, children: Cmd list) = cmds @ [ Cmd.Filtered(filters, children) ]
    [<CustomOperation("blendMode")>]
    member _.BlendMode(cmds, mode, children: Cmd list) = cmds @ [ Cmd.Blend(mode, children) ]
    [<CustomOperation("antiAlias")>]
    member _.AntiAlias(cmds, children: Cmd list) = cmds @ [ Cmd.AntiAlias(4, children) ]
    [<CustomOperation("antiAliasN")>]
    member _.AntiAliasN(cmds, samples, children: Cmd list) = cmds @ [ Cmd.AntiAlias(samples, children) ]
    [<CustomOperation("text")>]
    member _.Text(cmds, origin, font, size: float, str, paint) =
        cmds @ [ Cmd.DrawGlyphs(origin, Text.layout font size str, paint) ]
    [<CustomOperation("textFallback")>]
    member _.TextFallback(cmds, origin, fonts, size: float, str, paint) =
        cmds @ [ Cmd.DrawGlyphs(origin, Text.layoutWithFallback fonts size str, paint) ]
    [<CustomOperation("drawGlyphs")>]
    member _.DrawGlyphs(cmds, origin, runs, paint) =
        cmds @ [ Cmd.DrawGlyphs(origin, runs, paint) ]
    [<CustomOperation("drawBorder")>]
    member _.DrawBorder(cmds, r, top, right, bottom, left, tl: float, tr: float, br: float, bl: float) =
        cmds @ [ Cmd.DrawBorder(r, top, right, bottom, left, tl * 1.0<px>, tr * 1.0<px>, br * 1.0<px>, bl * 1.0<px>) ]
    [<CustomOperation("backdropFilter")>]
    member _.BackdropFilter(cmds, clipPath, filters, children: Cmd list) =
        cmds @ [ Cmd.BackdropFilter(clipPath, EvenOdd, filters, children) ]
    [<CustomOperation("group3d")>]
    member _.Group3D(cmds, xform, opacity, children: Cmd list) =
        cmds @ [ Cmd.Group3D(xform, opacity, children) ]

[<AutoOpen>]
module DslCE =
    let draw = DrawBuilder()

// ── V2: Pipeline operators ──

module Pipe =
    let clear c (cmds: Cmd list) = cmds @ [ Clear c ]
    let line' p0 p1 c (w: float) (cmds: Cmd list) = cmds @ [ Line(p0, p1, c, w * 1.0<px>) ]
    let fill r c (cmds: Cmd list) = cmds @ [ Cmd.FillRect(r, c) ]
    let stroke r c (w: float) (cmds: Cmd list) = cmds @ [ StrokeRect(r, c, w * 1.0<px>) ]
    let circle center (radius: float) c (cmds: Cmd list) = cmds @ [ Cmd.FillCircle(center, radius * 1.0<px>, c) ]
    let tri a b c col (cmds: Cmd list) = cmds @ [ Cmd.FillTri(a, b, c, col) ]
    let path' segs paint (cmds: Cmd list) = cmds @ [ Cmd.FillPath(segs, paint, EvenOdd) ]
    let pathNonZero segs paint (cmds: Cmd list) = cmds @ [ Cmd.FillPath(segs, paint, NonZero) ]
    let strokePath' segs style (cmds: Cmd list) = cmds @ [ Cmd.StrokePath(segs, style) ]
    let fillRoundedRect r (cr: float) c (cmds: Cmd list) = cmds @ [ Cmd.FillRoundedRect(r, cr * 1.0<px>, c) ]
    let strokeRoundedRect r (cr: float) style (cmds: Cmd list) = cmds @ [ Cmd.StrokeRoundedRect(r, cr * 1.0<px>, style) ]
    let fillRoundedRect4 r (tl: float) (tr: float) (br: float) (bl: float) c (cmds: Cmd list) =
        cmds @ [ Cmd.FillRoundedRect4(r, tl * 1.0<px>, tr * 1.0<px>, br * 1.0<px>, bl * 1.0<px>, c) ]
    let strokeRoundedRect4 r (tl: float) (tr: float) (br: float) (bl: float) style (cmds: Cmd list) =
        cmds @ [ Cmd.StrokeRoundedRect4(r, tl * 1.0<px>, tr * 1.0<px>, br * 1.0<px>, bl * 1.0<px>, style) ]
    let fillEllipse (cx: float) (cy: float) (rx: float) (ry: float) paint (cmds: Cmd list) =
        cmds @ [ Cmd.FillPath(Raster.ellipsePath cx cy rx ry, paint, EvenOdd) ]
    let strokeEllipse (cx: float) (cy: float) (rx: float) (ry: float) style (cmds: Cmd list) =
        cmds @ [ Cmd.StrokePath(Raster.ellipsePath cx cy rx ry, style) ]
    let drawImage src srcRect dstRect smooth (cmds: Cmd list) =
        cmds @ [ Cmd.DrawImage(src, srcRect, dstRect, smooth) ]
    let boxShadow r offsetX offsetY blur (spread: float) color inset (cornerRadius: float option) (cmds: Cmd list) =
        cmds @ [ Cmd.BoxShadow(r, offsetX, offsetY, blur, spread * 1.0<px>, color, inset, cornerRadius |> Option.map (fun cr -> cr * 1.0<px>)) ]
    let group' xform opacity children (cmds: Cmd list) = cmds @ [ Cmd.Group(xform, opacity, children) ]
    let clip clipPath children (cmds: Cmd list) = cmds @ [ Cmd.Clip(clipPath, EvenOdd, children) ]
    let clipNonZero clipPath children (cmds: Cmd list) = cmds @ [ Cmd.Clip(clipPath, NonZero, children) ]
    let pathBool op segsA segsB paint (cmds: Cmd list) = cmds @ [ Cmd.PathBool(op, segsA, segsB, paint, EvenOdd) ]
    let blit src dstPos opacity (cmds: Cmd list) = cmds @ [ Cmd.Blit(src, dstPos, opacity) ]
    let imagePath segs src xform (cmds: Cmd list) = cmds @ [ Cmd.FillPath(segs, ImagePaint(src, xform), EvenOdd) ]
    let mask maskCmds children (cmds: Cmd list) = cmds @ [ Cmd.Mask(maskCmds, children) ]
    let strokeToFill segs style paint (cmds: Cmd list) = cmds @ [ Cmd.StrokeToFill(segs, style, paint, EvenOdd) ]
    let saveRestore children (cmds: Cmd list) = cmds @ [ Cmd.SaveRestore(children) ]
    let filtered filters children (cmds: Cmd list) = cmds @ [ Cmd.Filtered(filters, children) ]
    let blendMode mode children (cmds: Cmd list) = cmds @ [ Cmd.Blend(mode, children) ]
    let antiAlias children (cmds: Cmd list) = cmds @ [ Cmd.AntiAlias(4, children) ]
    let antiAliasN samples children (cmds: Cmd list) = cmds @ [ Cmd.AntiAlias(samples, children) ]
    let text origin font size str paint (cmds: Cmd list) = cmds @ [ Cmd.DrawGlyphs(origin, Text.layout font size str, paint) ]
    let textFallback origin fonts size str paint (cmds: Cmd list) = cmds @ [ Cmd.DrawGlyphs(origin, Text.layoutWithFallback fonts size str, paint) ]
    let drawGlyphs origin runs paint (cmds: Cmd list) = cmds @ [ Cmd.DrawGlyphs(origin, runs, paint) ]
    let drawBorder r top right bottom left (tl: float) (tr: float) (br: float) (bl: float) (cmds: Cmd list) =
        cmds @ [ Cmd.DrawBorder(r, top, right, bottom, left, tl * 1.0<px>, tr * 1.0<px>, br * 1.0<px>, bl * 1.0<px>) ]
    let backdropFilter clipPath filters children (cmds: Cmd list) =
        cmds @ [ Cmd.BackdropFilter(clipPath, EvenOdd, filters, children) ]
    let group3d xform opacity children (cmds: Cmd list) = cmds @ [ Cmd.Group3D(xform, opacity, children) ]

// ── V3: Scene list (direct DU) + render helper ──

module Scene =
    let render (backend: Backend) w h (cmds: Cmd list) =
        backend.Init w h
        backend.Submit cmds
        backend.Finish ()
