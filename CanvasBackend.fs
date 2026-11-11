module Rbt.Renderer.CanvasBackend

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Rbt.Renderer
open Rbt.Renderer.CanvasApi

// ── Helpers ──────────────────────────────────────────────

let private css (c: Color) =
    sprintf "rgba(%d,%d,%d,%g)" c.R c.G c.B (float c.A / 255.0)

let private fillRuleStr = function
    | EvenOdd -> "evenodd"
    | NonZero -> "nonzero"

let private applyStroke (ctx: CanvasRenderingContext2D) (st: StrokeStyle) =
    // PDF: width 0 = thinnest device line. Canvas draws nothing at 0,
    // so clamp to 1 device pixel (cf. pdf.js getSinglePixelWidth).
    ctx.lineWidth <-
        if st.Width = 0.0<px> then
            let det : float = emitJsExpr ctx "(function(t){return Math.abs(t.a*t.d - t.b*t.c)})(($0).getTransform())"
            1.0 / max 1e-6 (sqrt det)
        else float st.Width
    ctx.lineCap <-
        match st.Cap with
        | Butt -> "butt" | Round -> "round" | Square -> "square"
    ctx.lineJoin <-
        match st.Join with
        | Miter -> "miter" | Bevel -> "bevel" | RoundJoin -> "round"
    ctx.miterLimit <- st.MiterLimit
    ctxSetLineDash ctx (st.DashArray |> List.map float |> Array.ofList)
    ctxSetLineDashOffset ctx (float st.DashOffset)
    match st.Paint with
    | Solid c -> ctx.strokeStyle <- !^(css c)
    | _ -> ()

let private blendOperation = function
    | Normal -> "source-over" | Multiply -> "multiply" | Screen -> "screen"
    | Overlay -> "overlay" | Darken -> "darken" | Lighten -> "lighten"
    | ColorDodge -> "color-dodge" | ColorBurn -> "color-burn"
    | HardLight -> "hard-light" | SoftLight -> "soft-light"
    | Difference -> "difference" | Exclusion -> "exclusion"
    | Hue -> "hue" | Saturation -> "saturation" | ColorMode -> "color"
    | Luminosity -> "luminosity" | SrcIn -> "source-in" | SrcOut -> "source-out"
    | DstOver -> "destination-over" | DstAtop -> "destination-atop" | Xor -> "xor"

let private canvasSize (ctx: CanvasRenderingContext2D) : int * int =
    emitJsExpr ctx "[$0.canvas.width, $0.canvas.height]"

let private layerFor (ctx: CanvasRenderingContext2D) width height =
    let canvas = createOffscreenCanvas width height
    let layer = getOffscreenContext2D canvas
    emitJsExpr (layer, ctx) "$0.setTransform($1.getTransform())"
    canvas, layer

let private drawLayer (ctx: CanvasRenderingContext2D) canvas operation opacity =
    ctx.save()
    ctx.setTransform(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)
    ctx.globalAlpha <- ctx.globalAlpha * opacity
    emitJsExpr (ctx, operation, canvas) "($0.globalCompositeOperation = $1, $0.drawImage($2, 0, 0))"
    ctx.restore()

// A PDF object's shape is its geometric coverage, not its painted alpha.
// In particular a transparent solid fill still knocks out the earlier objects.
let rec private drawShape (ctx: CanvasRenderingContext2D) (cmd: Cmd) =
    match cmd with
    | Group (m, _, children) ->
        ctx.save()
        ctx.transform(m.M11, m.M12, m.M21, m.M22, m.DX, m.DY)
        for child in children do drawShape ctx child
        ctx.restore()
    | Clip (segs, rule, children) ->
        ctx.save()
        ctxClipPath ctx (toPath2D segs) (fillRuleStr rule)
        for child in children do drawShape ctx child
        ctx.restore()
    | TransparencyGroup (_, _, _, _, _, children)
    | Blend (_, children)
    | SaveRestore children
    | Filtered (_, children) ->
        for child in children do drawShape ctx child
    | Mask (_, children) ->
        for child in children do drawShape ctx child
    | FillPath (segs, paint, rule) ->
        match paint with
        | NoPaint -> ()
        | ImagePaint (image, placement) ->
            ctx.save()
            ctxClipPath ctx (toPath2D segs) (fillRuleStr rule)
            ctx.transform(placement.M11, placement.M12, placement.M21, placement.M22, placement.DX, placement.DY)
            emitJsExpr (ctx, CanvasImage.canvasFor image) "$0.drawImage($1, -0.5, -0.5)"
            ctx.restore()
        | _ ->
            ctx.fillStyle <- !^"white"
            ctxFillPath ctx (toPath2D segs) (fillRuleStr rule)
    | StrokePath (segs, style) ->
        match style.Paint with
        | NoPaint -> ()
        | _ ->
            applyStroke ctx style
            ctx.strokeStyle <- !^"white"
            ctxStrokePath ctx (toPath2D segs)
    | DrawGlyphs (origin, runs, paint) ->
        match paint with
        | NoPaint -> ()
        | _ ->
            ctx.fillStyle <- !^"white"
            for run in runs do
                let upm = Text.unitsPerEm run.Font
                if upm > 0.0 then
                    let s = run.FontSize / upm
                    for g in run.Glyphs do
                        match CanvasGlyphCache.get run.Font g.GlyphId with
                        | Some path ->
                            ctx.save()
                            ctx.translate(float origin.X + g.X, float origin.Y + g.Y)
                            ctx.scale(s, s)
                            ctxFillPath ctx path "nonzero"
                            ctx.restore()
                        | None -> ()
    | DrawImage (image, src, dst, smooth) ->
        ctx.save()
        ctx.imageSmoothingEnabled <- smooth
        ctxDrawImage9 ctx (CanvasImage.canvasFor image)
            (float src.Pos.X) (float src.Pos.Y) (float src.Size.X) (float src.Size.Y)
            (float dst.Pos.X) (float dst.Pos.Y) (float dst.Size.X) (float dst.Size.Y)
        ctx.restore()
    | FillRect (rect, _) ->
        ctx.fillStyle <- !^"white"
        ctx.fillRect(float rect.Pos.X, float rect.Pos.Y, float rect.Size.X, float rect.Size.Y)
    | Line (p0, p1, _, width) ->
        ctx.strokeStyle <- !^"white"
        ctx.lineWidth <- float width
        ctx.beginPath()
        ctx.moveTo(float p0.X, float p0.Y)
        ctx.lineTo(float p1.X, float p1.Y)
        ctx.stroke()
    | Clear _ ->
        ctx.fillStyle <- !^"white"
        emitJsExpr ctx "$0.fillRect(0, 0, $0.canvas.width, $0.canvas.height)"
    | _ -> ()

// Clip and transform wrappers can contain many PDF paint objects. Their
// children retain their own knockout and blend boundaries.
let rec private groupObjects = function
    | Clip (path, rule, children) ->
        children |> List.collect (fun child ->
            groupObjects child |> List.map (fun objectCmd -> Clip (path, rule, [objectCmd])))
    | Group (matrix, opacity, children) when opacity >= 1.0 ->
        children |> List.collect (fun child ->
            groupObjects child |> List.map (fun objectCmd -> Group (matrix, opacity, [objectCmd])))
    | SaveRestore children
    | AntiAlias (_, children) ->
        children |> List.collect (fun child -> groupObjects child)
    | Mask (maskCommands, children) ->
        children |> List.collect (fun child ->
            groupObjects child |> List.map (fun objectCmd -> Mask (maskCommands, [objectCmd])))
    | child -> [child]

// Blend is an attribute of the object, not of the empty temporary canvas.
// Peel the single-object clip/transform wrappers so the mode is applied where
// the object actually meets the group's working backdrop.
let rec private groupObject = function
    | Blend (mode, children) -> SaveRestore children, mode
    | Clip (path, rule, [child]) ->
        let source, mode = groupObject child
        Clip (path, rule, [source]), mode
    | Group (matrix, opacity, [child]) ->
        let source, mode = groupObject child
        Group (matrix, opacity, [source]), mode
    | Mask (maskCommands, [child]) ->
        let source, mode = groupObject child
        Mask (maskCommands, [source]), mode
    | child -> child, Normal

let rec private groupNeedsBackdrop = function
    | TransparencyGroup (isolated, _, _, mode, _, _) -> not isolated || mode <> Normal
    | Group (_, opacity, [child]) when opacity >= 1.0 -> groupNeedsBackdrop child
    | Clip (_, _, [child])
    | SaveRestore [child]
    | AntiAlias (_, [child]) -> groupNeedsBackdrop child
    | _ -> false

// These wrappers carry graphics state, not an isolated compositing surface.
let rec private groupStateOnly = function
    | SaveRestore children | AntiAlias (_, children) ->
        Group (Matrix3x2.identity, 1.0, List.map groupStateOnly children)
    | Group (matrix, opacity, children) when opacity >= 1.0 ->
        Group (matrix, opacity, List.map groupStateOnly children)
    | Clip (path, rule, children) -> Clip (path, rule, List.map groupStateOnly children)
    | child -> child

// Form the new object's result against the initial backdrop, then retain the
// prior group contribution only outside its shape. Work in premultiplied space.
let private knockoutComposite (working: CanvasRenderingContext2D) (backdrop: CanvasRenderingContext2D) (candidate: CanvasRenderingContext2D) shape width height =
    let pixels: obj = emitJsExpr (working, backdrop, candidate, shape, width, height) """(function(w,b,c,s,width,height){
        const wi=w.getImageData(0,0,width,height), bd=b.getImageData(0,0,width,height).data;
        const cd=c.getImageData(0,0,width,height).data, sh=s.getImageData(0,0,width,height).data, p=wi.data;
        for(let i=0;i<p.length;i+=4){
            const inv=1-sh[i+3]/255;
            const wa=p[i+3]/255, ba=bd[i+3]/255, ca=cd[i+3]/255;
            const a=Math.max(0,Math.min(1,ca+(wa-ba)*inv));
            for(let j=0;j<3;j++){
                const premul=cd[i+j]*ca+(p[i+j]*wa-bd[i+j]*ba)*inv;
                p[i+j]=a ? Math.max(0,Math.min(255,premul/a)) : 0;
            }
            p[i+3]=a*255;
        }
        return wi;
    })($0,$1,$2,$3,$4,$5)"""
    emitJsExpr (working, pixels) "$0.putImageData($1,0,0)"

let private knockoutAlpha (alpha: CanvasRenderingContext2D) (source: CanvasRenderingContext2D) (shape: CanvasRenderingContext2D) width height =
    emitJsExpr (alpha, source, shape, width, height) """(function(a,s,h,w,hgt){
        const image=a.getImageData(0,0,w,hgt), p=image.data;
        const sp=s.getImageData(0,0,w,hgt).data, hp=h.getImageData(0,0,w,hgt).data;
        for(let i=0;i<p.length;i+=4){
            p[i]=p[i+1]=p[i+2]=255;
            p[i+3]=Math.min(255,sp[i+3]+p[i+3]*(1-hp[i+3]/255));
        }
        a.putImageData(image,0,0);
    })($0,$1,$2,$3,$4)"""

// A non-isolated group's working surface already contains its original
// backdrop. Recover the group's premultiplied source from the working result
// and its independently accumulated opacity, then composite it just once.
let private extractContribution (working: CanvasRenderingContext2D) (backdrop: CanvasRenderingContext2D) (alpha: CanvasRenderingContext2D) width height =
    let pixels: obj = emitJsExpr (working, backdrop, alpha, width, height) """(function(w,b,g,width,height){
        const wi=w.getImageData(0,0,width,height), bd=b.getImageData(0,0,width,height).data;
        const ga=g.getImageData(0,0,width,height).data, p=wi.data;
        for(let i=0;i<p.length;i+=4){
            const a=ga[i+3]/255, wa=p[i+3]/255, ba=bd[i+3]/255;
            for(let j=0;j<3;j++)
                p[i+j]=a ? Math.max(0,Math.min(255,(p[i+j]*wa-bd[i+j]*ba*(1-a))/a)) : 0;
            p[i+3]=ga[i+3];
        }
        return wi;
    })($0,$1,$2,$3,$4)"""
    let result = createOffscreenCanvas width height
    let resultContext = getOffscreenContext2D result
    emitJsExpr (resultContext, pixels) "$0.putImageData($1,0,0)"
    result

// ── Main interpreter ─────────────────────────────────────

let rec private execute (ctx: CanvasRenderingContext2D) (cmd: Cmd) =
    match cmd with
    | Group (m, opacity, cmds) ->
        if opacity >= 1.0 then
            ctx.save()
            ctx.transform(m.M11, m.M12, m.M21, m.M22, m.DX, m.DY)
            for c in cmds do execute ctx c
            ctx.restore()
        else
            let width, height = canvasSize ctx
            let layer, layerContext = layerFor ctx width height
            layerContext.transform(m.M11, m.M12, m.M21, m.M22, m.DX, m.DY)
            for c in cmds do execute layerContext c
            drawLayer ctx layer "source-over" opacity

    | TransparencyGroup (isolated, knockout, opacity, mode, mask, children) ->
        let width, height = canvasSize ctx
        let backdrop =
            if isolated then None
            else
                let canvas, backdropContext = layerFor ctx width height
                // Copy the unmodified destination before any child is painted.
                emitJsExpr (backdropContext, ctx) "$0.save(); $0.setTransform(1,0,0,1,0,0); $0.drawImage($1.canvas, 0, 0); $0.restore()"
                Some (canvas, backdropContext)
        let working, workingContext = layerFor ctx width height
        match backdrop with
        | Some (canvas, _) -> drawLayer workingContext canvas "source-over" 1.0
        | None -> ()
        let alpha =
            if isolated then None
            else Some (snd (layerFor ctx width height))

        if isolated && not knockout then
            for child in children do execute workingContext (groupStateOnly child)
        else
            for child in children |> List.collect groupObjects do
                let sourceCmd, childMode = groupObject child
                let objectLayer, objectContext = layerFor ctx width height
                execute objectContext sourceCmd
                let needsBackdrop = groupNeedsBackdrop child
                let operation = blendOperation childMode
                if knockout then
                    let baseLayer, baseContext =
                        match backdrop with
                        | Some value -> value
                        | None -> layerFor ctx width height
                    let _, shapeContext = layerFor ctx width height
                    drawShape shapeContext child
                    let _, candidateContext = layerFor ctx width height
                    drawLayer candidateContext baseLayer "source-over" 1.0
                    if needsBackdrop then execute candidateContext child
                    else drawLayer candidateContext objectLayer operation 1.0
                    knockoutComposite workingContext baseContext candidateContext shapeContext width height
                    match alpha with
                    | Some alphaContext -> knockoutAlpha alphaContext objectContext shapeContext width height
                    | None -> ()
                else
                    if needsBackdrop then execute workingContext child
                    else drawLayer workingContext objectLayer operation 1.0
                    match alpha with
                    | Some alphaContext -> drawLayer alphaContext objectLayer "source-over" 1.0
                    | None -> ()

        let contribution =
            match backdrop, alpha with
            | Some (_, backdropContext), Some alphaContext ->
                extractContribution workingContext backdropContext alphaContext width height
            | _ -> working
        match mask with
        | Some maskCommands ->
            let maskLayer, maskContext = layerFor ctx width height
            for c in maskCommands do execute maskContext c
            let sourceContext = getOffscreenContext2D contribution
            drawLayer sourceContext maskLayer "destination-in" 1.0
        | None -> ()
        drawLayer ctx contribution (blendOperation mode) opacity

    | Clip (segs, fr, cmds) ->
        ctx.save()
        ctxClipPath ctx (toPath2D segs) (fillRuleStr fr)
        for c in cmds do execute ctx c
        ctx.restore()
    | Blend (mode, commands) ->
        let operation = blendOperation mode
        let width: int = emitJsExpr ctx "$0.canvas.width"
        let height: int = emitJsExpr ctx "$0.canvas.height"
        let layer = createOffscreenCanvas width height
        let layerContext = getOffscreenContext2D layer
        emitJsExpr (layerContext, ctx) "$0.setTransform($1.getTransform())"
        for c in commands do execute layerContext c
        ctx.save()
        ctx.setTransform(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)
        emitJsExpr (ctx, operation) "$0.globalCompositeOperation = $1"
        emitJsExpr (ctx, layer) "$0.drawImage($1, 0, 0)"
        ctx.restore()

    | SaveRestore commands ->
        let width: int = emitJsExpr ctx "$0.canvas.width"
        let height: int = emitJsExpr ctx "$0.canvas.height"
        let layer = createOffscreenCanvas width height
        let layerContext = getOffscreenContext2D layer
        emitJsExpr (layerContext, ctx) "$0.setTransform($1.getTransform())"
        for c in commands do execute layerContext c
        ctx.save()
        ctx.setTransform(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)
        emitJsExpr (ctx, layer) "$0.drawImage($1, 0, 0)"
        ctx.restore()

    | Filtered (filters, commands) ->
        let width: int = emitJsExpr ctx "$0.canvas.width"
        let height: int = emitJsExpr ctx "$0.canvas.height"
        let layer = createOffscreenCanvas width height
        let layerContext = getOffscreenContext2D layer
        emitJsExpr (layerContext, ctx) "$0.setTransform($1.getTransform())"
        for c in commands do execute layerContext c
        let mutable pixels = CanvasImage.pixelsFromCanvas layer
        for filter in filters do
            match filter with
            | Custom pass -> pixels <- pass width height pixels
            | _ -> invalidOp "CanvasBackend: unsupported image filter"
        let result = CanvasImage.canvasFor (Framebuffer.ofArray pixels width height)
        ctx.save()
        ctx.setTransform(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)
        emitJsExpr (ctx, result) "$0.drawImage($1, 0, 0)"
        ctx.restore()


    | Mask (maskCommands, commands) ->
        let width: int = emitJsExpr ctx "$0.canvas.width"
        let height: int = emitJsExpr ctx "$0.canvas.height"
        let mask = createOffscreenCanvas width height
        let layer = createOffscreenCanvas width height
        let maskContext = getOffscreenContext2D mask
        let layerContext = getOffscreenContext2D layer
        emitJsExpr (maskContext, ctx) "$0.setTransform($1.getTransform())"
        emitJsExpr (layerContext, ctx) "$0.setTransform($1.getTransform())"
        for c in maskCommands do execute maskContext c
        for c in commands do execute layerContext c
        layerContext.setTransform(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)
        emitJsExpr layerContext "$0.globalCompositeOperation = 'destination-in'"
        emitJsExpr (layerContext, mask) "$0.drawImage($1, 0, 0)"
        ctx.save()
        ctx.setTransform(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)
        emitJsExpr (ctx, layer) "$0.drawImage($1, 0, 0)"
        ctx.restore()

    | FillPath (segs, paint, fr) ->
        match paint with
        | Solid c ->
            ctx.fillStyle <- !^(css c)
            ctxFillPath ctx (toPath2D segs) (fillRuleStr fr)
        | LinearGradient (p0, p1, stops, Pad) ->
            let gradient = ctx.createLinearGradient(float p0.X, float p0.Y, float p1.X, float p1.Y)
            for stop in stops do
                gradient.addColorStop(stop.Offset, css stop.StopColor)
            ctx.fillStyle <- !^gradient
            ctxFillPath ctx (toPath2D segs) (fillRuleStr fr)
        | ImagePaint (image, placement) ->
            ctx.save()
            ctxClipPath ctx (toPath2D segs) (fillRuleStr fr)
            ctx.transform(placement.M11, placement.M12, placement.M21, placement.M22, placement.DX, placement.DY)
            // ImagePaint coordinates address pixel centres, Canvas addresses edges.
            emitJsExpr (ctx, CanvasImage.canvasFor image) "$0.drawImage($1, -0.5, -0.5)"
            ctx.restore()
        | TilingPaint (image, placement, Repeat, Repeat) ->
            let pattern = emitJsExpr (ctx, CanvasImage.canvasFor image) "$0.createPattern($1, 'repeat')"
            let dx = placement.DX - 0.5 * (placement.M11 + placement.M21)
            let dy = placement.DY - 0.5 * (placement.M12 + placement.M22)
            emitJsExpr (pattern, placement.M11, placement.M12, placement.M21, placement.M22, dx, dy) "$0.setTransform(new DOMMatrix([$1,$2,$3,$4,$5,$6]))"
            ctx.fillStyle <- !^(pattern: Browser.Types.CanvasPattern)
            ctxFillPath ctx (toPath2D segs) (fillRuleStr fr)
        | _ -> ()

    | StrokePath (segs, st) ->
        applyStroke ctx st
        ctxStrokePath ctx (toPath2D segs)

    | DrawGlyphs (origin, runs, paint) ->
        match paint with
        | Solid c ->
            ctx.fillStyle <- !^(css c)
            for run in runs do
                let upm = Text.unitsPerEm run.Font
                if upm > 0.0 then
                    let s = run.FontSize / upm
                    for g in run.Glyphs do
                        match CanvasGlyphCache.get run.Font g.GlyphId with
                        | Some path ->
                            ctx.save()
                            ctx.translate(float origin.X + g.X, float origin.Y + g.Y)
                            ctx.scale(s, s)
                            ctxFillPath ctx path "nonzero"
                            ctx.restore()
                        | None -> ()
        | _ -> ()

    | DrawImage (fb, srcR, dstR, smooth) ->
        let bmp = CanvasImage.canvasFor fb
        ctx.save()
        ctx.imageSmoothingEnabled <- smooth
        ctxDrawImage9 ctx bmp
            (float srcR.Pos.X) (float srcR.Pos.Y) (float srcR.Size.X) (float srcR.Size.Y)
            (float dstR.Pos.X) (float dstR.Pos.Y) (float dstR.Size.X) (float dstR.Size.Y)
        ctx.restore()

    | FillRect (r, c) ->
        ctx.fillStyle <- !^(css c)
        ctx.fillRect(float r.Pos.X, float r.Pos.Y, float r.Size.X, float r.Size.Y)

    | Clear c ->
        ctx.fillStyle <- !^(css c)
        // Fill the entire canvas (use a large rect; the caller should setTransform first)
        emitJsExpr ctx "$0.fillRect(0, 0, $0.canvas.width, $0.canvas.height)"

    | Line (p0, p1, c, w) ->
        ctx.strokeStyle <- !^(css c)
        ctx.lineWidth <- float w
        ctx.beginPath()
        ctx.moveTo(float p0.X, float p0.Y)
        ctx.lineTo(float p1.X, float p1.Y)
        ctx.stroke()

    | other ->
        let tag : string = emitJsExpr other "($0.tag !== undefined ? $0.tag : 'unknown')"
        warnOnce (sprintf "CanvasBackend: unsupported Cmd variant %s" tag)

/// Reset transform to identity and execute all commands.
let render (ctx: CanvasRenderingContext2D) (cmds: Cmd list) =
    ctx.setTransform(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)
    for c in cmds do execute ctx c

/// Rasterize one transparent pattern period; CanvasPattern repeats its pixels without seams.
let rasterize width height commands =
    let canvas = createOffscreenCanvas width height
    render (getOffscreenContext2DReadFrequent canvas) commands
    Framebuffer.ofArray (CanvasImage.pixelsFromCanvas canvas) width height
