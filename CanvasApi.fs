module Rbt.Renderer.CanvasApi

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Rbt.Renderer

// ── Path2D ───────────────────────────────────────────────

type Path2D =
    abstract moveTo: x: float * y: float -> unit
    abstract lineTo: x: float * y: float -> unit
    abstract quadraticCurveTo: cpx: float * cpy: float * x: float * y: float -> unit
    abstract bezierCurveTo: cp1x: float * cp1y: float * cp2x: float * cp2y: float * x: float * y: float -> unit
    abstract closePath: unit -> unit
    abstract addPath: path: Path2D -> unit

[<Emit("new Path2D()")>]
let createPath2D () : Path2D = jsNative

// ── OffscreenCanvas ──────────────────────────────────────

type OffscreenCanvas =
    abstract width: int with get
    abstract height: int with get

[<Emit("new OffscreenCanvas($0, $1)")>]
let createOffscreenCanvas (w: int) (h: int) : OffscreenCanvas = jsNative

[<Emit("$0.getContext('2d')")>]
let getOffscreenContext2D (canvas: OffscreenCanvas) : CanvasRenderingContext2D = jsNative

[<Emit("$0.getContext('2d', {willReadFrequently: true})")>]
let getOffscreenContext2DReadFrequent (canvas: OffscreenCanvas) : CanvasRenderingContext2D = jsNative

// ── ImageBitmap ──────────────────────────────────────────

type ImageBitmap =
    abstract width: int with get
    abstract height: int with get
    abstract close: unit -> unit

[<Emit("createImageBitmap($0)")>]
let createImageBitmap (blob: Browser.Types.Blob) : JS.Promise<ImageBitmap> = jsNative

// ── ImageData helpers ────────────────────────────────────

[<Emit("new ImageData($0, $1)")>]
let createImageData (data: JS.Uint8ClampedArray) (width: int) : ImageData = jsNative

[<Emit("new ImageData($0, $1)")>]
let createImageDataEmpty (width: int) (height: int) : ImageData = jsNative

// ── CanvasRenderingContext2D extensions ───────────────────
// Fable.Browser.Dom 2.17 lacks: fill/clip with Path2D+rule, stroke with Path2D, setLineDash

[<Emit("$0.fill($1, $2)")>]
let ctxFillPath (ctx: CanvasRenderingContext2D) (path: Path2D) (fillRule: string) : unit = jsNative

[<Emit("$0.clip($1, $2)")>]
let ctxClipPath (ctx: CanvasRenderingContext2D) (path: Path2D) (fillRule: string) : unit = jsNative

[<Emit("$0.stroke($1)")>]
let ctxStrokePath (ctx: CanvasRenderingContext2D) (path: Path2D) : unit = jsNative

[<Emit("$0.setLineDash($1)")>]
let ctxSetLineDash (ctx: CanvasRenderingContext2D) (segments: float[]) : unit = jsNative

[<Emit("$0.lineDashOffset = $1")>]
let ctxSetLineDashOffset (ctx: CanvasRenderingContext2D) (offset: float) : unit = jsNative

[<Emit("$0.drawImage($1, $2, $3, $4, $5, $6, $7, $8, $9)")>]
let ctxDrawImage9 (ctx: CanvasRenderingContext2D) (source: OffscreenCanvas) (sx: float) (sy: float) (sw: float) (sh: float) (dx: float) (dy: float) (dw: float) (dh: float) : unit = jsNative

[<Emit("$0.drawImage($1, $2, $3)")>]
let ctxDrawImageBitmap (ctx: CanvasRenderingContext2D) (bmp: ImageBitmap) (dx: float) (dy: float) : unit = jsNative

// ── Promise helpers ──────────────────────────────────────

[<Emit("Promise.resolve($0)")>]
let promiseReturn (x: 'T) : JS.Promise<'T> = jsNative

[<Emit("$1.then($0)")>]
let promiseBind (f: 'T -> JS.Promise<'U>) (p: JS.Promise<'T>) : JS.Promise<'U> = jsNative

[<Emit("Promise.all($0)")>]
let promiseAll (ps: JS.Promise<'T>[]) : JS.Promise<'T[]> = jsNative

[<Emit("$1.then($0)")>]
let promiseMap (f: 'T -> 'U) (p: JS.Promise<'T>) : JS.Promise<'U> = jsNative

[<Emit("$1.catch($0)")>]
let promiseCatch (f: obj -> JS.Promise<'T>) (p: JS.Promise<'T>) : JS.Promise<'T> = jsNative

// ── WeakMap helpers ──────────────────────────────────────

[<Emit("new WeakMap()")>]
let createWeakMap<'K, 'V> () : obj = jsNative

[<Emit("$0.has($1)")>]
let weakMapHas (map: obj) (key: obj) : bool = jsNative

[<Emit("$0.get($1)")>]
let weakMapGet<'V> (map: obj) (key: obj) : 'V = jsNative

[<Emit("$0.set($1, $2)")>]
let weakMapSet<'V> (map: obj) (key: obj) (value: 'V) : unit = jsNative

let weakMapTryGet<'V> (map: obj) (key: obj) : 'V option =
    if weakMapHas map key then Some (weakMapGet<'V> map key)
    else None

// ── Blob helper ──────────────────────────────────────────

[<Emit("new Blob([$0], {type: $1})")>]
let createBlob (data: byte[]) (mimeType: string) : Browser.Types.Blob = jsNative

// ── Warn-once helper ─────────────────────────────────────

let private warnedSet = System.Collections.Generic.HashSet<string>()

let warnOnce (msg: string) =
    if warnedSet.Add(msg) then
        emitJsExpr msg "console.warn($0)"

// ── PathSeg → Path2D conversion ──────────────────────────
// Shared by glyph caching and the canvas command interpreter.

let toPath2D (segs: PathSeg list) : Path2D =
    let parts = ResizeArray<string>()
    let number (value: float) = value.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
    let point (v: Vec2) = number (float v.X) + " " + number (float v.Y)
    for segment in segs do
        parts.Add(
            match segment with
            | MoveTo v -> "M " + point v
            | LineTo v -> "L " + point v
            | QuadTo (c, v) -> "Q " + point c + " " + point v
            | CubicTo (c1, c2, v) -> "C " + point c1 + " " + point c2 + " " + point v
            | ArcTo (rx, ry, rotation, largeArc, sweep, v) ->
                "A " + number (abs (float rx)) + " " + number (abs (float ry)) + " "
                + number (rotation * 180.0 / System.Math.PI) + " "
                + (if largeArc then "1" else "0") + " " + (if sweep then "1" else "0") + " " + point v
            | Close -> "Z")
    emitJsExpr (String.concat " " parts) "new Path2D($0)"
