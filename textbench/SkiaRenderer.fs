module TextBench.SkiaRenderer

open SkiaSharp
open Renderer

// ── Helpers ──

let private readPixels (surface: SKSurface) (w: int) (h: int) : Framebuffer =
    let fb = Framebuffer.create w h { R = 0uy; G = 0uy; B = 0uy; A = 0uy }
    use pixmap = surface.PeekPixels()
    let span = pixmap.GetPixelSpan<byte>()
    let px = fb.Px.Span
    for i = 0 to w * h - 1 do
        let off = i * 4
        px.[i] <- Pixel.pack span.[off + 2] span.[off + 1] span.[off] span.[off + 3]
    fb

// ── Uncached (loads typeface from file each call — used for quality comparison) ──

/// Render text to a Framebuffer using SkiaSharp.
/// x, y specify the baseline origin (y = baseline position from top).
let renderText (fontPath: string) (fontSize: float32) (text: string) (w: int) (h: int) (x: float32) (y: float32) : Framebuffer =
    use typeface = SKTypeface.FromFile(fontPath)
    use font = new SKFont(typeface, fontSize)
    font.Subpixel <- true
    use paint = new SKPaint()
    paint.Color <- SKColors.White
    paint.IsAntialias <- true
    let info = new SKImageInfo(w, h, SKColorType.Rgba8888, SKAlphaType.Premul)
    use surface = SKSurface.Create(info)
    let canvas = surface.Canvas
    canvas.Clear(SKColors.Transparent)
    canvas.DrawText(text, x, y, font, paint)
    readPixels surface w h

/// Measure the advance width of text using SkiaSharp.
let measureWidth (fontPath: string) (fontSize: float32) (text: string) : float =
    use typeface = SKTypeface.FromFile(fontPath)
    use font = new SKFont(typeface, fontSize)
    float (font.MeasureText(text))

/// Get the ascender value (positive, distance from baseline to top) for baseline alignment.
let ascender (fontPath: string) (fontSize: float32) : float32 =
    use typeface = SKTypeface.FromFile(fontPath)
    use font = new SKFont(typeface, fontSize)
    let metrics = font.Metrics
    -metrics.Ascent  // Skia reports ascent as negative, we want positive

// ── Cached (pre-loaded typeface — used for fair performance comparison) ──

type CachedSkia = {
    RenderText: float32 -> string -> int -> int -> float32 -> float32 -> Framebuffer
    MeasureWidth: float32 -> string -> float
    Ascender: float32 -> float32
}

/// Create a cached Skia wrapper that pre-loads the typeface once.
/// This gives Skia a fair comparison against our renderer which also pre-loads the font.
let createCached (fontPath: string) : CachedSkia =
    let typeface = SKTypeface.FromFile(fontPath)
    {
        RenderText = fun fontSize text w h x y ->
            use font = new SKFont(typeface, fontSize)
            font.Subpixel <- true
            use paint = new SKPaint()
            paint.Color <- SKColors.White
            paint.IsAntialias <- true
            let info = new SKImageInfo(w, h, SKColorType.Rgba8888, SKAlphaType.Premul)
            use surface = SKSurface.Create(info)
            surface.Canvas.Clear(SKColors.Transparent)
            surface.Canvas.DrawText(text, x, y, font, paint)
            readPixels surface w h

        MeasureWidth = fun fontSize text ->
            use font = new SKFont(typeface, fontSize)
            float (font.MeasureText(text))

        Ascender = fun fontSize ->
            use font = new SKFont(typeface, fontSize)
            -font.Metrics.Ascent
    }
