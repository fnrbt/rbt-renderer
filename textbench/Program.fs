module TextBench.Program

open System
open System.IO
open Renderer
open TextBench.CompareMetrics

// ── PPM writer (same as Demo.writePpm) ──

let writePpm (path: string) (fb: Framebuffer) =
    use w = new StreamWriter(path)
    w.WriteLine $"P3\n{fb.W} {fb.H}\n255"
    let px = fb.Px.Span
    for i = 0 to fb.Px.Length - 1 do
        let c = Color.fromPremul (Pixel.toColor px.[i])
        w.WriteLine $"{c.R} {c.G} {c.B}"

// ── Render helpers ──

/// Render text with our CPU rasterizer to a Framebuffer.
let renderOurs (font: Text.FontData) (fontSize: float) (text: string) (w: int) (h: int) (x: float) (baselineY: float) : Framebuffer =
    let runs = Text.layout font fontSize text
    let cmds : Cmd list =
        [ Clear { R = 0uy; G = 0uy; B = 0uy; A = 0uy }
          DrawGlyphs (vec2 x baselineY, runs, Solid Color.white) ]
    let backend = Raster.create ()
    Scene.render backend w h cmds

// ── Test case definitions ──

type TestCase = {
    Name: string
    Category: string
    Text: string
    FontSizes: float list
}

let kerningCases = [
    { Name = "kern_AV";       Category = "Kerning"; Text = "AV AVAV";         FontSizes = [24.0; 48.0] }
    { Name = "kern_To";       Category = "Kerning"; Text = "To Ta Te Ty";     FontSizes = [24.0; 48.0] }
    { Name = "kern_WA";       Category = "Kerning"; Text = "WAWA";            FontSizes = [24.0; 48.0] }
    { Name = "kern_ff";       Category = "Kerning"; Text = "fff ffi ffl";     FontSizes = [24.0; 48.0] }
    { Name = "kern_narrow";   Category = "Kerning"; Text = "iiiiiiiiii";      FontSizes = [24.0; 48.0] }
    { Name = "kern_wide";     Category = "Kerning"; Text = "mmmmmmmmmm";      FontSizes = [24.0; 48.0] }
]

let ligatureCases = [
    { Name = "lig_standard";  Category = "Ligatures"; Text = "fi fl ffi ffl";    FontSizes = [24.0; 48.0] }
    { Name = "lig_context";   Category = "Ligatures"; Text = "office effect";    FontSizes = [24.0; 48.0] }
]

let rasterSizes = [10.0; 12.0; 14.0; 16.0; 24.0; 48.0; 96.0]

let rasterCases = [
    { Name = "raster_O"; Category = "Rasterization"; Text = "O"; FontSizes = rasterSizes }
    { Name = "raster_H"; Category = "Rasterization"; Text = "H"; FontSizes = rasterSizes }
    { Name = "raster_e"; Category = "Rasterization"; Text = "e"; FontSizes = rasterSizes }
    { Name = "raster_at"; Category = "Rasterization"; Text = "@"; FontSizes = [12.0; 24.0] }
]

let complexCases = [
    { Name = "arabic_salam";  Category = "ComplexScript"; Text = "السلام عليكم"; FontSizes = [24.0; 48.0] }
    { Name = "arabic_marhaba"; Category = "ComplexScript"; Text = "مرحبا";        FontSizes = [24.0; 48.0] }
]

let bidiCases = [
    { Name = "bidi_mixed";    Category = "Bidi"; Text = "Hello مرحبا World";  FontSizes = [24.0; 48.0] }
    { Name = "bidi_hebrew";   Category = "Bidi"; Text = "abc 123 דגה xyz";    FontSizes = [24.0; 48.0] }
]

let verticalCases = [
    { Name = "vert_Hg";      Category = "Vertical"; Text = "Hg";   FontSizes = [24.0; 48.0] }
    { Name = "vert_accents";  Category = "Vertical"; Text = "AÉĝ"; FontSizes = [24.0; 48.0] }
]

let specialCases = [
    { Name = "special_super"; Category = "Special"; Text = "x²+y²=r²"; FontSizes = [24.0; 48.0] }
    { Name = "special_frac";  Category = "Special"; Text = "½ ¼ ¾";    FontSizes = [24.0; 48.0] }
    { Name = "special_arrow"; Category = "Special"; Text = "→ ← ↑ ↓";  FontSizes = [24.0; 48.0] }
]

let allCases =
    kerningCases @ ligatureCases @ rasterCases @ complexCases @
    bidiCases @ verticalCases @ specialCases

// ── Canvas size heuristic ──

let canvasSize (text: string) (fontSize: float) =
    let estW = max 200 (int (float text.Length * fontSize * 0.8) + 40)
    let estH = max 60 (int (fontSize * 2.0) + 20)
    estW, estH

// ── Performance benchmark types and helpers ──

type PerfResult = {
    Name: string
    Description: string
    Iterations: int
    OurMs: float
    SkiaMs: float
}

/// Time a function over n iterations, returning ms/iter.
/// Runs one warmup call and a GC before measuring.
let timeMs (n: int) (f: unit -> unit) : float =
    f ()
    GC.Collect(2, GCCollectionMode.Forced, true)
    GC.WaitForPendingFinalizers()
    let sw = Diagnostics.Stopwatch.StartNew()
    for _ = 1 to n do f ()
    sw.Stop()
    sw.Elapsed.TotalMilliseconds / float n

/// Reload our font (clearing caches first) and return fresh FontData.
let reloadFont (fontPath: string) =
    Text.clearCaches ()
    Raster.clearGlyphBitmapCache ()
    Text.loadFontFile fontPath

// ── Main entry point ──

[<EntryPoint>]
let main argv =
    let fontPath =
        match argv |> Array.tryHead with
        | Some p when File.Exists p -> p
        | _ ->
            let candidates = [
                "NotoSans-Regular.ttf"
                "../renderer/NotoSans-Regular.ttf"
                "../NotoSans-Regular.ttf"
                "/usr/share/fonts/truetype/noto/NotoSans-Regular.ttf" ]
            match candidates |> List.tryFind File.Exists with
            | Some p -> p
            | None ->
                eprintfn "Error: No font file found. Pass font path as first argument."
                exit 1

    let outDir =
        match argv |> Array.tryItem 1 with
        | Some d -> d
        | None -> "textbench_output"
    Directory.CreateDirectory(outDir) |> ignore

    printfn "Font: %s" fontPath
    printfn "Output: %s/" outDir
    printfn ""

    let mutable font = Text.loadFontFile fontPath
    let mutable results : CompareResult list = []

    // ── Run all quality test cases ──
    let mutable currentCat = ""
    for tc in allCases do
        if tc.Category <> currentCat then
            currentCat <- tc.Category
            printfn "--- %s ---" currentCat

        for sz in tc.FontSizes do
            let label = sprintf "%s_%gpx" tc.Name sz
            let w, h = canvasSize tc.Text sz

            // Baseline alignment: both renderers place text at y = ascender
            let ourAsc = Text.ascender font sz
            let skiaAsc = float (SkiaRenderer.ascender fontPath (float32 sz))
            let baselineY = ourAsc
            let x = 10.0

            let ourFb = renderOurs font sz tc.Text w h x baselineY
            let skiaFb = SkiaRenderer.renderText fontPath (float32 sz) tc.Text w h (float32 x) (float32 skiaAsc)

            let ourAdv = Text.measureWidth font sz tc.Text
            let skiaAdv = SkiaRenderer.measureWidth fontPath (float32 sz) tc.Text

            let result = compare label ourFb skiaFb ourAdv skiaAdv
            results <- results @ [result]

            writePpm (Path.Combine(outDir, sprintf "ours_%s.ppm" label)) ourFb
            writePpm (Path.Combine(outDir, sprintf "skia_%s.ppm" label)) skiaFb
            writePpm (Path.Combine(outDir, sprintf "diff_%s.ppm" label)) (diffImage ourFb skiaFb)

            printfn "  %-35s  advDelta=%+.1fpx  RMSE=%.2f  >10:%d  >50:%d"
                label result.AdvanceDeltaPx result.RMSE result.PixelsDiffGt10 result.PixelsDiffGt50

    // ════════════════════════════════════════════════════════════════════
    // Performance benchmarks
    //
    // Our renderer has 3 cache layers:
    //   1. fontCache          — font parsing (keyed by byte[] hash)
    //   2. outlineCache       — glyph vector outlines (keyed by font+glyphId)
    //   3. glyphBitmapCache   — rasterized glyph alpha masks (keyed by font+glyphId+ppem+subpixel)
    //
    // These benchmarks isolate each dimension to show where caching helps
    // and where our pipeline is inherently faster or slower than Skia.
    // ════════════════════════════════════════════════════════════════════

    printfn ""
    printfn "=== Performance Benchmarks ==="

    let skia = SkiaRenderer.createCached fontPath
    let perfResults = ResizeArray<PerfResult>()
    let x = 10.0
    let skiaAsc24 = skia.Ascender 24.0f
    let ourAsc24 = Text.ascender font 24.0

    // ── 1. Cold start: clear all caches, measure first render ──
    //    Reveals one-time cost of font parsing + initial shaping + outline loading.
    printfn ""
    printfn "--- 1. Cold Start (first render after cache clear) ---"
    let coldText = "The quick brown fox jumps over the lazy dog"
    let coldW, coldH = canvasSize coldText 24.0
    let coldN = 20
    let coldOur = timeMs coldN (fun () ->
        let f = reloadFont fontPath
        renderOurs f 24.0 coldText coldW coldH x (Text.ascender f 24.0) |> ignore)
    let coldSkia = timeMs coldN (fun () ->
        SkiaRenderer.renderText fontPath 24.0f coldText coldW coldH (float32 x) (SkiaRenderer.ascender fontPath 24.0f) |> ignore)
    perfResults.Add({ Name = "cold_start"; Description = "First render (caches cleared)"; Iterations = coldN; OurMs = coldOur; SkiaMs = coldSkia })
    printfn "  Ours: %.2f ms  Skia: %.2f ms  Ratio: %.1fx" coldOur coldSkia (coldOur / coldSkia)

    // Reload for remaining benchmarks
    font <- reloadFont fontPath

    // ── 2. Warm repeated: same string, same size, pure cache hits ──
    //    Best case for our caching: shaping+outline all hit.
    //    Measures pure rasterization throughput with cached layout.
    printfn ""
    printfn "--- 2. Warm Repeated (same string, full cache hit) ---"
    let warmText = "The quick brown fox jumps over the lazy dog"
    let warmW, warmH = canvasSize warmText 24.0
    let warmN = 1000
    renderOurs font 24.0 warmText warmW warmH x ourAsc24 |> ignore
    skia.RenderText 24.0f warmText warmW warmH (float32 x) skiaAsc24 |> ignore
    let warmOur = timeMs warmN (fun () ->
        renderOurs font 24.0 warmText warmW warmH x ourAsc24 |> ignore)
    let warmSkia = timeMs warmN (fun () ->
        skia.RenderText 24.0f warmText warmW warmH (float32 x) skiaAsc24 |> ignore)
    perfResults.Add({ Name = "warm_repeated"; Description = "Same string 24px (full cache hit)"; Iterations = warmN; OurMs = warmOur; SkiaMs = warmSkia })
    printfn "  Ours: %.2f ms  Skia: %.2f ms  Ratio: %.1fx" warmOur warmSkia (warmOur / warmSkia)

    // ── 3. Size cycling: same text, rotating through 5 sizes ──
    //    Our shapingCache hits (key is font+text, rescales for size).
    //    Outline cache hits (same glyph IDs regardless of size).
    //    But rasterization cost scales with glyph area (bigger = more fill).
    //    Skia re-creates SKFont per size but typeface is cached.
    printfn ""
    printfn "--- 3. Size Cycling (same text, 5 sizes round-robin) ---"
    let sizes = [| 10.0f; 14.0f; 24.0f; 36.0f; 48.0f |]
    let sizeText = "Sphinx of black quartz, judge my vow"
    let sizeW, sizeH = canvasSize sizeText 48.0
    let sizeN = 500
    for sz in sizes do
        renderOurs font (float sz) sizeText sizeW sizeH x (Text.ascender font (float sz)) |> ignore
        skia.RenderText sz sizeText sizeW sizeH (float32 x) (skia.Ascender sz) |> ignore
    let mutable sizeIdx = 0
    let sizeOur = timeMs sizeN (fun () ->
        let sz = sizes.[sizeIdx % sizes.Length]
        sizeIdx <- sizeIdx + 1
        renderOurs font (float sz) sizeText sizeW sizeH x (Text.ascender font (float sz)) |> ignore)
    sizeIdx <- 0
    let sizeSkia = timeMs sizeN (fun () ->
        let sz = sizes.[sizeIdx % sizes.Length]
        sizeIdx <- sizeIdx + 1
        skia.RenderText sz sizeText sizeW sizeH (float32 x) (skia.Ascender sz) |> ignore)
    perfResults.Add({ Name = "size_cycling"; Description = "Same text, 5 sizes round-robin"; Iterations = sizeN; OurMs = sizeOur; SkiaMs = sizeSkia })
    printfn "  Ours: %.2f ms  Skia: %.2f ms  Ratio: %.1fx" sizeOur sizeSkia (sizeOur / sizeSkia)

    // ── 4. Unique strings: every render is a shaping cache miss ──
    //    Outlines still cached (ASCII glyph reuse), but Bidi+shaping runs
    //    fresh each time. Shows shaping overhead vs Skia's internal shaper.
    printfn ""
    printfn "--- 4. Unique Strings (shaping cache miss every time) ---"
    let uniqueN = 500
    let uniqueStrings =
        [| for i in 0 .. uniqueN - 1 ->
            sprintf "Sentence number %d: the quick brown fox #%d jumps" i i |]
    let uniqueW, uniqueH = canvasSize uniqueStrings.[0] 24.0
    font <- reloadFont fontPath
    let mutable uIdx = 0
    let uniqueOur = timeMs uniqueN (fun () ->
        renderOurs font 24.0 uniqueStrings.[uIdx % uniqueN] uniqueW uniqueH x ourAsc24 |> ignore
        uIdx <- uIdx + 1)
    uIdx <- 0
    let uniqueSkia = timeMs uniqueN (fun () ->
        skia.RenderText 24.0f uniqueStrings.[uIdx % uniqueN] uniqueW uniqueH (float32 x) skiaAsc24 |> ignore
        uIdx <- uIdx + 1)
    perfResults.Add({ Name = "unique_strings"; Description = "500 unique strings (shaping miss)"; Iterations = uniqueN; OurMs = uniqueOur; SkiaMs = uniqueSkia })
    printfn "  Ours: %.2f ms  Skia: %.2f ms  Ratio: %.1fx" uniqueOur uniqueSkia (uniqueOur / uniqueSkia)

    // ── 5. Growing glyph vocabulary: progressively more distinct glyphs ──
    //    Tier 1 = 26 lowercase outlines. Tier 2 = 62 (+ uppercase + digits).
    //    Tier 3 = 60 accented Latin chars (many new outline cache entries).
    //    Shows outline cache growth cost vs Skia's internal glyph cache.
    printfn ""
    printfn "--- 5. Glyph Vocabulary (outline cache growth) ---"
    let vocabStrings = [|
        "abcdefghijklmnopqrstuvwxyz"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
        "àáâãäåæçèéêëìíîïðñòóôõöùúûüýþÿĀāĂăĄąĆćĈĉĊċČčĎďĐđĒēĖėĘę"
    |]
    let vocabLabels = [| "ascii_26"; "ascii_62"; "latin_ext" |]
    font <- reloadFont fontPath
    let vocabN = 200
    for tier = 0 to vocabStrings.Length - 1 do
        let s = vocabStrings.[tier]
        let vW, vH = canvasSize s 24.0
        let vocabOur = timeMs vocabN (fun () ->
            renderOurs font 24.0 s vW vH x (Text.ascender font 24.0) |> ignore)
        let vocabSkia = timeMs vocabN (fun () ->
            skia.RenderText 24.0f s vW vH (float32 x) skiaAsc24 |> ignore)
        perfResults.Add({ Name = sprintf "vocab_%s" vocabLabels.[tier]; Description = sprintf "Tier %d: %s (%d chars)" (tier+1) vocabLabels.[tier] s.Length; Iterations = vocabN; OurMs = vocabOur; SkiaMs = vocabSkia })
        printfn "  Tier %d (%s): Ours=%.2f ms  Skia=%.2f ms  Ratio=%.1fx" (tier+1) vocabLabels.[tier] vocabOur vocabSkia (vocabOur / vocabSkia)

    // ── 6. Paragraph rendering: realistic multi-line workload ──
    //    10 pangram lines on a single surface. Many lines share common words
    //    (shaping cache hits after first pass) + heavy outline reuse.
    //    Single-surface Skia comparison (one canvas, 10 DrawText calls).
    printfn ""
    printfn "--- 6. Paragraph (multi-line, shared glyph outlines) ---"
    let paragraphLines = [|
        "The quick brown fox jumps over the lazy dog."
        "Pack my box with five dozen liquor jugs."
        "How vexingly quick daft zebras jump!"
        "The five boxing wizards jump quickly."
        "Bright vixens jump; dozy fowl quack."
        "Sphinx of black quartz, judge my vow."
        "Two driven jocks help fax my big quiz."
        "The jay, pig, fox, zebra and my wolves quack!"
        "Sympathizing would fix Quaker objectives."
        "A wizard's job is to vex chumps quickly in fog."
    |]
    let paraW = 600
    let paraLineH = 30.0
    let paraH = int (float paragraphLines.Length * paraLineH) + 20
    let paraN = 200
    for line in paragraphLines do
        Text.layout font 24.0 line |> ignore
    let paraOur = timeMs paraN (fun () ->
        let backend = Raster.create ()
        backend.Init paraW paraH
        let mutable cmds : Cmd list = [ Clear { R = 0uy; G = 0uy; B = 0uy; A = 0uy } ]
        for i = 0 to paragraphLines.Length - 1 do
            let ly = ourAsc24 + float i * paraLineH
            let runs = Text.layout font 24.0 paragraphLines.[i]
            cmds <- cmds @ [ DrawGlyphs (vec2 x ly, runs, Solid Color.white) ]
        backend.Submit cmds
        backend.Finish () |> ignore)
    let paraSkia = timeMs paraN (fun () ->
        let info = SkiaSharp.SKImageInfo(paraW, paraH, SkiaSharp.SKColorType.Rgba8888, SkiaSharp.SKAlphaType.Premul)
        use surface = SkiaSharp.SKSurface.Create(info)
        surface.Canvas.Clear(SkiaSharp.SKColors.Transparent)
        use paint = new SkiaSharp.SKPaint()
        paint.Color <- SkiaSharp.SKColors.White
        paint.IsAntialias <- true
        use skFont = new SkiaSharp.SKFont(SkiaSharp.SKTypeface.FromFile(fontPath), 24.0f)
        skFont.Subpixel <- true
        for i = 0 to paragraphLines.Length - 1 do
            let ly = float32 skiaAsc24 + float32 i * float32 paraLineH
            surface.Canvas.DrawText(paragraphLines.[i], float32 x, ly, skFont, paint)
        ())
    perfResults.Add({ Name = "paragraph_10line"; Description = "10-line paragraph at 24px"; Iterations = paraN; OurMs = paraOur; SkiaMs = paraSkia })
    printfn "  Ours: %.2f ms  Skia: %.2f ms  Ratio: %.1fx" paraOur paraSkia (paraOur / paraSkia)

    // ── 7. Incremental typing simulation ──
    //    Render progressively longer prefixes of a string: "H", "He", "Hel", ...
    //    Each prefix is a new shaping cache key but shares glyph outlines.
    //    Simulates a text editor re-rendering as the user types.
    printfn ""
    printfn "--- 7. Incremental Typing (growing prefix, shaping miss) ---"
    let typingBase = "The quick brown fox jumps over the lazy dog"
    let typingPrefixes = [| for i in 1 .. typingBase.Length -> typingBase.[0..i-1] |]
    let typingW, typingH = canvasSize typingBase 24.0
    font <- reloadFont fontPath
    let typingN = typingPrefixes.Length
    // One full pass = render all 43 prefixes
    let typingPasses = 10
    let typingOur = timeMs typingPasses (fun () ->
        for i = 0 to typingN - 1 do
            renderOurs font 24.0 typingPrefixes.[i] typingW typingH x ourAsc24 |> ignore)
    let typingSkia = timeMs typingPasses (fun () ->
        for i = 0 to typingN - 1 do
            skia.RenderText 24.0f typingPrefixes.[i] typingW typingH (float32 x) skiaAsc24 |> ignore)
    perfResults.Add({ Name = "typing_sim"; Description = sprintf "Typing sim (%d prefixes x%d)" typingN typingPasses; Iterations = typingPasses; OurMs = typingOur; SkiaMs = typingSkia })
    printfn "  Ours: %.2f ms/pass  Skia: %.2f ms/pass  Ratio: %.1fx" typingOur typingSkia (typingOur / typingSkia)

    // ── 8. Layout-only: measureWidth without rasterization ──
    //    Isolates shaping/layout cost from scanline fill.
    printfn ""
    printfn "--- 8. Layout Only (measureWidth, no rasterization) ---"
    let layoutText = "The quick brown fox jumps over the lazy dog"
    let layoutN = 5000

    // 8a: Warm (cache hit — pure dictionary lookup)
    Text.measureWidth font 24.0 layoutText |> ignore
    skia.MeasureWidth 24.0f layoutText |> ignore
    let layoutWarmOur = timeMs layoutN (fun () ->
        Text.measureWidth font 24.0 layoutText |> ignore)
    let layoutWarmSkia = timeMs layoutN (fun () ->
        skia.MeasureWidth 24.0f layoutText |> ignore)
    perfResults.Add({ Name = "layout_warm"; Description = "measureWidth (cache hit)"; Iterations = layoutN; OurMs = layoutWarmOur; SkiaMs = layoutWarmSkia })
    printfn "  Warm:  Ours=%.4f ms  Skia=%.4f ms  Ratio=%.1fx" layoutWarmOur layoutWarmSkia (layoutWarmOur / layoutWarmSkia)

    // 8b: Cold (unique strings — shaping cache miss every time)
    let layoutColdStrings = [| for i in 0 .. layoutN - 1 -> sprintf "Layout test string variant %d end" i |]
    font <- reloadFont fontPath
    let mutable lcIdx = 0
    let layoutColdOur = timeMs layoutN (fun () ->
        Text.measureWidth font 24.0 layoutColdStrings.[lcIdx % layoutN] |> ignore
        lcIdx <- lcIdx + 1)
    lcIdx <- 0
    let layoutColdSkia = timeMs layoutN (fun () ->
        skia.MeasureWidth 24.0f layoutColdStrings.[lcIdx % layoutN] |> ignore
        lcIdx <- lcIdx + 1)
    perfResults.Add({ Name = "layout_cold"; Description = "measureWidth (unique strings)"; Iterations = layoutN; OurMs = layoutColdOur; SkiaMs = layoutColdSkia })
    printfn "  Cold:  Ours=%.4f ms  Skia=%.4f ms  Ratio=%.1fx" layoutColdOur layoutColdSkia (layoutColdOur / layoutColdSkia)

    // ── Format performance report ──
    printfn ""
    let perfSb = System.Text.StringBuilder()
    perfSb.AppendLine("\nPERFORMANCE BENCHMARKS") |> ignore
    perfSb.AppendLine("=====================") |> ignore
    perfSb.AppendLine() |> ignore
    perfSb.AppendLine("Our renderer caches: fontCache (parse), outlineCache (vector paths per glyphId),") |> ignore
    perfSb.AppendLine("glyphBitmapCache (rasterized alpha masks per glyph+size+subpixel). Skia uses pre-loaded typeface for fairness.") |> ignore
    perfSb.AppendLine() |> ignore
    perfSb.AppendLine(
        sprintf "%-22s %-42s %6s %10s %10s %7s"
            "Benchmark" "Description" "Iters" "Ours(ms)" "Skia(ms)" "Ratio"
    ) |> ignore
    perfSb.AppendLine(String('-', 102)) |> ignore
    for r in perfResults do
        let ratio = if r.SkiaMs > 0.0 then r.OurMs / r.SkiaMs else infinity
        perfSb.AppendLine(
            sprintf "%-22s %-42s %6d %10.3f %10.3f %6.1fx"
                r.Name r.Description r.Iterations r.OurMs r.SkiaMs ratio
        ) |> ignore
    perfSb.AppendLine() |> ignore

    let renderBenches = perfResults |> Seq.filter (fun r -> not (r.Name.StartsWith("layout"))) |> Seq.toList
    if renderBenches.Length > 0 then
        let avgRatio = renderBenches |> List.averageBy (fun r -> if r.SkiaMs > 0.0 then r.OurMs / r.SkiaMs else 1.0)
        perfSb.AppendLine(sprintf "Average render ratio (ours/skia): %.1fx" avgRatio) |> ignore
    let layoutBenches = perfResults |> Seq.filter (fun r -> r.Name.StartsWith("layout")) |> Seq.toList
    if layoutBenches.Length > 0 then
        let avgLayoutRatio = layoutBenches |> List.averageBy (fun r -> if r.SkiaMs > 0.0 then r.OurMs / r.SkiaMs else 1.0)
        perfSb.AppendLine(sprintf "Average layout ratio (ours/skia): %.1fx" avgLayoutRatio) |> ignore

    // ── Write report ──
    let report = formatReport results
    let fullReport = report + perfSb.ToString()
    File.WriteAllText(Path.Combine(outDir, "report.txt"), fullReport)
    printfn "Report written to %s/report.txt" outDir
    printfn "%d quality tests, %d perf benchmarks." results.Length perfResults.Count

    0
