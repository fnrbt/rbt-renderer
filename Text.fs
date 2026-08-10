namespace Renderer

open System
open System.Collections.Generic
open System.IO
open System.Runtime.CompilerServices

module Text =

    // ── Types ──

    type OutlineFontData = {
        OtFont: OpenType.Font.Font
        ShFont: Shaper.Shaper.ParsedFont
        TextFont: global.Text.Engine.TextFont
    }

    type FontFace =
        | OutlineFace of OutlineFontData
        | BitmapFace of Pcf.Font

    type FontData = {
        // Kept for existing callers that inspect OpenType data directly.
        // Bitmap fonts have these as default/null values; use helpers below in renderer code.
        OtFont: OpenType.Font.Font
        ShFont: Shaper.Shaper.ParsedFont
        Face: FontFace
        Id: int
    }

    type PositionedGlyph = {
        GlyphId: int
        X: float
        Y: float
        XAdv: float
    }

    type GlyphRun = {
        Font: FontData
        FontSize: float
        Glyphs: PositionedGlyph[]
        Direction: Shaper.Shaper.TextDirection
    }

    type BitmapGlyph = {
        Alpha: byte[]
        W: int
        H: int
        BearingX: int
        BearingY: int
        ByteSize: int
    }

    // ── Font loading + cache ──

    let private fontCache = Dictionary<int, FontData>()
    let mutable private nextFontId = 1
    // Font loading happens concurrently (e.g. several @font-face downloads register
    // in parallel Tasks). fontCache/nextFontId/the text-engine registry are not
    // thread-safe, so serialize every loader body through this lock (Monitor is
    // re-entrant, so loaders that delegate to other loaders are fine).
    let private loadLock = obj()

    let private allocFontId () =
        let id = nextFontId
        nextFontId <- nextFontId + 1
        id

    let private makeOutlineFont (otFont: OpenType.Font.Font) (shFont: Shaper.Shaper.ParsedFont) =
        { OtFont = otFont
          ShFont = shFont
          Face = OutlineFace { OtFont = otFont; ShFont = shFont; TextFont = global.Text.Engine.createFont shFont }
          Id = allocFontId () }

    let private makeBitmapFont (pcfFont: Pcf.Font) =
        { OtFont = Unchecked.defaultof<OpenType.Font.Font>
          ShFont = Unchecked.defaultof<Shaper.Shaper.ParsedFont>
          Face = BitmapFace pcfFont
          Id = allocFontId () }

    let private cacheKey tag (data: byte[]) =
        HashCode.Combine(tag, data.GetHashCode())

    let loadFont (data: byte[]) : FontData =
      lock loadLock (fun () ->
        let hash = cacheKey 0x4f54544f data
        match fontCache.TryGetValue(hash) with
        | true, cached -> cached
        | _ ->
            // Unwrap WOFF/WOFF2 to raw sfnt once, then feed BOTH parsers the
            // decompressed bytes. OpenType.Font.load unwraps internally, but
            // Shaper.parseFont does not — handing it wrapped WOFF made it read the
            // WOFF header as a table directory and throw KeyNotFoundException on
            // tables.["head"]. Most modern web fonts ship only woff/woff2.
            let sfnt =
                if OpenType.Woff.isWoff data then OpenType.Woff.decodeWoff data
                elif OpenType.Woff.isWoff2 data then OpenType.Woff.decodeWoff2 data
                else data
            let otFont = OpenType.Font.load sfnt
            let shFont = Shaper.Shaper.parseFont sfnt
            let fd = makeOutlineFont otFont shFont
            fontCache.[hash] <- fd
            fd)

    let loadPcf (data: byte[]) : FontData =
      lock loadLock (fun () ->
        let hash = cacheKey 0x504346 data
        match fontCache.TryGetValue(hash) with
        | true, cached -> cached
        | _ ->
            let fd = Pcf.load data |> makeBitmapFont
            fontCache.[hash] <- fd
            fd)

    let loadPcfFile (path: string) : FontData =
        Pcf.loadFile path |> makeBitmapFont

    let loadFontFile (path: string) : FontData =
        if path.EndsWith(".pcf", StringComparison.OrdinalIgnoreCase)
           || path.EndsWith(".pcf.gz", StringComparison.OrdinalIgnoreCase) then
            loadPcfFile path
        else
            let data = File.ReadAllBytes(path)
            if Pcf.isPcfData data then loadPcf data else loadFont data

    /// Load a raw CFF (Type1C) font program directly, bypassing OpenType container parsing.
    /// glyphWidths: advance width per glyph in font units (1000 = full em for most CFF).
    /// ascender/descender: font metrics in font units. unitsPerEm: typically 1000 for CFF.
    let loadCff (cffData: byte[]) (glyphWidths: int[]) (ascender: int) (descender: int) (unitsPerEm: int) : FontData =
      lock loadLock (fun () ->
        let hash = cacheKey 0x434646 cffData
        match fontCache.TryGetValue(hash) with
        | true, cached -> cached
        | _ ->
            let cff = OpenType.Cff.parseCff cffData 0
            let numGlyphs = cff.CharStrings.Length
            let hmtx =
                Array.init numGlyphs (fun i ->
                    let w = if i < glyphWidths.Length then glyphWidths.[i] else 0
                    { OpenType.Tables.AdvanceWidth = uint16 (max 0 (min 65535 w))
                      OpenType.Tables.LeftSideBearing = 0s })
            let otFont : OpenType.Font.Font =
                { Data = cffData
                  Head = { UnitsPerEm = unitsPerEm; IndexToLocFormat = 0
                           XMin = 0s; YMin = int16 descender; XMax = int16 unitsPerEm; YMax = int16 ascender }
                  Maxp = { NumGlyphs = numGlyphs }
                  Hhea = { Ascender = int16 ascender; Descender = int16 descender; LineGap = 0s
                           NumberOfHMetrics = numGlyphs }
                  Hmtx = hmtx
                  Vhea = None; Vmtx = None; OS2 = None; Post = None; Names = [||]
                  CmapSubtable = OpenType.Cmap.Unsupported -1; CmapFormat14 = None
                  Outlines = OpenType.Font.CffOutlines cff
                  Kern = None; Colr = None; Cpal = None; Svg = None
                  Bitmaps = None; Gdef = None; Gsub = None; Gpos = None
                  Stat = None; Vorg = None; Variation = None }
            let shFont : Shaper.Shaper.ParsedFont =
                { Data = cffData
                  Cmap = Shaper.OpenType.Types.EmptyCmap
                  GlyphWidths = if glyphWidths.Length >= numGlyphs then glyphWidths
                                else Array.init numGlyphs (fun i -> if i < glyphWidths.Length then glyphWidths.[i] else 0)
                  UnitsPerEm = unitsPerEm
                  Ascender = ascender
                  Descender = descender
                  LineGap = 0
                  GSUB = lazy None; GPOS = lazy None; GDEF = lazy None }
            let fd = makeOutlineFont otFont shFont
            fontCache.[hash] <- fd
            fd)

    /// Load a raw Type 1 font program directly (from PDF FontFile stream).
    let loadType1 (data: byte[]) (length1: int) (length2: int) (glyphWidths: int[]) (ascender: int) (descender: int) (unitsPerEm: int) : FontData =
      lock loadLock (fun () ->
        let hash = cacheKey 0x5431 data
        match fontCache.TryGetValue(hash) with
        | true, cached -> cached
        | _ ->
            let t1 = OpenType.Type1.parseType1 data length1 length2
            let numGlyphs = t1.CharStrings.Length
            let hmtx =
                Array.init numGlyphs (fun i ->
                    let w = if i < glyphWidths.Length then glyphWidths.[i] else 0
                    { OpenType.Tables.AdvanceWidth = uint16 (max 0 (min 65535 w))
                      OpenType.Tables.LeftSideBearing = 0s })
            let otFont : OpenType.Font.Font =
                { Data = data
                  Head = { UnitsPerEm = unitsPerEm; IndexToLocFormat = 0
                           XMin = 0s; YMin = int16 descender; XMax = int16 unitsPerEm; YMax = int16 ascender }
                  Maxp = { NumGlyphs = numGlyphs }
                  Hhea = { Ascender = int16 ascender; Descender = int16 descender; LineGap = 0s
                           NumberOfHMetrics = numGlyphs }
                  Hmtx = hmtx
                  Vhea = None; Vmtx = None; OS2 = None; Post = None; Names = [||]
                  CmapSubtable = OpenType.Cmap.Unsupported -1; CmapFormat14 = None
                  Outlines = OpenType.Font.Type1Outlines t1
                  Kern = None; Colr = None; Cpal = None; Svg = None
                  Bitmaps = None; Gdef = None; Gsub = None; Gpos = None
                  Stat = None; Vorg = None; Variation = None }
            let shFont : Shaper.Shaper.ParsedFont =
                { Data = data
                  Cmap = Shaper.OpenType.Types.EmptyCmap
                  GlyphWidths = if glyphWidths.Length >= numGlyphs then glyphWidths
                                else Array.init numGlyphs (fun i -> if i < glyphWidths.Length then glyphWidths.[i] else 0)
                  UnitsPerEm = unitsPerEm
                  Ascender = ascender; Descender = descender
                  LineGap = 0
                  GSUB = lazy None; GPOS = lazy None; GDEF = lazy None }
            let fd = makeOutlineFont otFont shFont
            fontCache.[hash] <- fd
            fd)

    let fontId (font: FontData) =
        font.Id

    let tryOpenTypeFont (font: FontData) =
        match font.Face with
        | OutlineFace outline -> Some outline.OtFont
        | BitmapFace _ -> None

    let hasOutlines (font: FontData) =
        match font.Face with
        | OutlineFace _ -> true
        | BitmapFace _ -> false

    let isBitmapFont (font: FontData) =
        match font.Face with
        | BitmapFace _ -> true
        | OutlineFace _ -> false

    // ── Outline conversion + cache ──

    // Concurrent: the compositor renders on the event-loop thread while the
    // test-control / screenshot path can render on its own thread; a plain
    // Dictionary corrupts under that concurrent read+insert.
    let private outlineCache = System.Collections.Concurrent.ConcurrentDictionary<int * int, PathSeg list>()

    /// Convert OpenType PathCmd[] to renderer PathSeg list at normalized scale (font units).
    let glyphOutline (font: FontData) (glyphId: int) : PathSeg list =
        match font.Face with
        | BitmapFace _ -> []
        | OutlineFace outline ->
            let key = (font.Id, glyphId)
            match outlineCache.TryGetValue(key) with
            | true, cached -> cached
            | _ ->
                let cmds = OpenType.Font.outline outline.OtFont glyphId
                let segs =
                    [ for cmd in cmds do
                        match cmd with
                        | OpenType.Outline.MoveTo (x, y) ->
                            yield MoveTo (vec2 x (-y))
                        | OpenType.Outline.LineTo (x, y) ->
                            yield LineTo (vec2 x (-y))
                        | OpenType.Outline.QuadTo (cx, cy, x, y) ->
                            yield QuadTo (vec2 cx (-cy), vec2 x (-y))
                        | OpenType.Outline.CubicTo (cx1, cy1, cx2, cy2, x, y) ->
                            yield CubicTo (vec2 cx1 (-cy1), vec2 cx2 (-cy2), vec2 x (-y))
                        | OpenType.Outline.Close ->
                            yield Close ]
                outlineCache.[key] <- segs
                segs

    /// Translate and scale a PathSeg.
    let translatePathSeg (tx: float) (ty: float) (s: float) (seg: PathSeg) : PathSeg =
        let tr (v: Vec2) = vec2 (float v.X * s + tx) (float v.Y * s + ty)
        match seg with
        | MoveTo p -> MoveTo (tr p)
        | LineTo p -> LineTo (tr p)
        | QuadTo (cp, pt) -> QuadTo (tr cp, tr pt)
        | CubicTo (cp1, cp2, pt) -> CubicTo (tr cp1, tr cp2, tr pt)
        | ArcTo (rx, ry, rot, la, sw, pt) -> ArcTo (rx * s, ry * s, rot, la, sw, tr pt)
        | Close -> Close

    // ── Bitmap glyphs ──

    let private scaleBitmapGlyph sourcePpem targetPpem (glyph: Pcf.GlyphBitmap) =
        let sourcePpem = max 1 sourcePpem
        let targetPpem = max 1 targetPpem
        let scale = float targetPpem / float sourcePpem

        if glyph.W = 0 || glyph.H = 0 then
            { Alpha = [||]
              W = 0
              H = 0
              BearingX = int (round (float glyph.BearingX * scale))
              BearingY = int (round (float glyph.BearingY * scale))
              ByteSize = 0 }
        elif abs (scale - 1.0) < 0.000001 then
            { Alpha = Array.copy glyph.Alpha
              W = glyph.W
              H = glyph.H
              BearingX = glyph.BearingX
              BearingY = glyph.BearingY
              ByteSize = glyph.Alpha.Length }
        else
            let w = max 1 (int (round (float glyph.W * scale)))
            let h = max 1 (int (round (float glyph.H * scale)))
            let alpha = Array.zeroCreate<byte> (w * h)
            for y = 0 to h - 1 do
                let sy = min (glyph.H - 1) (int (floor (float y / scale)))
                for x = 0 to w - 1 do
                    let sx = min (glyph.W - 1) (int (floor (float x / scale)))
                    alpha.[y * w + x] <- glyph.Alpha.[sy * glyph.W + sx]

            { Alpha = alpha
              W = w
              H = h
              BearingX = int (round (float glyph.BearingX * scale))
              BearingY = int (round (float glyph.BearingY * scale))
              ByteSize = alpha.Length }

    let tryGlyphBitmap (font: FontData) (glyphId: int) (ppem: int) =
        match font.Face with
        | OutlineFace _ -> None
        | BitmapFace pcfFont ->
            Pcf.glyphBitmap pcfFont glyphId
            |> Option.map (scaleBitmapGlyph pcfFont.PixelSize ppem)

    // ── Metrics helpers ──

    let unitsPerEm (font: FontData) : float =
        match font.Face with
        | OutlineFace outline -> global.Text.Engine.unitsPerEm outline.TextFont
        | BitmapFace pcfFont -> float pcfFont.PixelSize

    let ascender (font: FontData) (fontSize: float) : float =
        match font.Face with
        | OutlineFace outline -> global.Text.Engine.ascender outline.TextFont fontSize
        | BitmapFace pcfFont -> float pcfFont.Ascender * fontSize / unitsPerEm font

    let descender (font: FontData) (fontSize: float) : float =
        match font.Face with
        | OutlineFace outline -> global.Text.Engine.descender outline.TextFont fontSize
        | BitmapFace pcfFont -> -float pcfFont.Descender * fontSize / unitsPerEm font

    let lineHeight (font: FontData) (fontSize: float) : float =
        match font.Face with
        | OutlineFace outline -> global.Text.Engine.lineHeight outline.TextFont fontSize
        | BitmapFace pcfFont ->
            float (pcfFont.Ascender + pcfFont.Descender) * fontSize / unitsPerEm font

    /// Windows-style ascent in screen pixels — uses OS/2 winAscent if
    /// available, falling back to hhea ascender + lineGap. Excel, Word, GDI
    /// and most native Win32 text apps anchor their box layout on these:
    /// `winAscent + winDescent` is the row height a single line "wants",
    /// and a line's baseline sits `winAscent` below its em-top. The hhea
    /// metrics are typographic ascender/descender (cap+accent height for
    /// nice line stacking) — they're smaller and would clip diacritics in
    /// fixed-height cells. OS/2 sFsSelection bit 7 (USE_TYPO_METRICS) opts
    /// fonts in to typo metrics; if set, we honor that instead.
    let private useTypoMetrics (font: FontData) =
        match font.Face with
        | OutlineFace outline ->
            match outline.OtFont.OS2 with
            | Some os2 -> (os2.FsSelection &&& 128us) <> 0us
            | None -> false
        | BitmapFace _ -> false

    let winAscent (font: FontData) (fontSize: float) : float =
        match font.Face with
        | BitmapFace _ -> ascender font fontSize
        | OutlineFace outline ->
            match outline.OtFont.OS2 with
            | Some os2 when not (useTypoMetrics font) ->
                float os2.WinAscent * fontSize / unitsPerEm font
            | Some os2 ->
                // USE_TYPO_METRICS set -> typo ascender + half typo lineGap above baseline.
                (float os2.TypoAscender + float os2.TypoLineGap / 2.0) * fontSize / unitsPerEm font
            | None ->
                ascender font fontSize

    let winDescent (font: FontData) (fontSize: float) : float =
        // Positive number; baseline-to-em-bottom distance.
        match font.Face with
        | BitmapFace pcfFont -> float pcfFont.Descender * fontSize / unitsPerEm font
        | OutlineFace outline ->
            match outline.OtFont.OS2 with
            | Some os2 when not (useTypoMetrics font) ->
                float os2.WinDescent * fontSize / unitsPerEm font
            | Some os2 ->
                (float -os2.TypoDescender + float os2.TypoLineGap / 2.0) * fontSize / unitsPerEm font
            | None ->
                - (descender font fontSize)

    let winLineHeight (font: FontData) (fontSize: float) : float =
        winAscent font fontSize + winDescent font fontSize

    /// True iff the font maps `codepoint` to a real glyph.
    let hasGlyph (font: FontData) (codepoint: int) : bool =
        match font.Face with
        | OutlineFace outline ->
            Shaper.OpenType.Types.CmapLookup.lookup outline.ShFont.Cmap codepoint <> 0
        | BitmapFace pcfFont ->
            Pcf.hasGlyph pcfFont codepoint

    // ── Layout engine ──

    let private layoutCache = Dictionary<int * string, GlyphRun list>()
    let private layoutCacheMaxEntries = 512

    let private runDirection (run: Bidi.Types.DirectionalRun) =
        match run.RunDirection with
        | Bidi.Types.LTR -> Shaper.Shaper.LeftToRight
        | Bidi.Types.RTL -> Shaper.Shaper.RightToLeft

    let private layoutCodepoints (font: FontData) (fontSize: float) (codepoints: int[]) dir cursorX =
        match font.Face with
        | OutlineFace outline ->
            let scale = fontSize / unitsPerEm font
            let shaped = Shaper.Shaper.shapeCodepoints outline.ShFont Shaper.Shaper.ShaperConfig.defaults codepoints dir
            let glyphs =
                let mutable x = cursorX
                [| for sg in shaped.Glyphs do
                    let gx = x + float sg.XOffset * scale
                    let gy = float sg.YOffset * scale
                    let adv = float sg.XAdvance * scale
                    yield { GlyphId = sg.GlyphId; X = gx; Y = gy; XAdv = adv }
                    x <- x + adv |]
            let nextX = cursorX + float shaped.TotalXAdvance * scale
            { Font = font; FontSize = fontSize; Glyphs = glyphs; Direction = dir }, nextX
        | BitmapFace pcfFont ->
            let scale = fontSize / unitsPerEm font
            let mutable x = cursorX
            let glyphs =
                [| for cp in codepoints do
                    let glyphId = Pcf.glyphIndexOrDefault pcfFont cp |> Option.defaultValue 0
                    let adv = float (Pcf.glyphAdvance pcfFont glyphId) * scale
                    yield { GlyphId = glyphId; X = x; Y = 0.0; XAdv = adv }
                    x <- x + adv |]
            { Font = font; FontSize = fontSize; Glyphs = glyphs; Direction = dir }, x

    let private prefersOutlineShaping codepoint =
        (codepoint >= 0x0590 && codepoint <= 0x08ff) || // Hebrew, Arabic, Syriac, Thaana, NKo
        (codepoint >= 0x0900 && codepoint <= 0x0dff) || // Indic scripts
        (codepoint >= 0x0e00 && codepoint <= 0x0eff) || // Thai, Lao
        (codepoint >= 0x0f00 && codepoint <= 0x0fff) || // Tibetan
        (codepoint >= 0x1000 && codepoint <= 0x109f) || // Myanmar
        (codepoint >= 0x1780 && codepoint <= 0x17ff) || // Khmer
        (codepoint >= 0xa800 && codepoint <= 0xa8ff) || // Syloti Nagri and related Indic blocks
        (codepoint >= 0xfb50 && codepoint <= 0xfdff) || // Arabic presentation forms
        (codepoint >= 0xfe70 && codepoint <= 0xfeff)

    let private chooseFallbackFont (fonts: FontData list) codepoint =
        if prefersOutlineShaping codepoint then
            fonts
            |> List.tryFind (fun font -> hasOutlines font && hasGlyph font codepoint)
            |> Option.orElseWith (fun () -> fonts |> List.tryFind (fun font -> hasGlyph font codepoint))
            |> Option.defaultValue fonts.Head
        else
            fonts
            |> List.tryFind (fun font -> hasGlyph font codepoint)
            |> Option.defaultValue fonts.Head

    let private layoutRuns (fonts: FontData list) (fontSize: float) (text: string) : GlyphRun list =
        match fonts with
        | [] -> []
        | _ when String.IsNullOrEmpty text -> []
        | _ ->
            let paragraph = Bidi.Algorithm.resolveParagraph text
            let mutable cursorX = 0.0
            let output = ResizeArray<GlyphRun>()

            for visualRun in paragraph.VisualRuns do
                let dir = runDirection visualRun
                let codepoints = ResizeArray<int>()
                let mutable currentFont : FontData option = None

                let flush () =
                    match currentFont with
                    | Some font when codepoints.Count > 0 ->
                        let run, nextX = layoutCodepoints font fontSize (codepoints.ToArray()) dir cursorX
                        cursorX <- nextX
                        output.Add run
                        codepoints.Clear()
                    | _ ->
                        codepoints.Clear()

                for ch in visualRun.Characters do
                    let font = chooseFallbackFont fonts ch.CodePoint
                    let sameFont =
                        match currentFont with
                        | Some current -> current.Id = font.Id
                        | None -> false

                    if not sameFont then
                        flush ()
                        currentFont <- Some font

                    codepoints.Add ch.CodePoint

                flush ()

            Seq.toList output

    // CAVEAT — this is an empirical match to OBSERVED Chrome output, NOT a prescribed
    // algorithm. No web/CSS standard mandates rounding glyph advances to whole pixels,
    // and modern browsers actually position text at SUBpixel offsets in many configs
    // (text-rendering, ClearType vs grayscale, and the platform rasterizer all matter).
    // Cross-browser/OS text measurement is genuinely inconsistent; there is no single
    // correct answer to reproduce. What we observed via Chrome canvas.measureText is
    // that monospace advances land on whole pixels (e.g. Lilex 0.6em = 9.12px @15.2 → 9).
    //
    // The underlying real mechanism is TrueType/OpenType HINTING (grid-fitting): the
    // font's bytecode snaps glyph outlines — including advances — to the pixel grid at
    // the rasterized size. That is per-glyph and size-dependent, NOT a simple round().
    // We don't run a hinter, so we approximate it ONLY where the approximation holds:
    //   - Monospace: every glyph shares one advance, so hinting snaps them all to the
    //     same integer — indistinguishable from round(advance). We round, and it matches.
    //   - Proportional: per-glyph grid-fitting doesn't reduce to round(advance); naive
    //     rounding diverges and drifts the text, so we leave those advances fractional
    //     (closer to Chrome). We are declining to fake hinting where faking it is wrong.
    // Using raw fractional advances for mono made our text a touch wide and wrap one
    // word early vs Chrome; rounding fixes that while re-placing glyph origins on the
    // rounded pen and preserving each glyph's positioning offset.
    //
    // The pen is threaded GLOBALLY across runs: per-glyph fallback splits a word into
    // separate runs (e.g. Lilex latin + latin-ext for accented Polish letters), and the
    // inter-run cursor uses unrounded totals. Resetting the pen per run would make a
    // run that rounded up overlap the next — gluing an accented letter to the preceding
    // glyph. origPen tracks the true cumulative advance (matching that cursor) so each
    // run's origin re-bases onto the rounded pen.
    //
    // Monospace is detected from the font's glyph widths (one advance dominating ⇒
    // monospace), cached per font.
    // Concurrent so the parallel render loop (one frame per thread) can populate it on a cold
    // cache without racing — the factory is pure, so an occasional duplicate computation under
    // contention is harmless (same value, stored once).
    let private monoFontCache = System.Collections.Concurrent.ConcurrentDictionary<int, bool>()
    let private isMonospaceFont (font: FontData) : bool =
        monoFontCache.GetOrAdd(font.Id, fun _ ->
            match font.Face with
            | OutlineFace o ->
                // Monospace if one advance dominates (a mono font + subset may have a
                // few odd-width glyphs, so allow ~10% exceptions rather than require all-equal).
                let ws = o.ShFont.GlyphWidths |> Array.filter (fun w -> w > 0)
                if ws.Length < 2 then false
                else
                    let (_, topCount) = ws |> Array.countBy id |> Array.maxBy snd
                    float topCount / float ws.Length >= 0.9
            | BitmapFace _ -> true)

    let private roundRunAdvances (runs: GlyphRun list) : GlyphRun list =
        let mutable origPen = 0.0
        let mutable newPen = 0.0
        runs |> List.map (fun run ->
            // Re-place glyph origins on the (threaded) pen; round advances for mono fonts.
            let isMono = isMonospaceFont run.Font
            let glyphs =
                run.Glyphs |> Array.map (fun g ->
                    let offset = g.X - origPen
                    let adv = if isMono then System.Math.Round(g.XAdv, System.MidpointRounding.AwayFromZero) else g.XAdv
                    let ng = { g with X = newPen + offset; XAdv = adv }
                    origPen <- origPen + g.XAdv
                    newPen <- newPen + adv
                    ng)
            { run with Glyphs = glyphs })

    let layoutWithFallback (fonts: FontData list) (fontSize: float) (text: string) : GlyphRun list =
        layoutRuns fonts fontSize text |> roundRunAdvances

    let layout (font: FontData) (fontSize: float) (text: string) : GlyphRun list =
        let runs =
            match font.Face with
            | OutlineFace outline ->
                global.Text.Engine.layout outline.TextFont fontSize text
                |> List.map (fun run ->
                    { Font = font
                      FontSize = run.FontSize
                      Glyphs = run.Glyphs |> Array.map (fun g -> { GlyphId = g.GlyphId; X = g.X; Y = g.Y; XAdv = g.XAdv })
                      Direction = run.Direction })
            | BitmapFace _ ->
                let key = (font.Id, text)
                match layoutCache.TryGetValue(key) with
                | true, cached when List.isEmpty cached -> cached
                | true, cached ->
                    if cached.[0].FontSize = fontSize then cached
                    else cached |> List.map (fun r ->
                        let ratio = fontSize / r.FontSize
                        { r with
                            FontSize = fontSize
                            Glyphs = r.Glyphs |> Array.map (fun g ->
                                { g with X = g.X * ratio; Y = g.Y * ratio; XAdv = g.XAdv * ratio }) })
                | _ ->
                    if layoutCache.Count >= layoutCacheMaxEntries then layoutCache.Clear()
                    let runs = layoutRuns [ font ] fontSize text
                    layoutCache.[key] <- runs
                    runs
        roundRunAdvances runs

    let measureWidthWithFallback (fonts: FontData list) (fontSize: float) (text: string) : float =
        layoutWithFallback fonts fontSize text
        |> List.sumBy (fun r -> r.Glyphs |> Array.sumBy (fun g -> g.XAdv))

    // Sum rounded advances (via layout) so measurement matches the integer-pixel
    // grid the glyphs are actually painted on.
    let measureWidth (font: FontData) (fontSize: float) (text: string) : float =
        layout font fontSize text
        |> List.sumBy (fun r -> r.Glyphs |> Array.sumBy (fun g -> g.XAdv))

    let clearCaches () =
        lock loadLock (fun () -> fontCache.Clear())
        outlineCache.Clear()
        global.Text.Engine.clearCaches()
