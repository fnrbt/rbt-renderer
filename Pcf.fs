namespace Renderer

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Text

module Pcf =

    [<Literal>]
    let private pcfMagic = 0x70636601u

    [<Literal>]
    let private pcfProperties = 1u <<< 0

    [<Literal>]
    let private pcfMetrics = 1u <<< 2

    [<Literal>]
    let private pcfBitmaps = 1u <<< 3

    [<Literal>]
    let private pcfEncodings = 1u <<< 5

    [<Literal>]
    let private pcfCompressedMetrics = 0x00000100u

    type PropertyValue =
        | PropertyString of string
        | PropertyNumber of int

    type GlyphMetric = {
        LeftSideBearing: int
        RightSideBearing: int
        CharacterWidth: int
        Ascent: int
        Descent: int
        Attributes: int
    }

    type GlyphBitmap = {
        Alpha: byte[]
        W: int
        H: int
        BearingX: int
        BearingY: int
        Advance: int
        ByteSize: int
    }

    type Font = {
        Glyphs: GlyphBitmap option[]
        Encoding: Map<int, int>
        DefaultGlyph: int option
        PixelSize: int
        Ascender: int
        Descender: int
        DefaultAdvance: int
        Properties: Map<string, PropertyValue>
        Name: string option
    }

    type private TableEntry = {
        Kind: uint32
        Format: uint32
        Size: int
        Offset: int
    }

    let private readU16LE (data: byte[]) (off: int) =
        int data.[off] ||| (int data.[off + 1] <<< 8)

    let private readU16BE (data: byte[]) (off: int) =
        (int data.[off] <<< 8) ||| int data.[off + 1]

    let private readU16 (bigEndian: bool) (data: byte[]) (off: int) =
        if bigEndian then readU16BE data off else readU16LE data off

    let private readI16 (bigEndian: bool) (data: byte[]) (off: int) =
        let u = readU16 bigEndian data off
        if u >= 0x8000 then u - 0x10000 else u

    let private readU32LE (data: byte[]) (off: int) =
        (uint32 data.[off])
        ||| (uint32 data.[off + 1] <<< 8)
        ||| (uint32 data.[off + 2] <<< 16)
        ||| (uint32 data.[off + 3] <<< 24)

    let private readU32BE (data: byte[]) (off: int) =
        (uint32 data.[off] <<< 24)
        ||| (uint32 data.[off + 1] <<< 16)
        ||| (uint32 data.[off + 2] <<< 8)
        ||| uint32 data.[off + 3]

    let private readU32 (bigEndian: bool) (data: byte[]) (off: int) =
        if bigEndian then readU32BE data off else readU32LE data off

    let private isBigEndian (format: uint32) =
        (format &&& 0x00000004u) <> 0u

    let private isMsbBitOrder (format: uint32) =
        (format &&& 0x00000008u) <> 0u

    let private align4 n =
        (n + 3) &&& ~~~3

    let private cString (data: byte[]) (start: int) (limit: int) =
        let mutable stop = start
        while stop < limit && data.[stop] <> 0uy do
            stop <- stop + 1
        if start >= 0 && stop >= start && stop <= data.Length then
            Encoding.ASCII.GetString(data, start, stop - start)
        else
            ""

    let isPcfData (data: byte[]) =
        data.Length >= 8 && readU32LE data 0 = pcfMagic

    let private readPossiblyGzipFile (path: string) =
        let raw = File.ReadAllBytes path
        if raw.Length >= 2 && raw.[0] = 0x1fuy && raw.[1] = 0x8buy then
            use input = new MemoryStream(raw)
            use gzip = new GZipStream(input, CompressionMode.Decompress)
            use output = new MemoryStream()
            gzip.CopyTo(output)
            output.ToArray()
        else
            raw

    let private tables (data: byte[]) =
        if not (isPcfData data) then
            invalidArg "data" "Not a PCF font."

        let count = int (readU32LE data 4)
        if count < 0 || 8 + count * 16 > data.Length then
            invalidArg "data" "Invalid PCF table directory."

        Array.init count (fun i ->
            let p = 8 + i * 16
            { Kind = readU32LE data p
              Format = readU32LE data (p + 4)
              Size = int (readU32LE data (p + 8))
              Offset = int (readU32LE data (p + 12)) })

    let private findTable kind (entries: TableEntry[]) =
        entries |> Array.tryFind (fun e -> e.Kind = kind)

    let private parseProperties (data: byte[]) (entry: TableEntry) =
        let off = entry.Offset
        if off < 0 || off + 8 > data.Length then
            Map.empty
        else
            let format = readU32LE data off
            let big = isBigEndian format
            let count = int (readU32 big data (off + 4))
            let propsStart = off + 8
            let stringsSizeOffset = align4 (propsStart + count * 9)
            if count < 0 || stringsSizeOffset + 4 > data.Length then
                Map.empty
            else
                let stringBytes = int (readU32 big data stringsSizeOffset)
                let stringsStart = stringsSizeOffset + 4
                let stringsLimit = min data.Length (stringsStart + max 0 stringBytes)

                let rawProps =
                    [ for i = 0 to count - 1 do
                        let p = propsStart + i * 9
                        if p + 9 <= data.Length then
                            let nameOffset = int (readU32 big data p)
                            let isString = data.[p + 4] <> 0uy
                            let value = int (readU32 big data (p + 5))
                            yield (nameOffset, isString, value) ]

                rawProps
                |> List.choose (fun (nameOffset, isString, value) ->
                    let nameStart = stringsStart + nameOffset
                    if nameStart < stringsStart || nameStart >= stringsLimit then
                        None
                    else
                        let name = cString data nameStart stringsLimit
                        if String.IsNullOrEmpty name then
                            None
                        elif isString then
                            let valueStart = stringsStart + value
                            if valueStart < stringsStart || valueStart >= stringsLimit then
                                Some (name, PropertyString "")
                            else
                                Some (name, PropertyString (cString data valueStart stringsLimit))
                        else
                            Some (name, PropertyNumber value))
                |> Map.ofList

    let private parseMetrics (data: byte[]) (entry: TableEntry) =
        let off = entry.Offset
        if off < 0 || off + 6 > data.Length then
            [||]
        else
            let format = readU32LE data off
            let big = isBigEndian format
            if (format &&& pcfCompressedMetrics) <> 0u then
                let count = readU16 big data (off + 4)
                let metricsStart = off + 6
                Array.init count (fun i ->
                    let p = metricsStart + i * 5
                    if p + 5 <= data.Length then
                        let b j = int data.[p + j] - 0x80
                        { LeftSideBearing = b 0
                          RightSideBearing = b 1
                          CharacterWidth = b 2
                          Ascent = b 3
                          Descent = b 4
                          Attributes = 0 }
                    else
                        { LeftSideBearing = 0
                          RightSideBearing = 0
                          CharacterWidth = 0
                          Ascent = 0
                          Descent = 0
                          Attributes = 0 })
            else
                if off + 8 > data.Length then
                    [||]
                else
                    let count = int (readU32 big data (off + 4))
                    let metricsStart = off + 8
                    Array.init count (fun i ->
                        let p = metricsStart + i * 12
                        if p + 12 <= data.Length then
                            { LeftSideBearing = readI16 big data p
                              RightSideBearing = readI16 big data (p + 2)
                              CharacterWidth = readI16 big data (p + 4)
                              Ascent = readI16 big data (p + 6)
                              Descent = readI16 big data (p + 8)
                              Attributes = readU16 big data (p + 10) }
                        else
                            { LeftSideBearing = 0
                              RightSideBearing = 0
                              CharacterWidth = 0
                              Ascent = 0
                              Descent = 0
                              Attributes = 0 })

    let private paddedRowBytes width glyphPadBytes =
        let bits = max 0 width
        ((bits + glyphPadBytes * 8 - 1) / (glyphPadBytes * 8)) * glyphPadBytes

    let private parseBitmaps (data: byte[]) (entry: TableEntry) (metrics: GlyphMetric[]) =
        let off = entry.Offset
        if off < 0 || off + 8 > data.Length then
            Array.empty
        else
            let format = readU32LE data off
            let big = isBigEndian format
            let msbBits = isMsbBitOrder format
            let glyphPadBytes = 1 <<< int (format &&& 0x3u)
            let count = int (readU32 big data (off + 4))
            let offsetsStart = off + 8
            let sizesStart = offsetsStart + count * 4
            let bitmapStart = sizesStart + 16

            if count < 0 || sizesStart + 16 > data.Length then
                Array.empty
            else
                let offsets =
                    Array.init count (fun i -> int (readU32 big data (offsetsStart + i * 4)))
                let sizeIndex = int (format &&& 0x3u)
                let bitmapSize = int (readU32 big data (sizesStart + sizeIndex * 4))
                let bitmapLimit = min data.Length (bitmapStart + max 0 bitmapSize)

                Array.init count (fun i ->
                    if i >= metrics.Length then
                        None
                    else
                        let m = metrics.[i]
                        let w = max 0 (m.RightSideBearing - m.LeftSideBearing)
                        let h = max 0 (m.Ascent + m.Descent)
                        let rowBytes = paddedRowBytes w glyphPadBytes
                        let src = bitmapStart + offsets.[i]

                        if w = 0 || h = 0 then
                            Some {
                                Alpha = [||]
                                W = 0
                                H = 0
                                BearingX = m.LeftSideBearing
                                BearingY = -m.Ascent
                                Advance = m.CharacterWidth
                                ByteSize = 0
                            }
                        elif rowBytes <= 0 || src < bitmapStart || src + rowBytes * h > bitmapLimit then
                            None
                        else
                            let alpha = Array.zeroCreate<byte> (w * h)
                            for y = 0 to h - 1 do
                                let row = src + y * rowBytes
                                for x = 0 to w - 1 do
                                    let b = int data.[row + x / 8]
                                    let bit =
                                        if msbBits then
                                            7 - (x &&& 7)
                                        else
                                            x &&& 7
                                    if ((b >>> bit) &&& 1) <> 0 then
                                        alpha.[y * w + x] <- 255uy

                            Some {
                                Alpha = alpha
                                W = w
                                H = h
                                BearingX = m.LeftSideBearing
                                BearingY = -m.Ascent
                                Advance = m.CharacterWidth
                                ByteSize = alpha.Length
                            })

    let private parseEncoding (data: byte[]) (entry: TableEntry) =
        let off = entry.Offset
        if off < 0 || off + 14 > data.Length then
            Map.empty, None
        else
            let format = readU32LE data off
            let big = isBigEndian format
            let minCharOrByte2 = readU16 big data (off + 4)
            let maxCharOrByte2 = readU16 big data (off + 6)
            let minByte1 = readU16 big data (off + 8)
            let maxByte1 = readU16 big data (off + 10)
            let defaultChar = readU16 big data (off + 12)

            if maxCharOrByte2 < minCharOrByte2 || maxByte1 < minByte1 then
                Map.empty, None
            else
                let valuesStart = off + 14
                let values = Dictionary<int, int>()
                let mutable p = valuesStart

                for b1 = minByte1 to maxByte1 do
                    for b2 = minCharOrByte2 to maxCharOrByte2 do
                        if p + 2 <= data.Length then
                            let glyph = readU16 big data p
                            if glyph <> 0xffff then
                                let codepoint =
                                    if minByte1 = 0 && maxByte1 = 0 then
                                        b2
                                    else
                                        (b1 <<< 8) ||| b2
                                values.[codepoint] <- glyph
                            p <- p + 2

                let encoding =
                    values
                    |> Seq.map (fun kv -> kv.Key, kv.Value)
                    |> Map.ofSeq

                let defaultGlyph =
                    match encoding.TryFind defaultChar with
                    | Some glyph -> Some glyph
                    | None -> None

                encoding, defaultGlyph

    let private tryNumber name (properties: Map<string, PropertyValue>) =
        match properties.TryFind name with
        | Some (PropertyNumber value) -> Some value
        | _ -> None

    let private tryString name (properties: Map<string, PropertyValue>) =
        match properties.TryFind name with
        | Some (PropertyString value) when not (String.IsNullOrEmpty value) -> Some value
        | _ -> None

    let load (data: byte[]) : Font =
        let entries = tables data

        let metrics =
            match findTable pcfMetrics entries with
            | Some entry -> parseMetrics data entry
            | None -> invalidArg "data" "PCF font has no metrics table."

        let glyphs =
            match findTable pcfBitmaps entries with
            | Some entry -> parseBitmaps data entry metrics
            | None -> invalidArg "data" "PCF font has no bitmap table."

        let properties =
            match findTable pcfProperties entries with
            | Some entry -> parseProperties data entry
            | None -> Map.empty

        let encoding, defaultGlyph =
            match findTable pcfEncodings entries with
            | Some entry -> parseEncoding data entry
            | None -> Map.empty, None

        let maxAscent =
            metrics
            |> Array.fold (fun acc m -> max acc m.Ascent) 0
        let maxDescent =
            metrics
            |> Array.fold (fun acc m -> max acc m.Descent) 0

        let ascender = tryNumber "FONT_ASCENT" properties |> Option.defaultValue maxAscent
        let descender = tryNumber "FONT_DESCENT" properties |> Option.defaultValue maxDescent
        let pixelSize =
            tryNumber "PIXEL_SIZE" properties
            |> Option.defaultWith (fun () -> max 1 (ascender + descender))
            |> max 1

        let defaultAdvance =
            metrics
            |> Array.map (fun m -> m.CharacterWidth)
            |> Array.filter (fun w -> w > 0)
            |> fun widths ->
                if widths.Length = 0 then
                    pixelSize
                else
                    widths |> Array.averageBy float |> round |> int

        { Glyphs = glyphs
          Encoding = encoding
          DefaultGlyph = defaultGlyph
          PixelSize = pixelSize
          Ascender = ascender
          Descender = descender
          DefaultAdvance = defaultAdvance
          Properties = properties
          Name = tryString "FONT" properties }

    let loadFile (path: string) : Font =
        readPossiblyGzipFile path |> load

    let hasGlyph (font: Font) (codepoint: int) =
        font.Encoding.ContainsKey codepoint

    let glyphIndex (font: Font) (codepoint: int) =
        font.Encoding.TryFind codepoint

    let glyphIndexOrDefault (font: Font) (codepoint: int) =
        match glyphIndex font codepoint with
        | Some glyph -> Some glyph
        | None -> font.DefaultGlyph

    let glyphBitmap (font: Font) (glyphId: int) =
        if glyphId >= 0 && glyphId < font.Glyphs.Length then
            font.Glyphs.[glyphId]
        else
            None

    let glyphAdvance (font: Font) (glyphId: int) =
        match glyphBitmap font glyphId with
        | Some glyph -> glyph.Advance
        | None -> font.DefaultAdvance
