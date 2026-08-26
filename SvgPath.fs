namespace Rbt.Renderer

module SvgPath =

    // =========================================================================
    // Path Commands (intermediate AST)
    // =========================================================================

    type private Point = { X: float; Y: float }

    type private PathCmd =
        | MoveTo of Point * absolute: bool
        | LineTo of Point * absolute: bool
        | HLine of float * absolute: bool
        | VLine of float * absolute: bool
        | CubicTo of c1: Point * c2: Point * p: Point * absolute: bool
        | SmoothCubicTo of c2: Point * p: Point * absolute: bool
        | QuadTo of c: Point * p: Point * absolute: bool
        | SmoothQuadTo of Point * absolute: bool
        | Arc of rx: float * ry: float * rotation: float * largeArc: bool * sweep: bool * p: Point * absolute: bool
        | Close

    // =========================================================================
    // Parser State & Primitives
    // =========================================================================

    type private Parser = { Input: string; mutable Pos: int }

    let inline private eof p = p.Pos >= p.Input.Length
    let inline private peek p = if eof p then '\000' else p.Input.[p.Pos]
    let inline private advance p = p.Pos <- p.Pos + 1
    let inline private skip f p = while not (eof p) && f (peek p) do advance p

    let inline private isWs c = c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = ','
    let inline private isDigit c = c >= '0' && c <= '9'
    let inline private isNumStart c = isDigit c || c = '-' || c = '+' || c = '.'

    let private skipWs p = skip isWs p

    let private number p : float option =
        skipWs p
        if eof p || not (isNumStart (peek p)) then None
        else
            let start = p.Pos
            if peek p = '-' || peek p = '+' then advance p
            while not (eof p) && isDigit (peek p) do advance p
            if not (eof p) && peek p = '.' then
                advance p
                while not (eof p) && isDigit (peek p) do advance p
            if not (eof p) && (peek p = 'e' || peek p = 'E') then
                advance p
                if not (eof p) && (peek p = '-' || peek p = '+') then advance p
                while not (eof p) && isDigit (peek p) do advance p
            try Some (float (p.Input.Substring(start, p.Pos - start)))
            with _ -> None

    let private flag p : bool option =
        skipWs p
        match peek p with
        | '0' -> advance p; Some false
        | '1' -> advance p; Some true
        | _ -> None

    let private command p : char option =
        skipWs p
        let c = peek p
        if System.Char.IsLetter c then advance p; Some c else None

    // =========================================================================
    // Command parsers
    // =========================================================================

    let private point p =
        match number p with
        | Some x -> match number p with Some y -> Some { X = x; Y = y } | None -> None
        | None -> None

    let private isAbs c = System.Char.IsUpper c

    let private parseMoveTo abs p = point p |> Option.map (fun pt -> MoveTo (pt, abs))
    let private parseLineTo abs p = point p |> Option.map (fun pt -> LineTo (pt, abs))
    let private parseHLine abs p = number p |> Option.map (fun x -> HLine (x, abs))
    let private parseVLine abs p = number p |> Option.map (fun y -> VLine (y, abs))

    let private parseCubic abs p =
        match point p, point p, point p with
        | Some c1, Some c2, Some pt -> Some (CubicTo (c1, c2, pt, abs))
        | _ -> None

    let private parseSmoothCubic abs p =
        match point p, point p with
        | Some c2, Some pt -> Some (SmoothCubicTo (c2, pt, abs))
        | _ -> None

    let private parseQuad abs p =
        match point p, point p with
        | Some c, Some pt -> Some (QuadTo (c, pt, abs))
        | _ -> None

    let private parseSmoothQuad abs p = point p |> Option.map (fun pt -> SmoothQuadTo (pt, abs))

    let private parseArc abs p =
        match number p, number p, number p, flag p, flag p, point p with
        | Some rx, Some ry, Some rot, Some large, Some sweep, Some pt ->
            Some (Arc (rx, ry, rot, large, sweep, pt, abs))
        | _ -> None

    let private parseClose _ = Some Close

    // =========================================================================
    // Main parser
    // =========================================================================

    let private getParser (c: char) : (bool -> Parser -> PathCmd option) option =
        match System.Char.ToUpper c with
        | 'M' -> Some parseMoveTo
        | 'L' -> Some parseLineTo
        | 'H' -> Some parseHLine
        | 'V' -> Some parseVLine
        | 'C' -> Some parseCubic
        | 'S' -> Some parseSmoothCubic
        | 'Q' -> Some parseQuad
        | 'T' -> Some parseSmoothQuad
        | 'A' -> Some parseArc
        | 'Z' -> Some (fun _ _ -> parseClose ())
        | _ -> None

    let private parseCmds (d: string) : PathCmd list =
        let p = { Input = d; Pos = 0 }
        let cmds = System.Collections.Generic.List<PathCmd>()
        let mutable currentParser: (bool -> Parser -> PathCmd option) option = None
        let mutable currentAbs = true

        while not (eof p) do
            skipWs p
            if not (eof p) then
                match command p with
                | Some c ->
                    match getParser c with
                    | Some parser ->
                        currentParser <- Some parser
                        currentAbs <- isAbs c
                        match parser currentAbs p with
                        | Some cmd ->
                            cmds.Add cmd
                            // SVG spec: implicit repeats after M/m are L/l
                            match cmd with
                            | MoveTo _ -> currentParser <- Some parseLineTo
                            | _ -> ()
                        | None -> ()
                    | None -> ()
                | None ->
                    match currentParser with
                    | Some parser ->
                        let effectiveParser =
                            if cmds.Count > 0 then
                                match cmds.[cmds.Count - 1] with
                                | MoveTo _ -> parseLineTo
                                | _ -> parser
                            else parser
                        match effectiveParser currentAbs p with
                        | Some cmd -> cmds.Add cmd
                        | None -> advance p
                    | None -> advance p

        cmds |> System.Linq.Enumerable.ToList |> List.ofSeq

    // =========================================================================
    // PathCmd list → PathSeg list
    // =========================================================================

    let private toPathSegs (cmds: PathCmd list) : PathSeg list =
        let result = System.Collections.Generic.List<PathSeg>()
        let mutable curX, curY = 0.0, 0.0
        let mutable startX, startY = 0.0, 0.0
        let mutable lastC2 : (float * float) option = None
        let mutable lastQuadC : (float * float) option = None

        let abs' (pt: Point) isAbs =
            if isAbs then (pt.X, pt.Y) else (curX + pt.X, curY + pt.Y)

        for cmd in cmds do
            match cmd with
            | MoveTo (pt, isAbs) ->
                let (x, y) = abs' pt isAbs
                result.Add(PathSeg.MoveTo (vec2 x y))
                curX <- x; curY <- y; startX <- x; startY <- y
                lastC2 <- None; lastQuadC <- None

            | LineTo (pt, isAbs) ->
                let (x, y) = abs' pt isAbs
                result.Add(PathSeg.LineTo (vec2 x y))
                curX <- x; curY <- y; lastC2 <- None; lastQuadC <- None

            | HLine (v, isAbs) ->
                let x = if isAbs then v else curX + v
                result.Add(PathSeg.LineTo (vec2 x curY))
                curX <- x; lastC2 <- None; lastQuadC <- None

            | VLine (v, isAbs) ->
                let y = if isAbs then v else curY + v
                result.Add(PathSeg.LineTo (vec2 curX y))
                curY <- y; lastC2 <- None; lastQuadC <- None

            | CubicTo (c1, c2, pt, isAbs) ->
                let (x1, y1) = abs' c1 isAbs
                let (x2, y2) = abs' c2 isAbs
                let (x, y) = abs' pt isAbs
                result.Add(PathSeg.CubicTo (vec2 x1 y1, vec2 x2 y2, vec2 x y))
                curX <- x; curY <- y; lastC2 <- Some (x2, y2); lastQuadC <- None

            | SmoothCubicTo (c2, pt, isAbs) ->
                let (x1, y1) = match lastC2 with Some (lx, ly) -> (2.0*curX - lx, 2.0*curY - ly) | None -> (curX, curY)
                let (x2, y2) = abs' c2 isAbs
                let (x, y) = abs' pt isAbs
                result.Add(PathSeg.CubicTo (vec2 x1 y1, vec2 x2 y2, vec2 x y))
                curX <- x; curY <- y; lastC2 <- Some (x2, y2); lastQuadC <- None

            | QuadTo (c, pt, isAbs) ->
                let (cx, cy) = abs' c isAbs
                let (x, y) = abs' pt isAbs
                result.Add(PathSeg.QuadTo (vec2 cx cy, vec2 x y))
                curX <- x; curY <- y; lastC2 <- None; lastQuadC <- Some (cx, cy)

            | SmoothQuadTo (pt, isAbs) ->
                let (cx, cy) = match lastQuadC with Some (lx, ly) -> (2.0*curX - lx, 2.0*curY - ly) | None -> (curX, curY)
                let (x, y) = abs' pt isAbs
                result.Add(PathSeg.QuadTo (vec2 cx cy, vec2 x y))
                curX <- x; curY <- y; lastC2 <- None; lastQuadC <- Some (cx, cy)

            | Arc (rx, ry, rot, large, sweep, pt, isAbs) ->
                let (x, y) = abs' pt isAbs
                let rotRad = rot * System.Math.PI / 180.0
                result.Add(PathSeg.ArcTo (rx * 1.0<px>, ry * 1.0<px>, rotRad, large, sweep, vec2 x y))
                curX <- x; curY <- y; lastC2 <- None; lastQuadC <- None

            | Close ->
                result.Add(PathSeg.Close)
                curX <- startX; curY <- startY; lastC2 <- None; lastQuadC <- None

        result |> List.ofSeq

    /// Parse SVG path data string into PathSeg list
    let parse (d: string) : PathSeg list =
        parseCmds d |> toPathSegs
