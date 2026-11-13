module Rbt.Renderer.CanvasGlyphCache

open System.Collections.Generic
open Rbt.Renderer

type GlyphPath =
    { Path: CanvasApi.Path2D
      Bounds: Rect option }

/// Cache paths and conservative control-point bounds together, so repeated
/// glyph paints do not walk the outline again just to size a scratch surface.
/// Uses a single int key to avoid struct-tuple allocation per lookup under Fable.
let private cache = Dictionary<int64, GlyphPath option>()

let inline private key (fontId: int) (gid: int) = (int64 fontId <<< 20) ||| int64 gid

/// Returns None for glyphs without outlines; unknown bounds retain a full-surface raster.
let get (font: Text.FontData) (gid: int) : GlyphPath option =
    let k = key font.Id gid
    match cache.TryGetValue(k) with
    | true, p -> p
    | _ ->
        let segs = Text.glyphOutline font gid
        let p =
            if segs.IsEmpty then None
            else
                let mutable x0, y0 = infinity, infinity
                let mutable x1, y1 = -infinity, -infinity
                let mutable unknown = false
                let includePoint (point: Vec2) =
                    x0 <- min x0 (float point.X); y0 <- min y0 (float point.Y)
                    x1 <- max x1 (float point.X); y1 <- max y1 (float point.Y)
                for segment in segs do
                    match segment with
                    | MoveTo p | LineTo p -> includePoint p
                    | QuadTo (c,p) -> includePoint c; includePoint p
                    | CubicTo (a,b,p) -> includePoint a; includePoint b; includePoint p
                    | ArcTo _ -> unknown <- true
                    | Close -> ()
                let bounds =
                    if unknown || x0 = infinity then None
                    else Some (rect x0 y0 (x1-x0) (y1-y0))
                Some { Path = CanvasApi.toPath2D segs; Bounds = bounds }
        cache.[k] <- p
        p

/// Clear the entire glyph cache (e.g. on document close).
let clear () =
    cache.Clear()
