module ComponentBoundsRegression

open Rbt.Renderer

let private width, height = 25, 19
let private firstColor = Color.rgb 40uy 105uy 190uy
let private fourthColor = Color.rgb 175uy 175uy 175uy

let private pixels command =
    let surface = Framebuffer.create width height (Color.rgba 0uy 0uy 0uy 0uy)
    Raster.dispatch surface command
    surface.Px.ToArray()

let private check tolerance label (expected: uint32[]) (actual: Framebuffer) =
    let result = actual.Px.Span
    for i = 0 to expected.Length - 1 do
        let expectedPixel, actualPixel = expected.[i], result.[i]
        let mutable mismatch = false
        for channel = 0 to 3 do
            let shift = channel * 8
            if abs (int ((actualPixel >>> shift) &&& 255u) - int ((expectedPixel >>> shift) &&& 255u)) > tolerance then
                mismatch <- true
        if mismatch then
            failwithf "%s at (%d,%d): expected %08X, got %08X" label (i % width) (i / width) expected.[i] result.[i]

let private pair make = Components4 (make firstColor, make fourthColor)

// Opaque, integer-aligned geometry makes the independent RGB raster an exact
// per-plane oracle, including premultiplied alpha and pixels outside the paint.
let private geometry () =
    let edge = rect -2.0 2.0 5.0 4.0
    let farEdge = rect 22.0 16.0 6.0 5.0
    let turn = { M11 = 0.0; M12 = 1.0; M21 = -1.0; M22 = 0.0; DX = 15.0; DY = 3.0 }
    let shift = { Matrix3x2.identity with DX = 1.0; DY = -1.0 }
    let clipPath =
        [ MoveTo (vec2 7.0 11.0); LineTo (vec2 13.0 11.0)
          LineTo (vec2 13.0 15.0); LineTo (vec2 7.0 15.0); Close ]
    let strokePath =
        [ MoveTo (vec2 17.0 3.0); LineTo (vec2 20.0 8.0); LineTo (vec2 23.0 3.0) ]
    let makeStroke color =
        StrokePath (strokePath, { Width = 3.0<px>; Cap = Butt; Join = Miter
                                  Paint = Solid color; DashArray = []; DashOffset = 0.0<px>; MiterLimit = 8.0 })
    let makes : (Color -> Cmd) list =
        [ (fun color -> FillRect (edge, color))
          (fun color -> FillRect (farEdge, color))
          (fun color -> Group (turn, 1.0, [FillRect (rect 2.0 1.0 4.0 3.0, color)]))
          (fun color -> Clip (clipPath, NonZero, [FillRect (rect 5.0 9.0 11.0 8.0, color)]))
          (fun color -> Group (shift, 1.0, [makeStroke color]))
          (fun color -> FillRect (rect -8.0 -8.0 2.0 2.0, color)) ]
    let first, fourth = Raster.rasterizeComponents width height Matrix3x2.identity 0 false (List.map pair makes)
    check 0 "offset/edge/transform/clip/stroke RGB" (pixels (SaveRestore (List.map (fun make -> make firstColor) makes))) first
    check 0 "offset/edge/transform/clip/stroke K" (pixels (SaveRestore (List.map (fun make -> make fourthColor) makes))) fourth
    for x, y in [0, 3; 24, 18; 12, 6; 9, 12; 20, 5] do
        let index = y * width + x
        if Pixel.a first.Px.Span.[index] = 0uy || Pixel.a fourth.Px.Span.[index] = 0uy then
            failwithf "Expected component paint at (%d,%d)" x y
    // The public transform argument must affect the component planes in the
    // same device coordinates as a direct RGB render, including negative origin.
    let external = { Matrix3x2.identity with DX = -3.0; DY = 4.0 }
    let paint color = FillRect (rect 4.0 -2.0 6.0 5.0, color)
    let a, k = Raster.rasterizeComponents width height external 0 false [pair paint]
    check 0 "external transform RGB" (pixels (Group (external, 1.0, [paint firstColor]))) a
    check 0 "external transform K" (pixels (Group (external, 1.0, [paint fourthColor]))) k

    // Fractional coverage and a translucent fourth plane retain the same
    // alpha even at the outermost antialiased pixel of the rasterized object.
    let alphaPaint color = FillRect (rect 3.25 6.25 5.25 3.5, color)
    let rgb = Color.rgba 60uy 110uy 200uy 160uy
    let gray = Color.rgba 180uy 180uy 180uy 160uy
    let aaFirst, aaFourth =
        Raster.rasterizeComponents width height Matrix3x2.identity 4 false
            [Components4 (alphaPaint rgb, alphaPaint gray)]
    check 0 "fractional translucent RGB" (pixels (AntiAlias (4, [alphaPaint rgb]))) aaFirst
    check 0 "fractional translucent K" (pixels (AntiAlias (4, [alphaPaint gray]))) aaFourth
    for i = 0 to width * height - 1 do
        if Pixel.a aaFirst.Px.Span.[i] <> Pixel.a aaFourth.Px.Span.[i] then
            failwithf "Component plane alpha differs at (%d,%d)" (i % width) (i / width)

// A nested knockout and a non-Normal object must not borrow the bounds of the
// preceding directly unpacked Components4 paint. Compare each component against
// an independently rendered RGB transparency scene, not another component path.
let private nested () =
    let baseRect = rect 1.0 1.0 5.0 5.0
    let distantRect = rect 15.0 10.0 8.0 6.0
    let overlap = rect 18.0 12.0 4.0 3.0
    let make color =
        [ FillRect (baseRect, color)
          TransparencyGroup (true, true, 1.0, Normal, None,
              [FillRect (distantRect, color); FillRect (overlap, color)])
          Blend (Multiply, [FillRect (rect 19.0 11.0 3.0 5.0, color)]) ]
    let componentCommands =
        [ pair (fun c -> FillRect (baseRect, c))
          TransparencyGroup (true, true, 1.0, Normal, None,
              [pair (fun c -> FillRect (distantRect, c)); pair (fun c -> FillRect (overlap, c))])
          Blend (Multiply, [pair (fun c -> FillRect (rect 19.0 11.0 3.0 5.0, c))]) ]
    let first, fourth = Raster.rasterizeComponents width height Matrix3x2.identity 0 false componentCommands
    // RGB blends quantize intermediate bytes; component blends retain float
    // precision. Permit their one-level rounding difference, not lost coverage.
    check 1 "nested knockout/multiply RGB" (pixels (TransparencyGroup (true, false, 1.0, Normal, None, make firstColor))) first
    check 1 "nested knockout/multiply K" (pixels (TransparencyGroup (true, false, 1.0, Normal, None, make fourthColor))) fourth

let run () =
    geometry ()
    nested ()
