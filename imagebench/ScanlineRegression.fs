module ScanlineRegression

open Rbt.Renderer

// Subdividing an edge without changing its geometry must not change coverage.
// A long contour also exercises the scanline traversal's asymptotic behavior.
let run () =
    let simple = [ [ (2.0, 2.0); (14.0, 2.0); (14.0, 14.0); (2.0, 14.0); (2.0, 2.0) ] ]
    let longContour =
        [ (2.0, 2.0) ] @ List.replicate 12000 (14.0, 2.0)
        @ [ (14.0, 14.0); (2.0, 14.0); (2.0, 2.0) ]
        |> List.singleton
    let gradient =
        LinearGradient (vec2 0.0 0.0, vec2 16.0 0.0,
                        [ { Offset = 0.0; StopColor = Color.black }
                          { Offset = 1.0; StopColor = Color.white } ], Pad)
    for rule in [ EvenOdd; NonZero ] do
        let render paint contours =
            let fb = Framebuffer.create 16 16 (Color.rgba 0uy 0uy 0uy 0uy)
            Raster.scanlineFillPaintAA fb contours paint 4 rule
            fb.Px.ToArray()
        for paint in [ Solid Color.black; gradient ] do
            if render paint simple <> render paint longContour then
                failwithf "Subdivided scanline changed %A coverage" rule
            let pixels = render paint simple
            if Pixel.a pixels.[0] <> 0uy || Pixel.a pixels.[8 * 16 + 8] <> 255uy then
                failwith "Rectangle coverage must exclude the exterior and fill the interior"
            match paint with
            | Solid _ when pixels.[8 * 16 + 8] <> 0xff000000u ->
                failwith "Solid rectangle must paint opaque black"
            | LinearGradient _ when Pixel.r pixels.[8 * 16 + 3] >= Pixel.r pixels.[8 * 16 + 12] ->
                failwith "Linear gradient must increase from black toward white"
            | _ -> ()
        let clip contours = Raster.buildClipMaskAA 16 16 contours 4 rule
        if clip simple <> clip longContour then
            failwithf "Subdivided clip changed %A coverage" rule
        let mask = clip simple
        if mask.[0] <> 0uy || mask.[8 * 16 + 8] <> 255uy then
            failwith "Clip must exclude the exterior and cover the interior"
