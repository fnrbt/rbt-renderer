module ImageBench.Program

open Rbt.Renderer

[<EntryPoint>]
let main args =
    let useGl = args |> Array.contains "--gl"
    if not useGl then
        ScanlineRegression.run ()
        printfn "scanline coverage: ok"
        ComponentSparseRegression.run ()
        printfn "sparse component coverage: ok"
        ComponentBoundsRegression.run ()
        printfn "bounded component coverage: ok"
    for name, width, height, commands, expectations in ImageBench.Fixtures.cases do
        let backend = if useGl then GlBackend.create () else Raster.create ()
        let fb = Scene.render backend width height (Clear (Color.rgba 0uy 0uy 0uy 0uy) :: commands)
        for x, y, expected in expectations do
            let actual = fb.Px.Span.[y * width + x]
            if actual <> expected then
                failwithf "%s (%d,%d): expected %08X, got %08X" name x y expected actual
        printfn "%s: ok" name
    0
