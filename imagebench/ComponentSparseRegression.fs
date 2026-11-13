module ComponentSparseRegression

open Rbt.Renderer

// Reused component scratch must not repeat translucent prior paint where the
// following object is empty. Expected values use independent scalar PDF
// component blending, then composite the isolated group's result onto white.
let run () =
    let bounds = rect 0.0 0.0 1.0 1.0
    let marker n = FillRect (bounds, Color.rgb n 0uy 0uy)
    let first = Components4 (marker 1uy, marker 2uy)
    let second = Components4 (marker 3uy, marker 4uy)
    let empty = Components4 (marker 5uy, marker 6uy)
    let conversion : ColorSpaceConversion4 =
        { ParentComponents = 3; GroupComponents = 4; Complement = true
          ToGroup = fun r g b _ -> struct (r, g, 0.0, b)
          ToParent = fun c m _ k -> struct (c, m, k, 0.0) }
    let samples =
        [| [| 0x80335500u; 0x80223300u; 0x80557700u; 0x80112200u |]
           [| 0x80660000u; 0x80330000u; 0x80110000u; 0x80770000u |]
           [| 0u; 0x80402010u; 0u; 0x800c0810u |]
           [| 0u; 0x80300000u; 0u; 0x80200000u |]
           Array.zeroCreate 4
           Array.zeroCreate 4 |]
    let render (positions: int[]) mode =
        let backdrop = Array.create positions.Length 0xffffffffu
        let raster commands : ComponentCompositor.RasterRect =
            match commands with
            | [FillRect (_, c)] ->
                { ComponentCompositor.RasterRect.X = 0; Y = 0; Width = positions.Length; Height = 1
                  Pixels = positions |> Array.map (fun p -> samples.[int c.R - 1].[p]) }
            | _ -> failwith "Unexpected component raster command"
        let command = ColorManagedGroup4 (conversion, true, false, 1.0, Normal, None,
                                          [ first; Blend (mode, [second]); empty ])
        let bounds _ : ComponentCompositor.Bounds = { X = 0; Y = 0; Width = positions.Length; Height = 1 }
        ComponentCompositor.render positions.Length 1 backdrop raster bounds command
        backdrop
    for mode in [Normal; Multiply] do
        let combined = render [|0; 1; 2; 3|] mode
        for p in 0 .. combined.Length - 1 do
            let da = float (samples.[0].[p] >>> 24) / 255.0
            let sa = float (samples.[2].[p] >>> 24) / 255.0
            let alpha = sa + da * (1.0-sa)
            for channel, shift in [0,16; 1,8; 3,0] do
                let value plane =
                    let packed = samples.[plane + (if channel=3 then 1 else 0)].[p]
                    let shift = if channel=1 then 8 else 16
                    float ((packed >>> shift) &&& 255u) / 255.0
                let d, s = value 0, value 2
                let result =
                    if mode=Normal then s+d*(1.0-sa)
                    else
                        let b = if da=0.0 then 0.0 else d/da
                        let s' = if sa=0.0 then 0.0 else s/sa
                        let blend = b+s'-b*s' // Multiply in complemented CMYK
                        (1.0-sa)*d + (1.0-da)*s + sa*da*blend
                let expected = int ((result+1.0-alpha)*255.0+0.5)
                let actual = int ((combined.[p] >>> shift) &&& 255u)
                if abs (actual-expected) > 1 then
                    failwithf "Sparse component paint pixel %d, channel %d, %A: %d <> %d" p channel mode actual expected
            if combined.[p] >>> 24 <> 255u then failwith "White backdrop lost opacity"
