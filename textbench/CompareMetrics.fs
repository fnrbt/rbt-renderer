module TextBench.CompareMetrics

open Rbt.Renderer

type CompareResult = {
    TestName: string
    OurAdvanceWidth: float
    SkiaAdvanceWidth: float
    AdvanceDeltaPx: float
    AdvanceDeltaPct: float
    RMSE: float
    PixelsDiffGt10: int
    PixelsDiffGt50: int
    TotalPixels: int
}

/// Compute per-pixel RMSE between two framebuffers (comparing RGB channels).
let rmse (a: Framebuffer) (b: Framebuffer) : float =
    let mutable sumSq = 0.0
    let n = min a.Px.Length b.Px.Length
    let apx = a.Px.Span
    let bpx = b.Px.Span
    for i = 0 to n - 1 do
        let pa = Color.fromPremul (Pixel.toColor apx.[i])
        let pb = Color.fromPremul (Pixel.toColor bpx.[i])
        let dr = float pa.R - float pb.R
        let dg = float pa.G - float pb.G
        let db = float pa.B - float pb.B
        sumSq <- sumSq + dr * dr + dg * dg + db * db
    sqrt (sumSq / float (n * 3))

/// Count pixels where max-channel delta exceeds threshold.
let countAboveThreshold (a: Framebuffer) (b: Framebuffer) (threshold: int) : int =
    let n = min a.Px.Length b.Px.Length
    let mutable count = 0
    let apx = a.Px.Span
    let bpx = b.Px.Span
    for i = 0 to n - 1 do
        let pa = Color.fromPremul (Pixel.toColor apx.[i])
        let pb = Color.fromPremul (Pixel.toColor bpx.[i])
        let d = max (abs (int pa.R - int pb.R)) (max (abs (int pa.G - int pb.G)) (abs (int pa.B - int pb.B)))
        if d > threshold then count <- count + 1
    count

/// Generate a diff framebuffer with amplified absolute differences (4x).
let diffImage (a: Framebuffer) (b: Framebuffer) : Framebuffer =
    let w = a.W
    let h = a.H
    let fb = Framebuffer.create w h Color.black
    let n = min a.Px.Length b.Px.Length
    let apx = a.Px.Span
    let bpx = b.Px.Span
    let fpx = fb.Px.Span
    for i = 0 to n - 1 do
        let pa = Color.fromPremul (Pixel.toColor apx.[i])
        let pb = Color.fromPremul (Pixel.toColor bpx.[i])
        let amp = 4
        let clamp v = byte (min 255 (max 0 v))
        let dr = clamp (abs (int pa.R - int pb.R) * amp)
        let dg = clamp (abs (int pa.G - int pb.G) * amp)
        let db = clamp (abs (int pa.B - int pb.B) * amp)
        fpx.[i] <- Pixel.fromColorPremul (Color.rgb dr dg db)
    fb

/// Build a full CompareResult for a named test case.
let compare (name: string) (ourFb: Framebuffer) (skiaFb: Framebuffer) (ourAdvance: float) (skiaAdvance: float) : CompareResult =
    let deltaPx = ourAdvance - skiaAdvance
    let deltaPct =
        if skiaAdvance > 0.0 then 100.0 * deltaPx / skiaAdvance
        else 0.0
    {
        TestName = name
        OurAdvanceWidth = ourAdvance
        SkiaAdvanceWidth = skiaAdvance
        AdvanceDeltaPx = deltaPx
        AdvanceDeltaPct = deltaPct
        RMSE = rmse ourFb skiaFb
        PixelsDiffGt10 = countAboveThreshold ourFb skiaFb 10
        PixelsDiffGt50 = countAboveThreshold ourFb skiaFb 50
        TotalPixels = ourFb.W * ourFb.H
    }

/// Format a list of results as a tabular report string.
let formatReport (results: CompareResult list) : string =
    let sb = System.Text.StringBuilder()
    sb.AppendLine("TEXT RENDERING BENCHMARK: Ours vs SkiaSharp") |> ignore
    sb.AppendLine("============================================") |> ignore
    sb.AppendLine() |> ignore
    sb.AppendLine(
        sprintf "%-35s %8s %8s %8s %7s %8s %8s %8s"
            "Test" "OurAdv" "SkiaAdv" "DeltaPx" "Delta%" "RMSE" ">10" ">50"
    ) |> ignore
    sb.AppendLine(System.String('-', 102)) |> ignore
    for r in results do
        sb.AppendLine(
            sprintf "%-35s %8.1f %8.1f %+8.1f %+6.1f%% %8.2f %8d %8d"
                r.TestName r.OurAdvanceWidth r.SkiaAdvanceWidth
                r.AdvanceDeltaPx r.AdvanceDeltaPct r.RMSE
                r.PixelsDiffGt10 r.PixelsDiffGt50
        ) |> ignore
    sb.AppendLine() |> ignore

    // Summary
    let avgRmse = results |> List.averageBy (fun r -> r.RMSE)
    let maxRmse = results |> List.maxBy (fun r -> r.RMSE)
    let avgAdvDelta = results |> List.averageBy (fun r -> abs r.AdvanceDeltaPct)
    sb.AppendLine(sprintf "Average RMSE:         %.2f" avgRmse) |> ignore
    sb.AppendLine(sprintf "Worst RMSE:           %.2f  (%s)" maxRmse.RMSE maxRmse.TestName) |> ignore
    sb.AppendLine(sprintf "Avg |advance delta|:  %.1f%%" avgAdvDelta) |> ignore

    sb.ToString()
