module ImageBench.Fixtures

open Rbt.Renderer

let checker =
    Framebuffer.ofArray (Array.init 64 (fun i -> if (i % 8 + i / 8) % 2 = 0 then 0xFF000000u else 0xFFFFFFFFu)) 8 8

let transparentEdge = Framebuffer.ofArray [| 0xFFFF0000u; 0u |] 2 1
let stripes =
    Framebuffer.ofArray (Array.init 32 (fun i ->
        let x = i % 8
        if x < 2 || x >= 6 then 0xFF0000FFu
        elif x < 4 then 0xFFFF0000u else 0xFF00FF00u)) 8 4

// All fixtures have transparent backgrounds. Coordinates in expectations are
// device pixel coordinates. Expected colors are premultiplied BGRA words.
let cases : (string * int * int * Cmd list * (int * int * uint32) list) list =
    [ "checker", 2, 2,
      [ DrawImageReduced(checker, rect 0.0 0.0 8.0 8.0, rect 0.0 0.0 2.0 2.0, false) ],
      [ 0, 0, 0xFF808080u; 1, 1, 0xFF808080u ]
      "nearest", 2, 2,
      [ DrawImage(checker, rect 0.0 0.0 8.0 8.0, rect 0.0 0.0 2.0 2.0, false) ],
      [ 0, 0, 0xFF000000u; 1, 1, 0xFF000000u ]
      "alpha", 1, 1,
      [ DrawImageReduced(transparentEdge, rect 0.0 0.0 2.0 1.0, rect 0.0 0.0 1.0 1.0, false) ],
      [ 0, 0, 0x80800000u ]
      "subrect-mirror", 2, 4,
      [ Group({ M11 = -1.0; M12 = 0.0; M21 = 0.0; M22 = 2.0; DX = 2.0; DY = 0.0 }, 1.0,
              [ DrawImageReduced(stripes, rect 2.0 0.0 4.0 4.0, rect 0.0 0.0 2.0 2.0, false) ]) ],
      [ 0, 0, 0xFF00FF00u; 1, 0, 0xFFFF0000u; 0, 3, 0xFF00FF00u; 1, 3, 0xFFFF0000u ]
      "subrect-rotate", 2, 2,
      [ Group({ M11 = 0.0; M12 = 1.0; M21 = -1.0; M22 = 0.0; DX = 2.0; DY = 0.0 }, 1.0,
              [ DrawImageReduced(stripes, rect 2.0 0.0 4.0 4.0, rect 0.0 0.0 2.0 2.0, false) ]) ],
      [ 0, 0, 0xFFFF0000u; 1, 0, 0xFFFF0000u; 0, 1, 0xFF00FF00u; 1, 1, 0xFF00FF00u ]
      "explicit-shape", 1, 1,
      [ Cmd.shapeOf (WithShape(lazy [FillRect(rect 0.0 0.0 1.0 1.0, Color.white)],
            [DrawImageReduced(transparentEdge, rect 0.0 0.0 2.0 1.0, rect 0.0 0.0 1.0 1.0, false)])) ],
      [ 0, 0, 0xFFFFFFFFu ] ]
