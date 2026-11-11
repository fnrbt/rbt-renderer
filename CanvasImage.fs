module Rbt.Renderer.CanvasImage

open Fable.Core
open Fable.Core.JsInterop
open Rbt.Renderer
open Rbt.Renderer.CanvasApi

/// Read a browser surface as premultiplied BGRA pixels.
let pixelsFromCanvas (oc: OffscreenCanvas) =
    let imageData = (getOffscreenContext2DReadFrequent oc).getImageData(0.0, 0.0, float oc.width, float oc.height)
    let rgba: uint32[] = emitJsExpr imageData.data "new Uint32Array($0.buffer)"
    rgba |> Array.map (fun v ->
        let a = v >>> 24
        let channel shift = (((v >>> shift) &&& 255u) * a + 127u) / 255u
        channel 16 ||| (channel 8 <<< 8) ||| (channel 0 <<< 16) ||| (a <<< 24))

// ── Framebuffer → OffscreenCanvas cache ──────────────────
// Keyed by fb.Px (uint32[]) identity via JS WeakMap.

let private fbCache : obj = createWeakMap ()

/// Convert a Framebuffer to an OffscreenCanvas, cached by pixel array identity.
/// Un-premultiplies BGRA pixels to RGBA for ImageData, then putImageData.
let canvasFor (fb: Framebuffer) : OffscreenCanvas =
    let px =
        match Framebuffer.tryGetArray fb with
        | Some arr -> arr
        | None -> failwith "canvasFor: non-array-backed Framebuffer"
    match weakMapTryGet<OffscreenCanvas> fbCache px with
    | Some oc -> oc
    | None ->
        let w = fb.W
        let h = fb.H
        let oc = createOffscreenCanvas w h
        let ctx = getOffscreenContext2D oc
        let imageData = createImageDataEmpty w h
        // Uint32Array view over ImageData.data for fast 32-bit writes
        let dst : uint32[] = emitJsExpr imageData.data "new Uint32Array($0.buffer)"
        for i in 0 .. px.Length - 1 do
            let v = px.[i]
            let a = v >>> 24
            if a = 0u then
                dst.[i] <- 0u
            elif a = 255u then
                // Opaque: BGRA→RGBA swap (B↔R, keep G and A)
                // Input LE: B|(G<<8)|(R<<16)|(A<<24)  Output LE: R|(G<<8)|(B<<16)|(A<<24)
                dst.[i] <- (v &&& 0xFF00FF00u) ||| ((v &&& 0xFFu) <<< 16) ||| ((v >>> 16) &&& 0xFFu)
            else
                // Un-premultiply + BGRA→RGBA
                let ia = 255.0 / float a
                let b = v &&& 0xFFu
                let g = (v >>> 8) &&& 0xFFu
                let r = (v >>> 16) &&& 0xFFu
                let ru = min 255u (uint32 (float r * ia))
                let gu = min 255u (uint32 (float g * ia))
                let bu = min 255u (uint32 (float b * ia))
                // RGBA LE: R|(G<<8)|(B<<16)|(A<<24)
                dst.[i] <- ru ||| (gu <<< 8) ||| (bu <<< 16) ||| (a <<< 24)
        ctx.putImageData(imageData, 0.0, 0.0)
        weakMapSet<OffscreenCanvas> fbCache px oc
        oc
