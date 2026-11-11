# GL backend: direct presentation, not readback

`GlBackend.fs` currently rasterizes the cmd list on the GPU and then
`glReadPixels` the entire FBO back to a CPU `Framebuffer`. That
contract makes sense for PNG export, PDF rasterization, and parity
tests against reference renders — those genuinely want pixels. For a
live window it throws away the win the GPU was supposed to provide.

This doc is the plan for adding a direct-to-window variant. The
existing offscreen / readback backend stays — they're sibling
capabilities, not a replacement.

## What's there today

`renderer/GlBackend.fs` exposes two constructors:

- `create () : Backend` (line 1858) — one-shot. `Init w h` builds an
  EGL pbuffer surface, compiles shaders, allocates a soft fallback FB.
  `Submit cmds` dispatches each cmd through the GLES2 path. `Finish ()`
  runs `glReadPixels` on the whole surface, merges with the software
  fallback FB, tears down EGL, returns the merged `Framebuffer`.
  (Lines 1912-1937.)
- `createPersistent (w, h) : PersistentGlBackend` (line 1955) — same
  shape but keeps EGL/shaders/FBO alive across frames. Renders into
  `mainFbo` (color *texture* + stencil renderbuffer, not just
  renderbuffers, so the color attachment is sampleable). Exposes
  `GetColorTexture`, `GetFlippedTexture` (Y-flipped for CUDA interop
  etc.), `Readback` (the per-frame readback), `Destroy`. Used by
  `composit/` for headless GBM/DRM output and CUDA video encode.

Underlying EGL bits live in `renderer/GlInterop.fs`:

- `init w h` uses `eglGetDisplay(EGL_DEFAULT_DISPLAY)`, requests
  `EGL_SURFACE_TYPE = EGL_PBUFFER_BIT`, allocates a pbuffer surface
  (line 67), makes it current.
- No code path constructs a *window* EGL surface today. No `wl_egl_window`,
  no `EGL_PLATFORM_X11_KHR`, no `EGL_PLATFORM_WAYLAND_KHR`.

Window backends (`widgets/platform/src/X11Window.fs`,
`WaylandWindow.fs`) don't reference `GlBackend` at all — they hold SHM
buffers, hand a software `Raster.create()` backend into them, and ship
the pixels via `XShmPutImage` / `wl_buffer.attach`.

So today: GL exists but only the composit project consumes it; widget
windows are entirely software.

## Shared compositing and Canvas backend

`Rbt.Renderer.Canvas` supplies the reusable Fable Canvas backend, glyph/image
caches and worker-safe canvas API. It references `Rbt.Renderer.Core`, not PDF
types. Image placement, premultiplied paint sampling, the standard blend modes
and isolated/non-isolated knockout groups belong here rather than in consumers.

Raster and GL stroke expansion preserve explicit `Close`: coincident endpoints
on an open path retain their caps; a closed path joins the seam. GL uses the
shared stroke outlines for solid and non-solid paints.

Canvas preserves positive fractional stroke widths under transforms; it does
not clamp them to a device pixel. Exactly zero width requests a device hairline.
An actual worker-rendered dense Form fixture and a reduced thin-stroke fixture
verify this distinction against the native output and reference geometry.
Raster and GL stroke expansion apply the same zero-width rule; native pixel
checks cover hairlines under shrinking transforms without promoting positive
fractional widths.

Nested transparency groups retain the parent's working backdrop, or its initial
backdrop for knockout. A nested group's own blend mode, mask and opacity apply
once. Isolated non-knockout groups render their children directly into the group
surface, avoiding per-object shape and source surfaces. Native regressions cover
isolated/non-isolated nested boundaries, internal blending, knockout and masks;
the original PDF transparency fixture also matches in the Chromium worker.

`ColorManagedGroup` adds explicit `ColorSpaceConversion.ToGroup`/`ToParent`
callbacks on normalized straight RGB components. Child paints already contain
the group's encoded components; ICC/PDF interpretation remains outside Renderer.
Raster, Canvas and GL convert the initial backdrop into group space, extract the
group-only contribution there, then convert that contribution into parent space
before boundary blend mode, opacity and mask. Untouched parent pixels do not
make a color-space round trip. Ordinary `TransparencyGroup` inherits its
parent's working space.

Packed source-over uses exact rounded division by 255 in both channel pairs;
the former division-by-256 approximation could reduce opaque alpha to 254.

Transparency groups on GL use CPU readback/compositing and upload, including
`ColorManagedGroup`; they are not a zero-readback direct-presentation path.
GL still explicitly rejects custom pixel filters, including consumer-defined
luminosity/transfer masks; use Raster or Canvas for those commands.
Knockout shape extraction is approximate for expanding
filters, text represented as masks and intrinsic image alpha.

Verification covers native raster pixels, Chromium Canvas and 11 live EGL cases
on Mesa llvmpipe (OpenGL ES 3.2): isolation/knockout combinations, opacity,
boundary Multiply, mask plus opacity, zero opacity, nested managed/unmanaged
groups, clipping and following-paint state restoration. GL pixels match the
independent expectations and raster probes within two channel values. This is
software Mesa execution, not hardware-GPU verification. No supported paint
currently reaches the deferred software-paint fallback, so that path was not
claimed as exercised.

## Why the current shape wastes work

For a live window, every frame does:

1. **GPU rasterize** the cmd list into the FBO. (Useful work.)
2. **`glReadPixels`** the full FBO to a CPU byte array. Hard sync point
   that flushes the GPU pipeline; PCIe transfer of ~8 MB at 1920×1080.
3. **Software merge** with the `softFb` fallback (per-pixel `blendPx`,
   line 1922-1931).
4. **Memcpy** the resulting `Framebuffer` into the SHM buffer in the
   platform window's render call.
5. **`XShmPutImage` / Wayland attach** uploads the SHM buffer to the
   compositor.

Three full-buffer copies per frame and a GPU sync. The GPU did its job
in step 1; everything after is overhead from the API contract that
`Finish` produces CPU pixels.

## The direct shape

**Render to a persistent GPU-side FBO. Present via a textured-quad pass
into the window's default framebuffer. Swap with damage rects.**

```
   Cmds + damageRects
            │
            ▼
   GLES2 rasterizer (glScissor = damageRects)
            │
            ▼
   Persistent FBO (color texture + stencil renderbuffer)  ← backing store
            │
            ▼
   Textured-quad pass → default framebuffer (EGL window surface)
            │
            ▼
   eglSwapBuffersWithDamageKHR(display, surface, damageRects)
```

The FBO is our owned backing store; pixels persist across frames because
we control the texture. Window-surface preservation
(`EGL_SWAP_BEHAVIOR = EGL_BUFFER_PRESERVED`) is unreliable (Mesa supports
it, some compositors don't) — FBO ownership sidesteps the question.

`glScissor` limits rasterization writes to the damaged rect. The
present pass is a single GPU draw call (full-screen textured quad
sampling the FBO color attachment) and is effectively free.

## Backend interface split

Today: `Backend = { Init; InitInto; Submit; Finish: unit -> Framebuffer }`.
Doesn't admit "present without producing CPU pixels."

**Recommended**: keep `Backend` as the pixel-producing contract; add
`WindowBackend` as a sibling.

```fsharp
// Existing — used by software Raster, GL offscreen, PDF, PNG export
type Backend = {
    Init      : int -> int -> unit
    InitInto  : Framebuffer -> unit
    Submit    : Cmd list -> unit
    Finish    : unit -> Framebuffer
}

// New — used by live windows
type WindowBackend = {
    Init     : int -> int -> unit
    Resize   : int -> int -> unit
    Submit   : Cmd list -> Rect list -> unit  // scissor = damage
    Present  : Rect list -> unit              // swap with damage
    Destroy  : unit -> unit
}
```

`Submit` taking damage rects keeps the contract symmetric across the
two implementations — software puts the scissor in `putPx`, GL puts it
in `glScissor`. `Present` is the new affordance: no return value, just
swap. Damage rects go to the platform via
`eglSwapBuffersWithDamageKHR` on the GL side, or per-rect
`XShmPutImage` / `wl_surface_damage_buffer` on the software side.

Both share the rasterization machinery; only termination differs.

## EGL window surface, by platform

### Wayland (clean target)

- Link `libwayland-egl.so` (`wayland-egl.h`) for `wl_egl_window_create(
  wl_surface, w, h)`.
- `eglGetPlatformDisplay(EGL_PLATFORM_WAYLAND_KHR, wl_display, NULL)`
  instead of `eglGetDisplay(EGL_DEFAULT_DISPLAY)`.
- `eglCreateWindowSurface(display, config, wl_egl_window, NULL)`
  instead of `eglCreatePbufferSurface`.
- Resize: `wl_egl_window_resize(window, w, h, 0, 0)` whenever the
  wl_surface size changes.
- Present: `eglSwapBuffersWithDamageKHR(display, surface, rects,
  nRects)`.
- `wl_surface_damage_buffer` opcode (`Protocol.Client.Generated.fs:828`)
  becomes unnecessary on this path — `eglSwapBuffersWithDamageKHR`
  emits the equivalent damage internally.

### X11 (uglier; lower priority)

Two paths:

- **EGL + X11** via `EGL_PLATFORM_X11_KHR`. Reuses existing
  `GlInterop.fs` bindings; needs visual matching against the X11
  window's visual. `eglSwapBuffersWithDamageEXT` works here too.
- **GLX** (legacy). Reliable, well-supported, but a separate binding
  surface from EGL and a different damage extension story
  (`GLX_EXT_buffer_age`).

Default to EGL+X11 since we already have the EGL plumbing. X11 visual
matching is the only real friction.

X11 GL is **deferred** — software path on X11 is fine for now. Land
Wayland first.

## Software fallback for un-GL'd cmds

`GlBackend` already has a `softFb` for cmds the GLES2 path doesn't
implement, merged at `Finish` (lines 1922-1931). The merge logic is
correct in concept, inverted in direction for window mode.

**For `WindowBackend`**: render the fallback cmds into a CPU FB →
upload as a GL texture → composite into the FBO with a textured quad.
Same idea, opposite side of the pipeline. The CPU fallback FB never
gets read back; it gets uploaded.

For the spreadsheet command vocabulary (`FillRect`, `StrokeRect`,
`GridLine`, `DrawGlyphs`, `Clip`) the GLES2 path already covers
everything. The fallback only matters for exotic cmds (`BackdropFilter`,
`ShaderFill` with quotation, custom blends).

## Scroll (the BitBlt equivalent)

There's no single primitive named "BitBlt" in GL — the accelerated
equivalent is **translation** (move existing pixels to a new
position) plus **clipping** (rasterize fresh content only in the
newly-exposed strip). Two GL primitives cover it:

- **`glBlitFramebuffer(src, dst, srcRect, dstRect, mask, filter)`** —
  rect-to-rect copy with optional scale/flip. Read FBO and draw FBO
  must differ (you can't blit a framebuffer onto itself with overlap
  — undefined). Used in a ping-pong arrangement.
- **Textured-quad pass** — bind FBO color texture, draw a quad with
  texcoords offset by the scroll delta. Same draw, different
  texcoords. Works against a sibling FBO; same overlap rule.

Either works; `glBlitFramebuffer` is more direct, the textured-quad
form gives you a fragment shader if we ever want filtered scroll
(unlikely).

### FBO sizing strategy

Two reasonable choices:

1. **Full-canvas FBO** sized to the entire scrollable content (e.g.
   5000×3000 for a real workbook). Scroll is genuinely free: the
   present-pass quad just samples a different region. Cost: memory
   scales with sheet size — 60 MB for the example, multi-GB for
   pathological sheets. Initial fill rasterizes the whole sheet.
2. **Viewport-sized FBO** (e.g. 1920×1080, ~8 MB). Bounded memory.
   Scroll requires the BitBlt-and-strip pattern below.

Default to **viewport-sized + ping-pong** for the spreadsheet — bounded
memory matters more than ideal scroll cost, and the strip-paint cost is
small (a column-width worth of cells on horizontal scroll, a row-height
worth on vertical).

### Scroll-frame path with viewport-sized FBOs

Two FBOs (`fboA`, `fboB`) ping-pong. `fboA` holds the current frame.

When the `ScrollView`'s `ScrollX`/`ScrollY` signal changes by `(dx, dy)`:

1. Bind `fboB` as draw target. Clear is unnecessary — the next steps
   overwrite every pixel.
2. **Translate**: textured-quad pass (or `glBlitFramebuffer`) sampling
   `fboA` with offset `(-dx, -dy)`. The overlap region of the old
   viewport now sits at its new position in `fboB`. Areas outside the
   overlap are still undefined.
3. **Clip + rasterize the newly-exposed strip** into `fboB`.
   `glScissor` to the strip rect (right strip for scroll-right, bottom
   strip for scroll-down, or both for diagonal). Run the cmd-list paint
   path, which produces cmds for the cells in that strip.
4. Swap `fboA` ↔ `fboB`. Present `fboA` (now holds the new frame) via
   the usual textured-quad present + `eglSwapBuffersWithDamageKHR`.

For the spreadsheet a scroll-only frame becomes: one `glBlitFramebuffer`
+ one strip rasterize (~one row/column's worth of cells). Independent
of scroll distance, viewport size, or sheet size.

### API surface (renderer side)

The renderer scroll path needs to be opt-in — the renderer can't
distinguish "this frame's damage is from a scroll" vs "this frame's
damage is from selection". One clean API:

```fsharp
type WindowBackend = {
    Init      : int -> int -> unit
    Resize    : int -> int -> unit
    Submit    : Cmd list -> Rect list -> unit   // scissor = damage
    Scroll    : viewport: Rect -> dx: int -> dy: int -> unit
                                                  // BitBlt within `viewport`,
                                                  // sets damage = newly-
                                                  // exposed strip(s)
    Present   : Rect list -> unit
    Destroy   : unit -> unit
}
```

`Scroll(viewport, dx, dy)` BitBlt's the existing pixels within the
viewport rect and sets internal damage to the newly-exposed strip(s).
The subsequent `Submit` rasterizes only those strips (already
scissored by the damage rects). `Present` swaps with the strip as the
damage list. Passing `viewport` rather than assuming "the whole
window" lets multiple scrollable regions coexist (chat history +
sidebar, multiple panes, etc.) — each region BitBlt's only its own
pixels.

### Widget-library wiring (reactive side)

The widget library needs an API that lets a `ScrollView` (or any
custom scrollable region) declare "I just scrolled by `(dx, dy)` within
this viewport rect". Mirrors `Damage.mark` from the damage-bounding
work:

```fsharp
// widgets/Reactive.fs — sibling of Damage.mark
module Damage =
    let mutable private pendingRects   : Rect list = []
    let mutable private pendingScrolls : (Rect * float * float) list = []

    let mark (r: Rect) = pendingRects <- r :: pendingRects
    let scroll (viewport: Rect) (dx: float) (dy: float) =
        pendingScrolls <- (viewport, dx, dy) :: pendingScrolls

    let drain () =
        let rs, ss = pendingRects, pendingScrolls
        pendingRects <- []; pendingScrolls <- []
        rs, ss
```

The frame loop in `x11-viewer/Program.fs` / `wayland-ime-test/Program.fs`
drains both lists. Scroll intents are dispatched through
`WindowBackend.Scroll` before the regular `Submit`; the strip rects
they produce union with the explicit damage rects from `pendingRects`.
Result: a scroll-only frame is one `glBlitFramebuffer` + one strip
rasterize; a scroll-during-cell-edit frame is the same plus the cell
rect added to the damage list.

### ScrollView's built-in integration

`ScrollView` already owns `ScrollX: Signal<float>` and `ScrollY:
Signal<float>` (`widgets/Widget.fs:172`). To produce scroll intents
without changing the widget's construction API, the *layout pass*
detects scroll-value changes between frames and calls `Damage.scroll`
itself.

Per-frame layout sees a fresh `ScrollView` widget value, so the
"previous scroll" state can't live on the widget. It lives in a
side-table keyed by signal identity:

```fsharp
// widgets/Layout.fs
let private scrollHistory =
    System.Collections.Generic.Dictionary<obj, float * float>()

// In the layout/paint pass for ScrollView nodes:
| ScrollView(_, _, sx, sy, _) ->
    let cx, cy = sx.Peek(), sy.Peek()
    let key = box sx  // Signal<T> is a reference type — stable identity
    let dx, dy =
        match scrollHistory.TryGetValue key with
        | true, (px, py) -> cx - px, cy - py
        | false, _ -> 0.0, 0.0
    scrollHistory.[key] <- (cx, cy)
    if dx <> 0.0 || dy <> 0.0 then
        Damage.scroll node.Bounds dx dy
    // ...existing paint emission
```

Caller code doesn't change. `scrollV child` / `scrollWith sH sV sx sy
child` gets accelerated scroll for free. Custom scrollable widgets
(virtualized list with its own scrollbar, etc.) can call
`Damage.scroll` directly and bypass `ScrollView`.

### Fallback rules at the renderer

`Damage.scroll` is a *hint*, not a contract. The renderer is free to
ignore it and fall back to full-rasterize when:

- **No `WindowBackend.Scroll` capability** (software backend that
  doesn't implement it yet, or a backend that targets a non-blittable
  target). Drop the hint; treat as full repaint within the viewport.
- **Scroll delta exceeds the viewport** in either axis. Overlap region
  is zero; nothing to blit. Treat as full repaint.
- **Subpixel scroll** (`|dx|, |dy| < 1`). Below blit's threshold. Pure
  rasterize.
- **Damage rects overlap the scroll viewport** beyond just the strips.
  Means something inside the scrolled region changed independently
  (e.g., a tooltip appeared while scrolling). Rbt.Renderer's choice: do
  the BitBlt then add the overlapping rects to the rasterize set, or
  punt to full repaint. Heuristic by area ratio.

In every fallback the system is still correct — just slower than the
fast path.

### Multiple ScrollViews on one frame

If two `ScrollView`s scroll in the same frame (e.g., synchronized
panes, or coincidental events), both call `Damage.scroll` with their
own viewport rects. The renderer applies each BitBlt independently in
viewport order. Each contributes its own strip(s) to the damage union.

### Software-backend parity

The same `Scroll` API works on the software backend:

- `Scroll(dx, dy)` does `memmove` within the SHM pixel buffer (row by
  row, account for negative `dy` so we copy in the right direction).
  Sets damage to the newly-exposed strip. `XShmPutImage` per damage
  rect at present-time.

The widget code is platform-agnostic. The backend abstracts away
"this is `glBlitFramebuffer`" vs "this is `memmove`".

## Where the patch lands

- **`renderer/GlInterop.fs`** — add Wayland-EGL bindings. New
  `initWindow` function that takes a `wl_egl_window` handle and returns
  an `EglContext` with a window surface instead of a pbuffer.
- **`renderer/GlBackend.fs`** — add `createWindow` constructor that
  takes the EGL context + initial size, builds the persistent FBO,
  returns a `WindowBackend`. Shares state types and dispatch with
  `createPersistent` — the diff from persistent is
  `Finish→Readback→merge` removed and `Present→textured-quad→
  SwapBuffersWithDamage` added.
- **`renderer/Cmd.fs`** — no change. Damage is out-of-band.
- **`widgets/platform/src/WaylandWindow.fs`** — new code path that
  constructs a `wl_egl_window` from the bound `wl_surface`, hands it to
  `GlBackend.createWindow`, and drives `Submit` / `Present` instead of
  the SHM path. The SHM path stays for fallback / debug.
- **`widgets/Rbt.Renderer.fs` or wherever the backend factory lives** —
  selection logic: prefer GL window when EGL is available, fall back to
  software SHM.

## Phasing

1. **Wayland-EGL bindings in `GlInterop.fs`.** Sanity check by
   constructing a window surface, clearing to a colour, swapping. No
   integration with the rest yet.
2. **`GlBackend.createWindow`.** New constructor; persistent FBO; no
   damage yet — full-frame `glScissor`, full-frame swap. Replaces
   `wayland-ime-test`'s SHM path; verify pixel-identical output.
3. **Damage piped to `glScissor` + `eglSwapBuffersWithDamageKHR`.**
   This is the payoff — depends on damage pipeline landing in the rest
   of the stack first.
4. **`Scroll(dx, dy)` API on `WindowBackend`.** Ping-pong FBOs; on
   scroll, `glBlitFramebuffer` from `fboA` to `fboB` with offset; emit
   damage = newly-exposed strip; subsequent `Submit` rasterizes only
   that strip. `ScrollView` widget wires its signal subscribers to
   `Scroll`. Mirror in the software backend with `memmove`.
5. **Software fallback via texture upload** (only if a cmd actually
   needs it for the spreadsheet — probably never).
6. **X11+EGL window backend.** Same shape, different platform glue.

## Composit compatibility

`composit/src/Program.fs:2820,2825` uses `GlBackend.createPersistent`
with EGL + GBM for DRM scanout. That path is unaffected — `createPersistent`
stays as-is. The new `createWindow` is an *additional* constructor; the
two coexist.

In fact `createWindow` is just `createPersistent` with the window-surface
EGL init substituted for the pbuffer init and `Present` substituted for
`Readback`. Roughly 90% of the code is shared. Worth factoring the
shared bits (state record, shader compile, FBO setup, dispatch loop)
out into a single module before adding the third constructor.

A future direction worth flagging: `composit` could itself take damage
rects from its clients (via `wl_surface_damage_buffer`) and pass them
through to its own GL output — same damage pipeline applies one layer
up. Out of scope for this doc but the surface area is friendly to it.

## Open questions

- **FBO resize**: on window resize, glTexImage2D to the new size and
  rescale-blit old contents? Or tear down + recreate? Recreate is
  simpler; resize is rare enough that the dropped frame doesn't
  matter.
- **Damage rect union**: `Rect list` (each a scissor / each a swap-with-
  damage entry) is simplest. EGL's swap-with-damage takes a flat
  `EGLint[]` array of `(x, y, w, h)` quads. No region/scanline
  structure needed.
- **`EGL_BUFFER_PRESERVED` as a fallback** for compositors that don't
  support `EGL_KHR_swap_buffers_with_damage`? Probably not worth it;
  the extension is universal on Mesa. If a system lacks it, fall back
  to full-frame swap, accept the perf hit.
- **Shared GL context** across multiple windows (e.g. modal dialog + main
  window): EGL contexts can share resources via `EGL_NO_CONTEXT`'s
  share-list parameter. Defer until we actually have multi-window.
