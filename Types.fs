namespace Rbt.Renderer

open System
#if !FABLE_COMPILER
open System.Buffers
open System.Runtime.InteropServices
#endif

#if !FABLE_COMPILER
#nowarn "3391" // implicit Memory -> ReadOnlyMemory conversion
#endif

[<AutoOpen>]
module Types =

    [<Measure>] type px

    [<Struct>]
    type Color = { R: byte; G: byte; B: byte; A: byte }

    module Color =
        let rgba r g b a = { R = r; G = g; B = b; A = a }
        let rgb r g b = rgba r g b 255uy
        let black  = rgb 0uy 0uy 0uy
        let white  = rgb 255uy 255uy 255uy
        let red    = rgb 255uy 0uy 0uy
        let green  = rgb 0uy 255uy 0uy
        let blue   = rgb 0uy 0uy 255uy
        let yellow = rgb 255uy 255uy 0uy
        let cyan   = rgb 0uy 255uy 255uy
        let toPremul (c: Color) =
            if c.A = 255uy then c
            elif c.A = 0uy then { R = 0uy; G = 0uy; B = 0uy; A = 0uy }
            else
                let a = float c.A / 255.0
                { R = byte (float c.R * a); G = byte (float c.G * a); B = byte (float c.B * a); A = c.A }
        let fromPremul (c: Color) =
            if c.A = 255uy then c
            elif c.A = 0uy then { R = 0uy; G = 0uy; B = 0uy; A = 0uy }
            else
                let a = float c.A / 255.0
                { R = byte (min 255.0 (float c.R / a)); G = byte (min 255.0 (float c.G / a)); B = byte (min 255.0 (float c.B / a)); A = c.A }

    type Vec2 = { X: float<px>; Y: float<px> }

    let vec2 x y = { X = x * 1.0<px>; Y = y * 1.0<px> }

    type Vec2 with
        static member (+) (a: Vec2, b: Vec2) = { X = a.X + b.X; Y = a.Y + b.Y }
        static member (-) (a: Vec2, b: Vec2) = { X = a.X - b.X; Y = a.Y - b.Y }
        static member (*) (a: Vec2, s: float) = { X = a.X * s; Y = a.Y * s }

    type Rect = { Pos: Vec2; Size: Vec2 }

    let rect x y w h = { Pos = vec2 x y; Size = vec2 w h }

    /// Packed BGRA premultiplied pixel.
    /// On little-endian: byte layout [B, G, R, A] — matches X11/Wayland ARGB8888.
    module Pixel =
        let inline pack (b: byte) (g: byte) (r: byte) (a: byte) : uint32 =
            uint32 b ||| (uint32 g <<< 8) ||| (uint32 r <<< 16) ||| (uint32 a <<< 24)
        let inline r (px: uint32) : byte = byte (px >>> 16)
        let inline g (px: uint32) : byte = byte (px >>> 8)
        let inline b (px: uint32) : byte = byte px
        let inline a (px: uint32) : byte = byte (px >>> 24)
        let inline toColor (px: uint32) : Color =
            { R = byte (px >>> 16); G = byte (px >>> 8); B = byte px; A = byte (px >>> 24) }
        let inline fromColor (c: Color) : uint32 =
            pack c.B c.G c.R c.A
        let inline fromColorPremul (c: Color) : uint32 =
            let p = Color.toPremul c
            pack p.B p.G p.R p.A
        let zero : uint32 = 0u
        /// Premultiplied alpha blend: out = src + dst * (1 - srcA/255)
        /// Uses SIMD-in-register: processes B+R and G+A as two 16-bit channel pairs.
        let inline blend (src: uint32) (dst: uint32) : uint32 =
            let sa = src >>> 24
            if sa = 255u then src
            elif sa = 0u then dst
            else
                let inv = 255u - sa
                let src_rb = src &&& 0x00FF00FFu
                let dst_rb = dst &&& 0x00FF00FFu
                let src_ag = (src >>> 8) &&& 0x00FF00FFu
                let dst_ag = (dst >>> 8) &&& 0x00FF00FFu
                let out_rb = (src_rb + ((dst_rb * inv + 0x00800080u) >>> 8)) &&& 0x00FF00FFu
                let out_ag = (src_ag + ((dst_ag * inv + 0x00800080u) >>> 8)) &&& 0x00FF00FFu
                out_rb ||| (out_ag <<< 8)

#if FABLE_COMPILER
    type Framebuffer = { W: int; H: int; Px: uint32[] }

    module Framebuffer =
        let create w h (c: Color) =
            let arr = Array.create (w * h) (Pixel.fromColorPremul c)
            { W = w; H = h; Px = arr }

        let ofArray (px: uint32[]) (w: int) (h: int) : Framebuffer =
            { W = w; H = h; Px = px }

        let tryGetArray (fb: Framebuffer) : uint32[] option =
            Some fb.Px
#else
    /// MemoryManager wrapping a native pointer as Memory<'T>.
    #nowarn "9"
    type UnmanagedMemoryManager<'T when 'T : unmanaged>(ptr: nativeint, length: int) =
        inherit MemoryManager<'T>()
        override _.GetSpan() =
            Span<'T>(Microsoft.FSharp.NativeInterop.NativePtr.toVoidPtr (Microsoft.FSharp.NativeInterop.NativePtr.ofNativeInt<'T> ptr), length)
        override _.Pin(elementIndex) =
            let bytePtr = ptr + nativeint (elementIndex * sizeof<'T>)
            new MemoryHandle(Microsoft.FSharp.NativeInterop.NativePtr.toVoidPtr (Microsoft.FSharp.NativeInterop.NativePtr.ofNativeInt<byte> bytePtr))
        override _.Unpin() = ()
        override _.Dispose(_disposing) = ()

    type Framebuffer = { W: int; H: int; Px: Memory<uint32> }

    module Framebuffer =
        let create w h (c: Color) =
            let arr = Array.create (w * h) (Pixel.fromColorPremul c)
            { W = w; H = h; Px = Memory(arr) }

        let fromNative (ptr: nativeint) (w: int) (h: int) : Framebuffer =
            let mgr = new UnmanagedMemoryManager<uint32>(ptr, w * h)
            { W = w; H = h; Px = mgr.Memory }

        let ofArray (px: uint32[]) (w: int) (h: int) : Framebuffer =
            { W = w; H = h; Px = Memory(px) }

        /// Get the underlying array (only for array-backed framebuffers)
        let tryGetArray (fb: Framebuffer) : uint32[] option =
            let mutable seg = ArraySegment<uint32>()
            let rom : ReadOnlyMemory<uint32> = fb.Px
            if MemoryMarshal.TryGetArray(rom, &seg) && seg.Offset = 0 then
                Some seg.Array
            else None
#endif

    let (|Inside|Outside|) (p: Vec2, r: Rect) =
        if p.X >= r.Pos.X && p.X < r.Pos.X + r.Size.X &&
           p.Y >= r.Pos.Y && p.Y < r.Pos.Y + r.Size.Y
        then Inside else Outside

    let (|InRadius|_|) (center: Vec2, radius: float<px>, p: Vec2) =
        let dx = p.X - center.X
        let dy = p.Y - center.Y
        if dx * dx + dy * dy <= radius * radius then Some InRadius else None

    [<Struct>]
    type Matrix3x2 =
        { M11: float; M12: float
          M21: float; M22: float
          DX: float; DY: float }

    module Matrix3x2 =
        let identity = { M11 = 1.0; M12 = 0.0; M21 = 0.0; M22 = 1.0; DX = 0.0; DY = 0.0 }

        let translate tx ty =
            { M11 = 1.0; M12 = 0.0; M21 = 0.0; M22 = 1.0; DX = tx; DY = ty }

        let rotate angle =
            let c = cos angle
            let s = sin angle
            { M11 = c; M12 = s; M21 = -s; M22 = c; DX = 0.0; DY = 0.0 }

        let scale sx sy =
            { M11 = sx; M12 = 0.0; M21 = 0.0; M22 = sy; DX = 0.0; DY = 0.0 }

        let multiply (a: Matrix3x2) (b: Matrix3x2) =
            { M11 = a.M11 * b.M11 + a.M12 * b.M21
              M12 = a.M11 * b.M12 + a.M12 * b.M22
              M21 = a.M21 * b.M11 + a.M22 * b.M21
              M22 = a.M21 * b.M12 + a.M22 * b.M22
              DX  = a.DX * b.M11 + a.DY * b.M21 + b.DX
              DY  = a.DX * b.M12 + a.DY * b.M22 + b.DY }

        let inverse (m: Matrix3x2) =
            let det = m.M11 * m.M22 - m.M12 * m.M21
            if abs det < 1e-10 then identity
            else
                let id = 1.0 / det
                { M11 = m.M22*id; M12 = -m.M12*id
                  M21 = -m.M21*id; M22 = m.M11*id
                  DX = (m.M21*m.DY - m.M22*m.DX)*id
                  DY = (m.M12*m.DX - m.M11*m.DY)*id }

        let transformVec2 (m: Matrix3x2) (v: Vec2) =
            let x = float v.X
            let y = float v.Y
            vec2 (m.M11 * x + m.M21 * y + m.DX) (m.M12 * x + m.M22 * y + m.DY)

    type SpreadMode = Pad | Reflect | Repeat

    type GradientStop = { Offset: float; StopColor: Color }

    type Paint =
        | NoPaint
        | Solid of Color
        | LinearGradient of p0: Vec2 * p1: Vec2 * stops: GradientStop list * spread: SpreadMode
        | RadialGradient of center: Vec2 * radius: float<px> * stops: GradientStop list * spread: SpreadMode
        | ConicGradient of center: Vec2 * angle: float * stops: GradientStop list * spread: SpreadMode
        | FocalRadialGradient of center: Vec2 * radius: float<px> * focus: Vec2 * stops: GradientStop list * spread: SpreadMode
        | TilingPaint of source: Framebuffer * xform: Matrix3x2 * tileX: SpreadMode * tileY: SpreadMode
        | ImagePaint of source: Framebuffer * xform: Matrix3x2

    type LineCap = Butt | Round | Square

    type LineJoin = Miter | Bevel | RoundJoin

    type StrokeStyle =
        { Width: float<px>; Cap: LineCap; Join: LineJoin; Paint: Paint
          DashArray: float<px> list; DashOffset: float<px>; MiterLimit: float }

    type PathSeg =
        | MoveTo of Vec2
        | LineTo of Vec2
        | QuadTo of cp: Vec2 * pt: Vec2
        | CubicTo of cp1: Vec2 * cp2: Vec2 * pt: Vec2
        | ArcTo of rx: float<px> * ry: float<px> * rotation: float * largeArc: bool * sweep: bool * pt: Vec2
        | Close

    type BoolOp = Union | Intersect | BoolDifference | BoolXor

    type FillRule = EvenOdd | NonZero

    type BlendMode =
        | Normal | Multiply | Screen | Overlay
        | SrcIn | SrcOut | DstOver | DstAtop | Xor
        | Darken | Lighten | ColorDodge | ColorBurn
        | SoftLight | Difference | Exclusion
        | Hue | Saturation | ColorMode | Luminosity

    /// A direct pixel pass over a w×h premultiplied-BGRA framebuffer: it receives the pixel
    /// array (after the wrapped content has been rendered offscreen) and returns the processed
    /// pixels. This is the renderer's open, registry-free filter extension point — a host (e.g.
    /// an image editor) supplies the implementation as a plain function rather than a named
    /// table entry. The built-in CPU filters below have exactly this shape internally.
    type PixelPass = int -> int -> uint32[] -> uint32[]

    type Filter =
        | Blur of radius: int
        | DropShadow of dx: float * dy: float * blurRadius: int * color: Color
        | Brightness of float
        | Contrast of float
        | Saturate of float
        | Grayscale of float
        | Sepia of float
        | Invert of float
        | HueRotate of float
        | OpacityFilter of float
        /// A host-supplied pixel pass (no name lookup). Carried through `Filtered` /
        /// `BackdropFilter` like any other filter. CPU/software backend; the GL backend may
        /// fall back to software for this case.
        | Custom of PixelPass

    type BorderStyle = NoBorder | BorderSolid | BorderDashed | BorderDotted | BorderDouble

    type BorderSide =
        { Width: float<px>; Color: Color; Style: BorderStyle }

    module BorderSide =
        let none = { Width = 0.0<px>; Color = Color.black; Style = NoBorder }
        let solid w c = { Width = w * 1.0<px>; Color = c; Style = BorderSolid }
        let dashed w c = { Width = w * 1.0<px>; Color = c; Style = BorderDashed }
        let dotted w c = { Width = w * 1.0<px>; Color = c; Style = BorderDotted }
        let double' w c = { Width = w * 1.0<px>; Color = c; Style = BorderDouble }

    [<Struct>]
    type Matrix4x4 =
        { M11: float; M12: float; M13: float; M14: float
          M21: float; M22: float; M23: float; M24: float
          M31: float; M32: float; M33: float; M34: float
          M41: float; M42: float; M43: float; M44: float }

    module Matrix4x4 =
        let identity =
            { M11 = 1.0; M12 = 0.0; M13 = 0.0; M14 = 0.0
              M21 = 0.0; M22 = 1.0; M23 = 0.0; M24 = 0.0
              M31 = 0.0; M32 = 0.0; M33 = 1.0; M34 = 0.0
              M41 = 0.0; M42 = 0.0; M43 = 0.0; M44 = 1.0 }

        let perspective (d: float) =
            // CSS perspective: M34 = -1/d, objects at z=0 are unaffected
            { identity with M34 = -1.0 / d }

        let translate (tx: float) (ty: float) (tz: float) =
            { identity with M41 = tx; M42 = ty; M43 = tz }

        let scale (sx: float) (sy: float) (sz: float) =
            { identity with M11 = sx; M22 = sy; M33 = sz }

        let rotateX (angle: float) =
            let c = cos angle
            let s = sin angle
            { identity with M22 = c; M23 = s; M32 = -s; M33 = c }

        let rotateY (angle: float) =
            let c = cos angle
            let s = sin angle
            { identity with M11 = c; M13 = -s; M31 = s; M33 = c }

        let rotateZ (angle: float) =
            let c = cos angle
            let s = sin angle
            { identity with M11 = c; M12 = s; M21 = -s; M22 = c }

        let multiply (a: Matrix4x4) (b: Matrix4x4) =
            { M11 = a.M11*b.M11 + a.M12*b.M21 + a.M13*b.M31 + a.M14*b.M41
              M12 = a.M11*b.M12 + a.M12*b.M22 + a.M13*b.M32 + a.M14*b.M42
              M13 = a.M11*b.M13 + a.M12*b.M23 + a.M13*b.M33 + a.M14*b.M43
              M14 = a.M11*b.M14 + a.M12*b.M24 + a.M13*b.M34 + a.M14*b.M44
              M21 = a.M21*b.M11 + a.M22*b.M21 + a.M23*b.M31 + a.M24*b.M41
              M22 = a.M21*b.M12 + a.M22*b.M22 + a.M23*b.M32 + a.M24*b.M42
              M23 = a.M21*b.M13 + a.M22*b.M23 + a.M23*b.M33 + a.M24*b.M43
              M24 = a.M21*b.M14 + a.M22*b.M24 + a.M23*b.M34 + a.M24*b.M44
              M31 = a.M31*b.M11 + a.M32*b.M21 + a.M33*b.M31 + a.M34*b.M41
              M32 = a.M31*b.M12 + a.M32*b.M22 + a.M33*b.M32 + a.M34*b.M42
              M33 = a.M31*b.M13 + a.M32*b.M23 + a.M33*b.M33 + a.M34*b.M43
              M34 = a.M31*b.M14 + a.M32*b.M24 + a.M33*b.M34 + a.M34*b.M44
              M41 = a.M41*b.M11 + a.M42*b.M21 + a.M43*b.M31 + a.M44*b.M41
              M42 = a.M41*b.M12 + a.M42*b.M22 + a.M43*b.M32 + a.M44*b.M42
              M43 = a.M41*b.M13 + a.M42*b.M23 + a.M43*b.M33 + a.M44*b.M43
              M44 = a.M41*b.M14 + a.M42*b.M24 + a.M43*b.M34 + a.M44*b.M44 }

        /// Transform a 2D point (z=0) through the 4x4 matrix with perspective division
        let transformPoint (m: Matrix4x4) (x: float) (y: float) =
            let x' = m.M11*x + m.M21*y + m.M41
            let y' = m.M12*x + m.M22*y + m.M42
            let w' = m.M14*x + m.M24*y + m.M44
            if abs w' < 1e-10 then (x', y')
            else (x' / w', y' / w')

        /// Convert to column-major float32 array for GL upload
        let toFloat32Array (m: Matrix4x4) : float32[] =
            [| float32 m.M11; float32 m.M12; float32 m.M13; float32 m.M14
               float32 m.M21; float32 m.M22; float32 m.M23; float32 m.M24
               float32 m.M31; float32 m.M32; float32 m.M33; float32 m.M34
               float32 m.M41; float32 m.M42; float32 m.M43; float32 m.M44 |]

        /// Lift a 2D affine Matrix3x2 into a 4x4 matrix
        let fromMatrix3x2 (m: Matrix3x2) =
            { M11 = m.M11; M12 = m.M12; M13 = 0.0; M14 = 0.0
              M21 = m.M21; M22 = m.M22; M23 = 0.0; M24 = 0.0
              M31 = 0.0;   M32 = 0.0;   M33 = 1.0; M34 = 0.0
              M41 = m.DX;  M42 = m.DY;  M43 = 0.0; M44 = 1.0 }
