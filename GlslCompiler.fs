namespace Rbt.Renderer

open System
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

// GPU types for quotation-based shader generation
module Gpu =
    [<Struct>]
    type vec2 = { x: float32; y: float32 }

    [<Struct>]
    type vec3 = { x: float32; y: float32; z: float32 }

    [<Struct>]
    type vec4 = { x: float32; y: float32; z: float32; w: float32 }

    let mkVec2 x y : vec2 = { x = x; y = y }
    let mkVec3 x y z : vec3 = { x = x; y = y; z = z }
    let mkVec3s (s: float32) : vec3 = { x = s; y = s; z = s }
    let mkVec4 x y z w : vec4 = { x = x; y = y; z = z; w = w }

    type vec2 with
        static member (+) (a: vec2, b: vec2) = { x = a.x + b.x; y = a.y + b.y }
        static member (-) (a: vec2, b: vec2) = { x = a.x - b.x; y = a.y - b.y }
        static member (*) (a: vec2, s: float32) = { x = a.x * s; y = a.y * s }

    type vec3 with
        static member (+) (a: vec3, b: vec3) = { vec3.x = a.x + b.x; y = a.y + b.y; z = a.z + b.z }
        static member (-) (a: vec3, b: vec3) = { vec3.x = a.x - b.x; y = a.y - b.y; z = a.z - b.z }
        static member (*) (a: vec3, s: float32) = { vec3.x = a.x * s; y = a.y * s; z = a.z * s }

    type vec4 with
        static member (+) (a: vec4, b: vec4) = { x = a.x + b.x; y = a.y + b.y; z = a.z + b.z; w = a.w + b.w }
        static member (-) (a: vec4, b: vec4) = { x = a.x - b.x; y = a.y - b.y; z = a.z - b.z; w = a.w - b.w }
        static member (*) (a: vec4, s: float32) = { x = a.x * s; y = a.y * s; z = a.z * s; w = a.w * s }

    module GpuMath =
        let dot (a: vec2) (b: vec2) : float32 = a.x * b.x + a.y * b.y
        let length (a: vec2) : float32 = sqrt (a.x * a.x + a.y * a.y)
        let mix (a: float32) (b: float32) (t: float32) : float32 = a + (b - a) * t
        let mixV4 (a: vec4) (b: vec4) (t: float32) : vec4 = { x = a.x + (b.x - a.x) * t; y = a.y + (b.y - a.y) * t; z = a.z + (b.z - a.z) * t; w = a.w + (b.w - a.w) * t }
        let clamp (v: float32) (lo: float32) (hi: float32) : float32 = max lo (min hi v)
        let atan2' (y: float32) (x: float32) : float32 = atan2 y x
        let floor' (v: float32) : float32 = floor v
        let sqrt' (v: float32) : float32 = sqrt v
        let abs' (v: float32) : float32 = abs v
        let step (edge: float32) (v: float32) : float32 = if v < edge then 0.0f else 1.0f
        let fract (v: float32) : float32 = v - floor v
        let sin' (v: float32) : float32 = sin v
        let cos' (v: float32) : float32 = cos v
        let mod' (a: float32) (b: float32) : float32 = a - b * floor (a / b)
        let smoothstep (e0: float32) (e1: float32) (v: float32) : float32 =
            let t = max 0.0f (min 1.0f ((v - e0) / (e1 - e0))) in t * t * (3.0f - 2.0f * t)
        let pow' (a: float32) (b: float32) : float32 = a ** b
        let texture2D (sampler: int) (uv: vec2) : vec4 = mkVec4 0.0f 0.0f 0.0f 0.0f
        let min' (a: float32) (b: float32) : float32 = min a b
        let max' (a: float32) (b: float32) : float32 = max a b
        let callGlsl (name: string) (a: float32) (b: float32) (c: float32) (d: float32) (e: float32) (f: float32) : vec4 = mkVec4 0.0f 0.0f 0.0f 0.0f
        let uniformFloat (name: string) : float32 = 0.0f
        let uniformVec2 (name: string) : vec2 = mkVec2 0.0f 0.0f
        let mat4MulVec4 (matName: string) (v: vec4) : vec4 = v
        let texture2DName (name: string) (uv: vec2) : vec4 = mkVec4 0.0f 0.0f 0.0f 0.0f

module GlslCompiler =

    open Gpu

    // Quotation walker: convert F# expression tree to GLSL string
    let rec exprToGlsl (e: Expr) : string =
        match e with
        | Value (v, t) when t = typeof<float32> ->
            let f = v :?> float32
            sprintf "%.6f" f
        | Value (v, t) when t = typeof<int> ->
            sprintf "%d" (v :?> int)

        | Var v -> v.Name

        | PropertyGet (Some inst, pi, []) when pi.Name = "x" || pi.Name = "y" || pi.Name = "z" || pi.Name = "w" ->
            sprintf "%s.%s" (exprToGlsl inst) pi.Name

        | NewRecord (t, args) when t = typeof<vec2> ->
            sprintf "vec2(%s, %s)" (exprToGlsl args.[0]) (exprToGlsl args.[1])
        | NewRecord (t, args) when t = typeof<vec3> ->
            sprintf "vec3(%s, %s, %s)" (exprToGlsl args.[0]) (exprToGlsl args.[1]) (exprToGlsl args.[2])
        | NewRecord (t, args) when t = typeof<vec4> ->
            sprintf "vec4(%s, %s, %s, %s)" (exprToGlsl args.[0]) (exprToGlsl args.[1]) (exprToGlsl args.[2]) (exprToGlsl args.[3])

        // mkVec2/mkVec3/mkVec4 constructor calls
        | Call (None, mi, [a; b]) when mi.Name = "mkVec2" ->
            sprintf "vec2(%s, %s)" (exprToGlsl a) (exprToGlsl b)
        | Call (None, mi, [a; b; c]) when mi.Name = "mkVec3" ->
            sprintf "vec3(%s, %s, %s)" (exprToGlsl a) (exprToGlsl b) (exprToGlsl c)
        | Call (None, mi, [a]) when mi.Name = "mkVec3s" ->
            sprintf "vec3(%s)" (exprToGlsl a)
        | Call (None, mi, [a; b; c; d]) when mi.Name = "mkVec4" ->
            sprintf "vec4(%s, %s, %s, %s)" (exprToGlsl a) (exprToGlsl b) (exprToGlsl c) (exprToGlsl d)

        // Arithmetic on GPU types (op_Addition, op_Subtraction, op_Multiply)
        | Call (None, mi, [a; b]) when mi.Name = "op_Addition" ->
            sprintf "(%s + %s)" (exprToGlsl a) (exprToGlsl b)
        | Call (None, mi, [a; b]) when mi.Name = "op_Subtraction" ->
            sprintf "(%s - %s)" (exprToGlsl a) (exprToGlsl b)
        | Call (None, mi, [a; b]) when mi.Name = "op_Multiply" ->
            sprintf "(%s * %s)" (exprToGlsl a) (exprToGlsl b)
        | Call (None, mi, [a; b]) when mi.Name = "op_Division" ->
            sprintf "(%s / %s)" (exprToGlsl a) (exprToGlsl b)

        // Comparison operators
        | Call (None, mi, [a; b]) when mi.Name = "op_LessThan" ->
            sprintf "(%s < %s)" (exprToGlsl a) (exprToGlsl b)
        | Call (None, mi, [a; b]) when mi.Name = "op_GreaterThan" ->
            sprintf "(%s > %s)" (exprToGlsl a) (exprToGlsl b)
        | Call (None, mi, [a; b]) when mi.Name = "op_LessThanOrEqual" ->
            sprintf "(%s <= %s)" (exprToGlsl a) (exprToGlsl b)
        | Call (None, mi, [a; b]) when mi.Name = "op_GreaterThanOrEqual" ->
            sprintf "(%s >= %s)" (exprToGlsl a) (exprToGlsl b)
        | Call (None, mi, [a; b]) when mi.Name = "op_Equality" || mi.Name = "GenericEquality" || mi.Name = "GenericEqualityIntrinsic" ->
            sprintf "(%s == %s)" (exprToGlsl a) (exprToGlsl b)
        | Call (None, mi, [a; b]) when mi.Name = "op_Inequality" || mi.Name = "GenericInequality" || mi.Name = "GenericInequalityIntrinsic" ->
            sprintf "(%s != %s)" (exprToGlsl a) (exprToGlsl b)

        // GpuMath calls → GLSL built-ins
        | Call (None, mi, args) when mi.DeclaringType <> null && mi.DeclaringType.Name = "GpuMath" ->
            match mi.Name with
            | "texture2D" ->
                // First arg is sampler index (int literal), second is UV vec2
                let samplerIdx = match args.[0] with Value (v, _) -> v :?> int | _ -> 0
                let uvStr = exprToGlsl args.[1]
                sprintf "texture2D(uTex%d, %s)" samplerIdx uvStr
            | "callGlsl" ->
                // callGlsl "funcName" a b c d e f → funcName(a, b, c, d, e, f)
                let funcName = match args.[0] with Value (v, _) -> v :?> string | _ -> "unknown"
                let argStrs = args.[1..] |> List.map exprToGlsl |> String.concat ", "
                sprintf "%s(%s)" funcName argStrs
            | "uniformFloat" | "uniformVec2" ->
                // uniformFloat "name" → name (raw GLSL uniform reference)
                match args.[0] with Value (v, _) -> v :?> string | _ -> "unknown"
            | "mat4MulVec4" ->
                // mat4MulVec4 "matName" v → (matName * v)
                let matName = match args.[0] with Value (v, _) -> v :?> string | _ -> "unknown"
                sprintf "(%s * %s)" matName (exprToGlsl args.[1])
            | "texture2DName" ->
                // texture2DName "samplerName" uv → texture2D(samplerName, uv)
                let samplerName = match args.[0] with Value (v, _) -> v :?> string | _ -> "uTex"
                sprintf "texture2D(%s, %s)" samplerName (exprToGlsl args.[1])
            | _ ->
                let glslName =
                    match mi.Name with
                    | "dot" -> "dot"
                    | "length" -> "length"
                    | "mix" | "mixV4" -> "mix"
                    | "clamp" -> "clamp"
                    | "atan2'" -> "atan"
                    | "floor'" -> "floor"
                    | "sqrt'" -> "sqrt"
                    | "abs'" -> "abs"
                    | "step" -> "step"
                    | "fract" -> "fract"
                    | "min'" -> "min"
                    | "max'" -> "max"
                    | "sin'" -> "sin"
                    | "cos'" -> "cos"
                    | "mod'" -> "mod"
                    | "smoothstep" -> "smoothstep"
                    | "pow'" -> "pow"
                    | other -> failwith $"Unknown GpuMath function: {other}"
                let argStrs = args |> List.map exprToGlsl |> String.concat ", "
                sprintf "%s(%s)" glslName argStrs

        // int-to-float conversion (float32 i) → float(i)
        | Call (None, mi, [a]) when mi.Name = "ToSingle" ->
            sprintf "float(%s)" (exprToGlsl a)

        // Unary negation
        | Call (None, mi, [a]) when mi.Name = "op_UnaryNegation" ->
            sprintf "(-%s)" (exprToGlsl a)

        // If-then-else → ternary
        | IfThenElse (cond, thenE, elseE) ->
            sprintf "((%s) ? (%s) : (%s))" (exprToGlsl cond) (exprToGlsl thenE) (exprToGlsl elseE)

        // Curried function application: f a b → f(a, b)
        | Application _ ->
            let rec collectApp e args =
                match e with
                | Application (func, arg) -> collectApp func (arg :: args)
                | _ -> (e, args)
            let (func, args) = collectApp e []
            let funcStr = exprToGlsl func
            let argStrs = args |> List.map exprToGlsl |> String.concat ", "
            sprintf "%s(%s)" funcStr argStrs

        // Let binding — handled via emitStatements in the shader assembly
        | Let _ -> failwith "Let bindings should be handled by emitStatements"

        | _ -> failwith $"Unsupported quotation node: {e}"

    // Emit statements from let-binding chains, returning (statements list, final expression string)
    let rec emitStatements (e: Expr) : string list * string =
        match e with
        | Let (v, rhs, body) ->
            let rhsStr = exprToGlsl rhs
            let glslType = mapType rhs.Type
            let stmt = sprintf "    %s %s = %s;" glslType v.Name rhsStr
            let (moreStmts, result) = emitStatements body
            (stmt :: moreStmts, result)
        | Sequential (a, b) ->
            let (stmtsA, _) = emitStatements a
            let (stmtsB, result) = emitStatements b
            (stmtsA @ stmtsB, result)
        | VarSet (v, value) ->
            let stmt = sprintf "    %s = %s;" v.Name (exprToGlsl value)
            ([stmt], "()")
        | ForIntegerRangeLoop (v, lo, hi, body) ->
            let loStr = exprToGlsl lo
            let hiStr = exprToGlsl hi
            let (bodyStmts, _) = emitStatements body
            let bodyBlock = bodyStmts |> String.concat "\n"
            let stmt = sprintf "    for (int %s = %s; %s <= %s; %s++) {\n%s\n    }" v.Name loStr v.Name hiStr v.Name bodyBlock
            ([stmt], "()")
        | Value (_, t) when t = typeof<unit> ->
            ([], "()")
        | _ ->
            ([], exprToGlsl e)

    and mapType (t: Type) : string =
        if t = typeof<float32> then "float"
        elif t = typeof<vec2> then "vec2"
        elif t = typeof<vec3> then "vec3"
        elif t = typeof<vec4> then "vec4"
        elif t = typeof<int> then "int"
        elif t = typeof<bool> then "bool"
        else "float" // default

    // Peel nested Lambda chain to extract parameter list + inner body
    let rec peelLambdas (e: Expr) : Var list * Expr =
        match e with
        | Lambda (v, body) ->
            let (moreParams, innerBody) = peelLambdas body
            (v :: moreParams, innerBody)
        | _ -> ([], e)

    // Extract top-level function definitions from let-bindings where RHS is a Lambda.
    // Returns (GLSL function definition strings, remaining body expression).
    let rec extractFuncDefs (e: Expr) : string list * Expr =
        match e with
        | Let (v, (Lambda _ as funcExpr), body) ->
            let (parms, funcBody) = peelLambdas funcExpr
            let retType = mapType funcBody.Type
            let paramDecls = parms |> List.map (fun p -> sprintf "%s %s" (mapType p.Type) p.Name) |> String.concat ", "
            let (stmts, resultExpr) = emitStatements funcBody
            let stmtBlock = stmts |> String.concat "\n"
            let funcDef =
                sprintf "%s %s(%s) {\n%s\n    return %s;\n}" retType v.Name paramDecls
                    (if stmtBlock = "" then "" else stmtBlock)
                    resultExpr
            let (moreDefs, remainingBody) = extractFuncDefs body
            (funcDef :: moreDefs, remainingBody)
        | LetRecursive (bindings, body) ->
            let funcDefs =
                bindings |> List.map (fun (v, funcExpr) ->
                    let (parms, funcBody) = peelLambdas funcExpr
                    let retType = mapType funcBody.Type
                    let paramDecls = parms |> List.map (fun p -> sprintf "%s %s" (mapType p.Type) p.Name) |> String.concat ", "
                    let (stmts, resultExpr) = emitStatements funcBody
                    let stmtBlock = stmts |> String.concat "\n"
                    sprintf "%s %s(%s) {\n%s\n    return %s;\n}" retType v.Name paramDecls
                        (if stmtBlock = "" then "" else stmtBlock)
                        resultExpr)
            let (moreDefs, remainingBody) = extractFuncDefs body
            (funcDefs @ moreDefs, remainingBody)
        | _ -> ([], e)

    // Extract fragment shader: peel Lambda chain. First param = vPos varying, rest would be uniforms.
    let extractFragBody (expr: Expr) : Var * Expr =
        match expr with
        | Lambda (vPos, body) -> (vPos, body)
        | _ -> failwith "Expected a lambda expression for fragment shader"

    // Gradient vertex shader: passes vPos = aPos as varying
    let gradientVertSrc = """
attribute vec2 aPos;
uniform mat4 uMVP;
varying vec2 vPos;
void main() {
    vPos = aPos;
    gl_Position = uMVP * vec4(aPos, 0.0, 1.0);
}
"""

    // Compile a fragment shader from a quotation that takes vec2 vPos and returns vec4 color
    let compileFragmentShader (quotation: Expr) (stopSamplingGlsl: string) : string =
        let (_vPos, body) = extractFragBody quotation
        let (stmts, tExpr) = emitStatements body
        let stmtBlock = stmts |> String.concat "\n"
        "precision highp float;\nvarying vec2 vPos;\nvoid main() {\n"
        + stmtBlock + "\n"
        + "    float tRaw = " + tExpr + ";\n"
        + stopSamplingGlsl
        + "}\n"

    type CompiledShader =
        { Program: int
          LocPos: int
          LocMVP: int }

    let private shaderCache = Dictionary<string, CompiledShader>()

    let compileAndCache (fragSrc: string) : CompiledShader =
        match shaderCache.TryGetValue(fragSrc) with
        | true, cached -> cached
        | _ ->
            eprintfn "--- Compiled gradient GLSL ---\n%s\n%s\n---" gradientVertSrc fragSrc
            let vs = Gl.compileShader Gl.GL_VERTEX_SHADER gradientVertSrc
            let fs = Gl.compileShader Gl.GL_FRAGMENT_SHADER fragSrc
            let prog = Gl.linkProgram vs fs
            let locPos = Gl.glGetAttribLocation(prog, "aPos")
            let locMVP = Gl.glGetUniformLocation(prog, "uMVP")
            let result = { Program = prog; LocPos = locPos; LocMVP = locMVP }
            shaderCache.[fragSrc] <- result
            result

    // --- Gradient stop sampling: direct GLSL generation ---

    let emitStopSampling (stops: GradientStop list) (spreadGlsl: string) : string =
        // spreadGlsl transforms tRaw → t with the spread mode applied
        let sortedStops = stops |> List.sortBy (fun s -> s.Offset)
        let stops = if sortedStops.IsEmpty then [ { Offset = 0.0; StopColor = Color.black }; { Offset = 1.0; StopColor = Color.white } ] else sortedStops

        let colorVec (c: Color) =
            let pm = Color.toPremul c
            sprintf "vec4(%.6f, %.6f, %.6f, %.6f)"
                (float32 pm.R / 255.0f) (float32 pm.G / 255.0f) (float32 pm.B / 255.0f) (float32 pm.A / 255.0f)

        let sb = System.Text.StringBuilder()
        sb.AppendLine(spreadGlsl) |> ignore

        // Default to last stop color
        let lastStop = stops |> List.last
        sb.AppendLine(sprintf "    vec4 color = %s;" (colorVec lastStop.StopColor)) |> ignore

        // Generate if-else chain for stop intervals
        let pairs = stops |> List.pairwise
        for i, (s0, s1) in pairs |> List.indexed do
            let keyword = if i = 0 then "if" else "} else if"
            sb.AppendLine(sprintf "    %s (t <= %.6f) {" keyword (float32 s1.Offset)) |> ignore
            let range = s1.Offset - s0.Offset
            if range > 1e-8 then
                sb.AppendLine(sprintf "        float f = clamp((t - %.6f) / %.6f, 0.0, 1.0);" (float32 s0.Offset) (float32 range)) |> ignore
                sb.AppendLine(sprintf "        color = mix(%s, %s, f);" (colorVec s0.StopColor) (colorVec s1.StopColor)) |> ignore
            else
                sb.AppendLine(sprintf "        color = %s;" (colorVec s1.StopColor)) |> ignore

        if not pairs.IsEmpty then
            sb.AppendLine("    }") |> ignore

        sb.AppendLine("    gl_FragColor = color;") |> ignore
        sb.ToString()

    let spreadGlsl (spread: SpreadMode) : string =
        match spread with
        | Pad     -> "    float t = clamp(tRaw, 0.0, 1.0);"
        | Repeat  -> "    float t = fract(tRaw);"
        | Reflect ->
            "    float t2 = tRaw - 2.0 * floor(tRaw / 2.0);\n    float t = (t2 > 1.0) ? (2.0 - t2) : t2;"

    // --- Gradient builders ---

    let buildLinearGradientFrag (p0: Vec2) (p1: Vec2) (stops: GradientStop list) (spread: SpreadMode) : string =
        let ox = float32 (float p0.X)
        let oy = float32 (float p0.Y)
        let dx = float32 (float p1.X - float p0.X)
        let dy = float32 (float p1.Y - float p0.Y)
        let len2 = dx * dx + dy * dy

        let quotation =
            <@ fun (vPos: vec2) ->
                let px = vPos.x - ox
                let py = vPos.y - oy
                (px * dx + py * dy) / len2 @>

        let stopGlsl = emitStopSampling stops (spreadGlsl spread)
        compileFragmentShader quotation stopGlsl

    let buildRadialGradientFrag (center: Vec2) (radius: float<px>) (stops: GradientStop list) (spread: SpreadMode) : string =
        let cx = float32 (float center.X)
        let cy = float32 (float center.Y)
        let r = float32 (float radius)

        let quotation =
            <@ fun (vPos: vec2) ->
                let dx = vPos.x - cx
                let dy = vPos.y - cy
                GpuMath.sqrt' (dx * dx + dy * dy) / r @>

        let stopGlsl = emitStopSampling stops (spreadGlsl spread)
        compileFragmentShader quotation stopGlsl

    let buildConicGradientFrag (center: Vec2) (angle: float) (stops: GradientStop list) (spread: SpreadMode) : string =
        let cx = float32 (float center.X)
        let cy = float32 (float center.Y)
        let angleOffset = float32 angle

        let quotation =
            <@ fun (vPos: vec2) ->
                let dx = vPos.x - cx
                let dy = vPos.y - cy
                let a = GpuMath.atan2' dy dx
                // Normalize from [-pi, pi] to [0, 1], applying angle offset
                let normalized = (a - angleOffset) / 6.283185f
                normalized - GpuMath.floor' normalized @>

        let stopGlsl = emitStopSampling stops (spreadGlsl spread)
        compileFragmentShader quotation stopGlsl

    let buildFocalRadialGradientFrag (center: Vec2) (radius: float<px>) (focus: Vec2) (stops: GradientStop list) (spread: SpreadMode) : string =
        let cx = float32 (float center.X)
        let cy = float32 (float center.Y)
        let r = float32 (float radius)
        let focX = float32 (float focus.X)
        let focY = float32 (float focus.Y)
        let ffx = focX - cx
        let ffy = focY - cy

        let quotation =
            <@ fun (vPos: vec2) ->
                let dx = vPos.x - focX
                let dy = vPos.y - focY
                let a = dx * dx + dy * dy
                let b = 2.0f * (dx * ffx + dy * ffy)
                let c = ffx * ffx + ffy * ffy - r * r
                let disc = b * b - 4.0f * a * c
                (if disc < 0.0f || a < 0.0000001f then
                    GpuMath.sqrt' (dx * dx + dy * dy) / r
                 else
                    let sq = GpuMath.sqrt' disc
                    let t1 = (-b + sq) / (2.0f * a)
                    let t2 = (-b - sq) / (2.0f * a)
                    let t = (if t1 > 0.0f then t1 else t2)
                    (if t > 0.0f then 1.0f / t else 0.0f)) @>

        let stopGlsl = emitStopSampling stops (spreadGlsl spread)
        compileFragmentShader quotation stopGlsl

    // Build a compiled shader for a given gradient paint
    let buildGradientShader (paint: Paint) : CompiledShader option =
        match paint with
        | LinearGradient (p0, p1, stops, spread) ->
            let fragSrc = buildLinearGradientFrag p0 p1 stops spread
            Some (compileAndCache fragSrc)
        | RadialGradient (center, radius, stops, spread) ->
            let fragSrc = buildRadialGradientFrag center radius stops spread
            Some (compileAndCache fragSrc)
        | ConicGradient (center, angle, stops, spread) ->
            let fragSrc = buildConicGradientFrag center angle stops spread
            Some (compileAndCache fragSrc)
        | FocalRadialGradient (center, radius, focus, stops, spread) ->
            let fragSrc = buildFocalRadialGradientFrag center radius focus stops spread
            Some (compileAndCache fragSrc)
        | _ -> None

    // ========== Post-process shader pipeline ==========

    let postProcessVertSrc = """
attribute vec2 aPos;
varying vec2 vUV;
void main() {
    vUV = aPos * 0.5 + 0.5;
    gl_Position = vec4(aPos, 0.0, 1.0);
}
"""

    let compilePostProcessFrag (quotation: Expr) : string =
        let (_vUV, body) = extractFragBody quotation
        let (funcDefs, mainBody) = extractFuncDefs body
        let (stmts, resultExpr) = emitStatements mainBody
        let funcDefsBlock = if funcDefs.IsEmpty then "" else (funcDefs |> String.concat "\n") + "\n"
        let stmtBlock = stmts |> String.concat "\n"
        "precision highp float;\nuniform sampler2D uTex0;\nuniform sampler2D uTex1;\nvarying vec2 vUV;\n"
        + funcDefsBlock
        + "void main() {\n"
        + "    vec2 uv = vUV;\n"
        + stmtBlock + "\n"
        + "    gl_FragColor = " + resultExpr + ";\n"
        + "}\n"

    // Compile a fragment shader with a custom header. Lambda parameter name must match the varying.
    let compileCustomFragShader (header: string) (quotation: Expr) : string =
        let (_v, body) = extractFragBody quotation
        let (funcDefs, mainBody) = extractFuncDefs body
        let (stmts, resultExpr) = emitStatements mainBody
        let funcDefsBlock = if funcDefs.IsEmpty then "" else (funcDefs |> String.concat "\n") + "\n"
        let stmtBlock = stmts |> String.concat "\n"
        header + "\n"
        + funcDefsBlock
        + "void main() {\n"
        + stmtBlock + "\n"
        + "    gl_FragColor = " + resultExpr + ";\n"
        + "}\n"

    // User-callable custom shader fill (used by Cmd.ShaderFill). Quotation must be
    // `fun (vPos: vec2) -> vec4` — vPos is in pixel-space and the result is the fragment color.
    let private shaderFillExprCache =
        System.Runtime.CompilerServices.ConditionalWeakTable<Expr, CompiledShader>()

    let buildShaderFillFrag (quotation: Expr) : string =
        compileCustomFragShader "precision highp float;\nvarying vec2 vPos;" quotation

    let buildShaderFill (quotation: Expr) : CompiledShader =
        match shaderFillExprCache.TryGetValue(quotation) with
        | true, cached -> cached
        | _ ->
            let compiled = compileAndCache (buildShaderFillFrag quotation)
            shaderFillExprCache.Add(quotation, compiled)
            compiled

    let compilePostProcessFragWithPrefix (prefix: string) (quotation: Expr) : string =
        let (_vUV, body) = extractFragBody quotation
        let (funcDefs, mainBody) = extractFuncDefs body
        let (stmts, resultExpr) = emitStatements mainBody
        let funcDefsBlock = if funcDefs.IsEmpty then "" else (funcDefs |> String.concat "\n") + "\n"
        let stmtBlock = stmts |> String.concat "\n"
        "precision highp float;\nuniform sampler2D uTex0;\nuniform sampler2D uTex1;\nvarying vec2 vUV;\n"
        + prefix + "\n"
        + funcDefsBlock
        + "void main() {\n"
        + "    vec2 uv = vUV;\n"
        + stmtBlock + "\n"
        + "    gl_FragColor = " + resultExpr + ";\n"
        + "}\n"

    type PostProcessShader =
        { Program: int; LocPos: int; LocTex0: int; LocTex1: int }

    let private ppShaderCache = Dictionary<string, PostProcessShader>()

    type TexturedQuadShader =
        { Program: int; LocPos: int; LocUV: int; LocMVP: int; LocTex0: int; LocOpacity: int }

    let mutable private texQuadShader : TexturedQuadShader option = None
    /// Same as texQuadShader but samples a GL_TEXTURE_EXTERNAL_OES (dmabuf EGLImage).
    let mutable private extTexQuadShader : TexturedQuadShader option = None

    type ImagePaintShader =
        { Program: int; LocPos: int; LocMVP: int; LocTex: int; LocInvXform: int; LocTexSize: int }

    let mutable private imgPaintShader : ImagePaintShader option = None
    let mutable private tilePaintShader : ImagePaintShader option = None

    let compilePostProcess (fragSrc: string) : PostProcessShader =
        match ppShaderCache.TryGetValue(fragSrc) with
        | true, cached -> cached
        | _ ->
            eprintfn "--- Compiled post-process GLSL ---\n%s\n%s\n---" postProcessVertSrc fragSrc
            let vs = Gl.compileShader Gl.GL_VERTEX_SHADER postProcessVertSrc
            let fs = Gl.compileShader Gl.GL_FRAGMENT_SHADER fragSrc
            let prog = Gl.linkProgram vs fs
            let locPos = Gl.glGetAttribLocation(prog, "aPos")
            let locTex0 = Gl.glGetUniformLocation(prog, "uTex0")
            let locTex1 = Gl.glGetUniformLocation(prog, "uTex1")
            let result = { Program = prog; LocPos = locPos; LocTex0 = locTex0; LocTex1 = locTex1 }
            ppShaderCache.[fragSrc] <- result
            result

    let clearCaches () =
        shaderCache.Clear()
        ppShaderCache.Clear()
        shaderFillExprCache.Clear()   // holds context-specific program IDs too
        texQuadShader <- None
        extTexQuadShader <- None
        imgPaintShader <- None
        tilePaintShader <- None

    // ========== Filter shader builders ==========

    let buildBrightnessShader (factor: float) : PostProcessShader =
        let f = float32 factor
        let fragSrc = compilePostProcessFrag
                        <@ fun (uv: vec2) ->
                            let c = GpuMath.texture2D 0 uv
                            mkVec4 (c.x * f) (c.y * f) (c.z * f) c.w @>
        compilePostProcess fragSrc

    let buildContrastShader (factor: float) : PostProcessShader =
        let f = float32 factor
        let fragSrc = compilePostProcessFrag
                        <@ fun (uv: vec2) ->
                            let c = GpuMath.texture2D 0 uv
                            let a = GpuMath.max' c.w 0.000001f
                            let r = c.x / a in let g = c.y / a in let b = c.z / a
                            let cr = (r - 0.5f) * f + 0.5f
                            let cg = (g - 0.5f) * f + 0.5f
                            let cb = (b - 0.5f) * f + 0.5f
                            mkVec4 (cr * c.w) (cg * c.w) (cb * c.w) c.w @>
        compilePostProcess fragSrc

    let buildSaturateShader (factor: float) : PostProcessShader =
        let f = float32 factor
        let fragSrc = compilePostProcessFrag
                        <@ fun (uv: vec2) ->
                            let c = GpuMath.texture2D 0 uv
                            let a = GpuMath.max' c.w 0.000001f
                            let r = c.x / a in let g = c.y / a in let b = c.z / a
                            let gray = 0.2126f * r + 0.7152f * g + 0.0722f * b
                            mkVec4 ((gray + (r - gray) * f) * c.w) ((gray + (g - gray) * f) * c.w) ((gray + (b - gray) * f) * c.w) c.w @>
        compilePostProcess fragSrc

    let buildGrayscaleShader (factor: float) : PostProcessShader =
        let f = float32 factor
        let fragSrc = compilePostProcessFrag
                        <@ fun (uv: vec2) ->
                            let c = GpuMath.texture2D 0 uv
                            let a = GpuMath.max' c.w 0.000001f
                            let r = c.x / a in let g = c.y / a in let b = c.z / a
                            let gray = 0.2126f * r + 0.7152f * g + 0.0722f * b
                            let mr = gray + (r - gray) * (1.0f - f)
                            let mg = gray + (g - gray) * (1.0f - f)
                            let mb = gray + (b - gray) * (1.0f - f)
                            mkVec4 (mr * c.w) (mg * c.w) (mb * c.w) c.w @>
        compilePostProcess fragSrc

    let buildSepiaShader (factor: float) : PostProcessShader =
        let f = float32 factor
        let fragSrc = compilePostProcessFrag
                        <@ fun (uv: vec2) ->
                            let c = GpuMath.texture2D 0 uv
                            let a = GpuMath.max' c.w 0.000001f
                            let r = c.x / a in let g = c.y / a in let b = c.z / a
                            let sr = GpuMath.min' 1.0f (0.393f * r + 0.769f * g + 0.189f * b)
                            let sg = GpuMath.min' 1.0f (0.349f * r + 0.686f * g + 0.168f * b)
                            let sb = GpuMath.min' 1.0f (0.272f * r + 0.534f * g + 0.131f * b)
                            let mr = r + (sr - r) * f
                            let mg = g + (sg - g) * f
                            let mb = b + (sb - b) * f
                            mkVec4 (mr * c.w) (mg * c.w) (mb * c.w) c.w @>
        compilePostProcess fragSrc

    let buildInvertShader (factor: float) : PostProcessShader =
        let f = float32 factor
        let fragSrc = compilePostProcessFrag
                        <@ fun (uv: vec2) ->
                            let c = GpuMath.texture2D 0 uv
                            let a = GpuMath.max' c.w 0.000001f
                            let r = c.x / a in let g = c.y / a in let b = c.z / a
                            let ir = r * (1.0f - f) + (1.0f - r) * f
                            let ig = g * (1.0f - f) + (1.0f - g) * f
                            let ib = b * (1.0f - f) + (1.0f - b) * f
                            mkVec4 (ir * c.w) (ig * c.w) (ib * c.w) c.w @>
        compilePostProcess fragSrc

    let buildHueRotateShader (degrees: float) : PostProcessShader =
        // Precompute Rodrigues rotation matrix around (1,1,1)/sqrt(3)
        let rad = degrees * System.Math.PI / 180.0
        let cosA = float32 (cos rad)
        let sinA = float32 (sin rad)
        let oneMinusCos = 1.0f - cosA
        let k = 1.0f / 3.0f
        let sq3 = 1.0f / float32 (sqrt 3.0)
        let r00 = cosA + oneMinusCos * k
        let r01 = oneMinusCos * k - sinA * sq3
        let r02 = oneMinusCos * k + sinA * sq3
        let r10 = oneMinusCos * k + sinA * sq3
        let r11 = cosA + oneMinusCos * k
        let r12 = oneMinusCos * k - sinA * sq3
        let r20 = oneMinusCos * k - sinA * sq3
        let r21 = oneMinusCos * k + sinA * sq3
        let r22 = cosA + oneMinusCos * k
        let fragSrc = compilePostProcessFrag
                        <@ fun (uv: vec2) ->
                            let c = GpuMath.texture2D 0 uv
                            let a = GpuMath.max' c.w 0.000001f
                            let r = c.x / a in let g = c.y / a in let b = c.z / a
                            let nr = GpuMath.clamp (r00 * r + r01 * g + r02 * b) 0.0f 1.0f
                            let ng = GpuMath.clamp (r10 * r + r11 * g + r12 * b) 0.0f 1.0f
                            let nb = GpuMath.clamp (r20 * r + r21 * g + r22 * b) 0.0f 1.0f
                            mkVec4 (nr * c.w) (ng * c.w) (nb * c.w) c.w @>
        compilePostProcess fragSrc

    let buildHBlurShader (radius: int) (texW: int) : PostProcessShader =
        let px = 1.0f / float32 texW
        let scale = 1.0f / float32 (2 * radius + 1)
        let fragSrc = compilePostProcessFrag
                        <@ fun (uv: vec2) ->
                            let mutable acc = mkVec4 0.0f 0.0f 0.0f 0.0f
                            for i = -radius to radius do
                                acc <- acc + GpuMath.texture2D 0 (mkVec2 (uv.x + float32 i * px) uv.y)
                            acc * scale @>
        compilePostProcess fragSrc

    let buildVBlurShader (radius: int) (texH: int) : PostProcessShader =
        let px = 1.0f / float32 texH
        let scale = 1.0f / float32 (2 * radius + 1)
        let fragSrc = compilePostProcessFrag
                        <@ fun (uv: vec2) ->
                            let mutable acc = mkVec4 0.0f 0.0f 0.0f 0.0f
                            for i = -radius to radius do
                                acc <- acc + GpuMath.texture2D 0 (mkVec2 uv.x (uv.y + float32 i * px))
                            acc * scale @>
        compilePostProcess fragSrc

    let buildDropShadowColorizeShader (dx: float) (dy: float) (color: Color) (texW: int) (texH: int) : PostProcessShader =
        let offsetX = float32 dx / float32 texW
        let offsetY = float32 dy / float32 texH
        let cr = float32 color.R / 255.0f
        let cg = float32 color.G / 255.0f
        let cb = float32 color.B / 255.0f
        let ca = float32 color.A / 255.0f
        let fragSrc = compilePostProcessFrag
                        <@ fun (uv: vec2) ->
                            let srcUv = mkVec2 (uv.x - offsetX) (uv.y - offsetY)
                            let c = GpuMath.texture2D 0 srcUv
                            let a = c.w * ca
                            mkVec4 (cr * a) (cg * a) (cb * a) a @>
        compilePostProcess fragSrc

    let buildOpacityShader (opacity: float) : PostProcessShader =
        let o = float32 opacity
        let fragSrc = compilePostProcessFrag
                        <@ fun (uv: vec2) ->
                            let c = GpuMath.texture2D 0 uv
                            mkVec4 (c.x * o) (c.y * o) (c.z * o) (c.w * o) @>
        compilePostProcess fragSrc

    let buildMaskShader () : PostProcessShader =
        let fragSrc = compilePostProcessFrag
                        <@ fun (uv: vec2) ->
                            let content = GpuMath.texture2D 0 uv
                            let mask = GpuMath.texture2D 1 uv
                            let ma = mask.w
                            mkVec4 (content.x * ma) (content.y * ma) (content.z * ma) (content.w * ma) @>
        compilePostProcess fragSrc

    // Passthrough shader for compositing a texture with blending
    let buildPassthroughShader () : PostProcessShader =
        let fragSrc = compilePostProcessFrag
                        <@ fun (uv: vec2) ->
                            GpuMath.texture2D 0 uv @>
        compilePostProcess fragSrc

    // ========== Textured quad shader for 3D transforms ==========

    let buildTexturedQuadShader () : TexturedQuadShader =
        match texQuadShader with
        | Some s -> s
        | None ->
            let vertSrc = """
attribute vec2 aPos;
attribute vec2 aUV;
uniform mat4 uMVP;
varying vec2 vUV;
void main() {
    gl_Position = uMVP * vec4(aPos, 0.0, 1.0);
    vUV = aUV;
}
"""
            let fragSrc =
                compileCustomFragShader
                    "precision highp float;\nvarying vec2 vUV;\nuniform sampler2D uTex0;\nuniform float uOpacity;"
                    <@ fun (vUV: vec2) ->
                        let c = GpuMath.texture2D 0 vUV
                        c * GpuMath.uniformFloat "uOpacity" @>
            eprintfn "--- Compiled textured-quad GLSL ---\n%s\n%s\n---" vertSrc fragSrc
            let vs = Gl.compileShader Gl.GL_VERTEX_SHADER vertSrc
            let fs = Gl.compileShader Gl.GL_FRAGMENT_SHADER fragSrc
            let prog = Gl.linkProgram vs fs
            let s =
                { Program = prog
                  LocPos = Gl.glGetAttribLocation(prog, "aPos")
                  LocUV = Gl.glGetAttribLocation(prog, "aUV")
                  LocMVP = Gl.glGetUniformLocation(prog, "uMVP")
                  LocTex0 = Gl.glGetUniformLocation(prog, "uTex0")
                  LocOpacity = Gl.glGetUniformLocation(prog, "uOpacity") }
            texQuadShader <- Some s
            s

    /// Textured-quad shader that samples a GL_TEXTURE_EXTERNAL_OES (an EGLImage
    /// imported from a client dmabuf). Same interface (LocPos/LocUV/LocMVP/LocTex0/
    /// LocOpacity) as buildTexturedQuadShader so the GL backend draws it identically,
    /// only the sampler type differs. Hand-written GLSL (the quotation compiler emits
    /// sampler2D, but external images need samplerExternalOES + the OES extension).
    let buildExternalTexturedQuadShader () : TexturedQuadShader =
        match extTexQuadShader with
        | Some s -> s
        | None ->
            let vertSrc = """
attribute vec2 aPos;
attribute vec2 aUV;
uniform mat4 uMVP;
varying vec2 vUV;
void main() {
    gl_Position = uMVP * vec4(aPos, 0.0, 1.0);
    vUV = aUV;
}
"""
            let fragSrc = """#extension GL_OES_EGL_image_external : require
precision highp float;
varying vec2 vUV;
uniform samplerExternalOES uTex0;
uniform float uOpacity;
void main() {
    gl_FragColor = texture2D(uTex0, vUV) * uOpacity;
}
"""
            let vs = Gl.compileShader Gl.GL_VERTEX_SHADER vertSrc
            let fs = Gl.compileShader Gl.GL_FRAGMENT_SHADER fragSrc
            let prog = Gl.linkProgram vs fs
            let s =
                { Program = prog
                  LocPos = Gl.glGetAttribLocation(prog, "aPos")
                  LocUV = Gl.glGetAttribLocation(prog, "aUV")
                  LocMVP = Gl.glGetUniformLocation(prog, "uMVP")
                  LocTex0 = Gl.glGetUniformLocation(prog, "uTex0")
                  LocOpacity = Gl.glGetUniformLocation(prog, "uOpacity") }
            extTexQuadShader <- Some s
            s

    // ========== Image paint shader for stencil-then-cover with texture ==========

    let buildImagePaintShader () : ImagePaintShader =
        match imgPaintShader with
        | Some s -> s
        | None ->
            let vertSrc = """
attribute vec2 aPos;
uniform mat4 uMVP;
varying vec2 vPos;
void main() {
    gl_Position = uMVP * vec4(aPos, 0.0, 1.0);
    vPos = aPos;
}
"""
            let fragSrc =
                compileCustomFragShader
                    "precision highp float;\nvarying vec2 vPos;\nuniform sampler2D uTex;\nuniform mat4 uInvXform;\nuniform vec2 uTexSize;"
                    <@ fun (vPos: vec2) ->
                        let tp = GpuMath.mat4MulVec4 "uInvXform" (mkVec4 vPos.x vPos.y 0.0f 1.0f)
                        let texSize = GpuMath.uniformVec2 "uTexSize"
                        let uvx = tp.x / texSize.x
                        let uvy = tp.y / texSize.y
                        (if uvx < 0.0f then mkVec4 0.0f 0.0f 0.0f 0.0f
                         elif uvx > 1.0f then mkVec4 0.0f 0.0f 0.0f 0.0f
                         elif uvy < 0.0f then mkVec4 0.0f 0.0f 0.0f 0.0f
                         elif uvy > 1.0f then mkVec4 0.0f 0.0f 0.0f 0.0f
                         else GpuMath.texture2DName "uTex" (mkVec2 uvx uvy)) @>
            let vs = Gl.compileShader Gl.GL_VERTEX_SHADER vertSrc
            let fs = Gl.compileShader Gl.GL_FRAGMENT_SHADER fragSrc
            let prog = Gl.linkProgram vs fs
            let s =
                { Program = prog
                  LocPos = Gl.glGetAttribLocation(prog, "aPos")
                  LocMVP = Gl.glGetUniformLocation(prog, "uMVP")
                  LocTex = Gl.glGetUniformLocation(prog, "uTex")
                  LocInvXform = Gl.glGetUniformLocation(prog, "uInvXform")
                  LocTexSize = Gl.glGetUniformLocation(prog, "uTexSize") }
            imgPaintShader <- Some s
            s

    // ========== Tiling paint shader for stencil-then-cover with repeating texture ==========

    let buildTilingPaintShader () : ImagePaintShader =
        match tilePaintShader with
        | Some s -> s
        | None ->
            let vertSrc = """
attribute vec2 aPos;
uniform mat4 uMVP;
varying vec2 vPos;
void main() {
    gl_Position = uMVP * vec4(aPos, 0.0, 1.0);
    vPos = aPos;
}
"""
            let fragSrc =
                compileCustomFragShader
                    "precision highp float;\nvarying vec2 vPos;\nuniform sampler2D uTex;\nuniform mat4 uInvXform;\nuniform vec2 uTexSize;"
                    <@ fun (vPos: vec2) ->
                        let tp = GpuMath.mat4MulVec4 "uInvXform" (mkVec4 vPos.x vPos.y 0.0f 1.0f)
                        let texSize = GpuMath.uniformVec2 "uTexSize"
                        GpuMath.texture2DName "uTex" (mkVec2 (tp.x / texSize.x) (tp.y / texSize.y)) @>
            let vs = Gl.compileShader Gl.GL_VERTEX_SHADER vertSrc
            let fs = Gl.compileShader Gl.GL_FRAGMENT_SHADER fragSrc
            let prog = Gl.linkProgram vs fs
            let s =
                { Program = prog
                  LocPos = Gl.glGetAttribLocation(prog, "aPos")
                  LocMVP = Gl.glGetUniformLocation(prog, "uMVP")
                  LocTex = Gl.glGetUniformLocation(prog, "uTex")
                  LocInvXform = Gl.glGetUniformLocation(prog, "uInvXform")
                  LocTexSize = Gl.glGetUniformLocation(prog, "uTexSize") }
            tilePaintShader <- Some s
            s

    // ========== Blend mode shader builders ==========

    // Blend mode shaders (all modes)
    let buildBlendShader (mode: BlendMode) : PostProcessShader =
        match mode with
        | Normal ->
            compilePostProcessFrag
                <@ fun (uv: vec2) ->
                    let src = GpuMath.texture2D 0 uv
                    let dst = GpuMath.texture2D 1 uv
                    let inv = 1.0f - src.w
                    mkVec4 (src.x + dst.x * inv) (src.y + dst.y * inv) (src.z + dst.z * inv) (src.w + dst.w * inv) @>
            |> compilePostProcess
        | SrcIn ->
            // Preserve dst where src is transparent (matches software behavior)
            compilePostProcessFrag
                <@ fun (uv: vec2) ->
                    let src = GpuMath.texture2D 0 uv
                    let dst = GpuMath.texture2D 1 uv
                    let da = dst.w
                    let h = (if src.w < 0.001f then 0.0f else 1.0f)
                    let ih = 1.0f - h
                    mkVec4 (src.x * da * h + dst.x * ih) (src.y * da * h + dst.y * ih) (src.z * da * h + dst.z * ih) (src.w * da * h + dst.w * ih) @>
            |> compilePostProcess
        | SrcOut ->
            compilePostProcessFrag
                <@ fun (uv: vec2) ->
                    let src = GpuMath.texture2D 0 uv
                    let dst = GpuMath.texture2D 1 uv
                    let inv = 1.0f - dst.w
                    let h = (if src.w < 0.001f then 0.0f else 1.0f)
                    let ih = 1.0f - h
                    mkVec4 (src.x * inv * h + dst.x * ih) (src.y * inv * h + dst.y * ih) (src.z * inv * h + dst.z * ih) (src.w * inv * h + dst.w * ih) @>
            |> compilePostProcess
        | DstOver ->
            compilePostProcessFrag
                <@ fun (uv: vec2) ->
                    let src = GpuMath.texture2D 0 uv
                    let dst = GpuMath.texture2D 1 uv
                    let inv = 1.0f - dst.w
                    mkVec4 (dst.x + src.x * inv) (dst.y + src.y * inv) (dst.z + src.z * inv) (dst.w + src.w * inv) @>
            |> compilePostProcess
        | DstAtop ->
            compilePostProcessFrag
                <@ fun (uv: vec2) ->
                    let src = GpuMath.texture2D 0 uv
                    let dst = GpuMath.texture2D 1 uv
                    let sa = src.w
                    let inv = 1.0f - dst.w
                    let h = (if src.w < 0.001f then 0.0f else 1.0f)
                    let ih = 1.0f - h
                    mkVec4 (( dst.x * sa + src.x * inv) * h + dst.x * ih) ((dst.y * sa + src.y * inv) * h + dst.y * ih) ((dst.z * sa + src.z * inv) * h + dst.z * ih) (src.w * h + dst.w * ih) @>
            |> compilePostProcess
        | Xor ->
            compilePostProcessFrag
                <@ fun (uv: vec2) ->
                    let src = GpuMath.texture2D 0 uv
                    let dst = GpuMath.texture2D 1 uv
                    let invS = 1.0f - src.w
                    let invD = 1.0f - dst.w
                    mkVec4 (src.x * invD + dst.x * invS) (src.y * invD + dst.y * invS) (src.z * invD + dst.z * invS) (src.w * invD + dst.w * invS) @>
            |> compilePostProcess

        // Separable blend modes: unpremultiply, blend, recomposite
        | Multiply ->
            compilePostProcessFrag
                <@ fun (uv: vec2) ->
                    let src = GpuMath.texture2D 0 uv
                    let dst = GpuMath.texture2D 1 uv
                    let sa = GpuMath.max' src.w 0.000001f
                    let da = GpuMath.max' dst.w 0.000001f
                    let cs_r = src.x / sa in let cs_g = src.y / sa in let cs_b = src.z / sa
                    let cd_r = dst.x / da in let cd_g = dst.y / da in let cd_b = dst.z / da
                    let br = cs_r * cd_r in let bg = cs_g * cd_g in let bb = cs_b * cd_b
                    let oa = sa + da * (1.0f - sa)
                    let invDa = 1.0f - da in let invSa = 1.0f - sa
                    mkVec4 (invDa * src.x + invSa * dst.x + sa * da * br) (invDa * src.y + invSa * dst.y + sa * da * bg) (invDa * src.z + invSa * dst.z + sa * da * bb) oa @>
            |> compilePostProcess
        | Screen ->
            compilePostProcessFrag
                <@ fun (uv: vec2) ->
                    let src = GpuMath.texture2D 0 uv
                    let dst = GpuMath.texture2D 1 uv
                    let sa = GpuMath.max' src.w 0.000001f
                    let da = GpuMath.max' dst.w 0.000001f
                    let cs_r = src.x / sa in let cs_g = src.y / sa in let cs_b = src.z / sa
                    let cd_r = dst.x / da in let cd_g = dst.y / da in let cd_b = dst.z / da
                    let br = cs_r + cd_r - cs_r * cd_r
                    let bg = cs_g + cd_g - cs_g * cd_g
                    let bb = cs_b + cd_b - cs_b * cd_b
                    let oa = sa + da * (1.0f - sa)
                    let invDa = 1.0f - da in let invSa = 1.0f - sa
                    mkVec4 (invDa * src.x + invSa * dst.x + sa * da * br) (invDa * src.y + invSa * dst.y + sa * da * bg) (invDa * src.z + invSa * dst.z + sa * da * bb) oa @>
            |> compilePostProcess
        | Overlay ->
            compilePostProcessFrag
                <@ fun (uv: vec2) ->
                    let src = GpuMath.texture2D 0 uv
                    let dst = GpuMath.texture2D 1 uv
                    let sa = GpuMath.max' src.w 0.000001f
                    let da = GpuMath.max' dst.w 0.000001f
                    let cs_r = src.x / sa in let cs_g = src.y / sa in let cs_b = src.z / sa
                    let cd_r = dst.x / da in let cd_g = dst.y / da in let cd_b = dst.z / da
                    let br = (if cd_r < 0.5f then 2.0f * cs_r * cd_r else 1.0f - 2.0f * (1.0f - cs_r) * (1.0f - cd_r))
                    let bg = (if cd_g < 0.5f then 2.0f * cs_g * cd_g else 1.0f - 2.0f * (1.0f - cs_g) * (1.0f - cd_g))
                    let bb = (if cd_b < 0.5f then 2.0f * cs_b * cd_b else 1.0f - 2.0f * (1.0f - cs_b) * (1.0f - cd_b))
                    let oa = sa + da * (1.0f - sa)
                    let invDa = 1.0f - da in let invSa = 1.0f - sa
                    mkVec4 (invDa * src.x + invSa * dst.x + sa * da * br) (invDa * src.y + invSa * dst.y + sa * da * bg) (invDa * src.z + invSa * dst.z + sa * da * bb) oa @>
            |> compilePostProcess
        | Darken ->
            compilePostProcessFrag
                <@ fun (uv: vec2) ->
                    let src = GpuMath.texture2D 0 uv
                    let dst = GpuMath.texture2D 1 uv
                    let sa = GpuMath.max' src.w 0.000001f
                    let da = GpuMath.max' dst.w 0.000001f
                    let cs_r = src.x / sa in let cs_g = src.y / sa in let cs_b = src.z / sa
                    let cd_r = dst.x / da in let cd_g = dst.y / da in let cd_b = dst.z / da
                    let br = GpuMath.min' cs_r cd_r in let bg = GpuMath.min' cs_g cd_g in let bb = GpuMath.min' cs_b cd_b
                    let oa = sa + da * (1.0f - sa)
                    let invDa = 1.0f - da in let invSa = 1.0f - sa
                    mkVec4 (invDa * src.x + invSa * dst.x + sa * da * br) (invDa * src.y + invSa * dst.y + sa * da * bg) (invDa * src.z + invSa * dst.z + sa * da * bb) oa @>
            |> compilePostProcess
        | Lighten ->
            compilePostProcessFrag
                <@ fun (uv: vec2) ->
                    let src = GpuMath.texture2D 0 uv
                    let dst = GpuMath.texture2D 1 uv
                    let sa = GpuMath.max' src.w 0.000001f
                    let da = GpuMath.max' dst.w 0.000001f
                    let cs_r = src.x / sa in let cs_g = src.y / sa in let cs_b = src.z / sa
                    let cd_r = dst.x / da in let cd_g = dst.y / da in let cd_b = dst.z / da
                    let br = GpuMath.max' cs_r cd_r in let bg = GpuMath.max' cs_g cd_g in let bb = GpuMath.max' cs_b cd_b
                    let oa = sa + da * (1.0f - sa)
                    let invDa = 1.0f - da in let invSa = 1.0f - sa
                    mkVec4 (invDa * src.x + invSa * dst.x + sa * da * br) (invDa * src.y + invSa * dst.y + sa * da * bg) (invDa * src.z + invSa * dst.z + sa * da * bb) oa @>
            |> compilePostProcess
        | ColorDodge ->
            compilePostProcessFrag
                <@ fun (uv: vec2) ->
                    let src = GpuMath.texture2D 0 uv
                    let dst = GpuMath.texture2D 1 uv
                    let sa = GpuMath.max' src.w 0.000001f
                    let da = GpuMath.max' dst.w 0.000001f
                    let cs_r = src.x / sa in let cs_g = src.y / sa in let cs_b = src.z / sa
                    let cd_r = dst.x / da in let cd_g = dst.y / da in let cd_b = dst.z / da
                    let br = (if cs_r >= 1.0f then 1.0f elif cd_r < 0.000001f then 0.0f else GpuMath.min' 1.0f (cd_r / (1.0f - cs_r)))
                    let bg = (if cs_g >= 1.0f then 1.0f elif cd_g < 0.000001f then 0.0f else GpuMath.min' 1.0f (cd_g / (1.0f - cs_g)))
                    let bb = (if cs_b >= 1.0f then 1.0f elif cd_b < 0.000001f then 0.0f else GpuMath.min' 1.0f (cd_b / (1.0f - cs_b)))
                    let oa = sa + da * (1.0f - sa)
                    let invDa = 1.0f - da in let invSa = 1.0f - sa
                    mkVec4 (invDa * src.x + invSa * dst.x + sa * da * br) (invDa * src.y + invSa * dst.y + sa * da * bg) (invDa * src.z + invSa * dst.z + sa * da * bb) oa @>
            |> compilePostProcess
        | ColorBurn ->
            compilePostProcessFrag
                <@ fun (uv: vec2) ->
                    let src = GpuMath.texture2D 0 uv
                    let dst = GpuMath.texture2D 1 uv
                    let sa = GpuMath.max' src.w 0.000001f
                    let da = GpuMath.max' dst.w 0.000001f
                    let cs_r = src.x / sa in let cs_g = src.y / sa in let cs_b = src.z / sa
                    let cd_r = dst.x / da in let cd_g = dst.y / da in let cd_b = dst.z / da
                    let br = (if cs_r < 0.000001f then 0.0f elif cd_r >= 1.0f then 1.0f else 1.0f - GpuMath.min' 1.0f ((1.0f - cd_r) / cs_r))
                    let bg = (if cs_g < 0.000001f then 0.0f elif cd_g >= 1.0f then 1.0f else 1.0f - GpuMath.min' 1.0f ((1.0f - cd_g) / cs_g))
                    let bb = (if cs_b < 0.000001f then 0.0f elif cd_b >= 1.0f then 1.0f else 1.0f - GpuMath.min' 1.0f ((1.0f - cd_b) / cs_b))
                    let oa = sa + da * (1.0f - sa)
                    let invDa = 1.0f - da in let invSa = 1.0f - sa
                    mkVec4 (invDa * src.x + invSa * dst.x + sa * da * br) (invDa * src.y + invSa * dst.y + sa * da * bg) (invDa * src.z + invSa * dst.z + sa * da * bb) oa @>
            |> compilePostProcess
        | SoftLight ->
            compilePostProcessFrag
                <@ fun (uv: vec2) ->
                    let src = GpuMath.texture2D 0 uv
                    let dst = GpuMath.texture2D 1 uv
                    let sa = GpuMath.max' src.w 0.000001f
                    let da = GpuMath.max' dst.w 0.000001f
                    let cs_r = src.x / sa in let cs_g = src.y / sa in let cs_b = src.z / sa
                    let cd_r = dst.x / da in let cd_g = dst.y / da in let cd_b = dst.z / da
                    let br = (if cs_r <= 0.5f then cd_r - (1.0f - 2.0f * cs_r) * cd_r * (1.0f - cd_r) else cd_r + (2.0f * cs_r - 1.0f) * (GpuMath.sqrt' cd_r - cd_r))
                    let bg = (if cs_g <= 0.5f then cd_g - (1.0f - 2.0f * cs_g) * cd_g * (1.0f - cd_g) else cd_g + (2.0f * cs_g - 1.0f) * (GpuMath.sqrt' cd_g - cd_g))
                    let bb = (if cs_b <= 0.5f then cd_b - (1.0f - 2.0f * cs_b) * cd_b * (1.0f - cd_b) else cd_b + (2.0f * cs_b - 1.0f) * (GpuMath.sqrt' cd_b - cd_b))
                    let oa = sa + da * (1.0f - sa)
                    let invDa = 1.0f - da in let invSa = 1.0f - sa
                    mkVec4 (invDa * src.x + invSa * dst.x + sa * da * br) (invDa * src.y + invSa * dst.y + sa * da * bg) (invDa * src.z + invSa * dst.z + sa * da * bb) oa @>
            |> compilePostProcess
        | Difference ->
            compilePostProcessFrag
                <@ fun (uv: vec2) ->
                    let src = GpuMath.texture2D 0 uv
                    let dst = GpuMath.texture2D 1 uv
                    let sa = GpuMath.max' src.w 0.000001f
                    let da = GpuMath.max' dst.w 0.000001f
                    let cs_r = src.x / sa in let cs_g = src.y / sa in let cs_b = src.z / sa
                    let cd_r = dst.x / da in let cd_g = dst.y / da in let cd_b = dst.z / da
                    let br = GpuMath.abs' (cs_r - cd_r) in let bg = GpuMath.abs' (cs_g - cd_g) in let bb = GpuMath.abs' (cs_b - cd_b)
                    let oa = sa + da * (1.0f - sa)
                    let invDa = 1.0f - da in let invSa = 1.0f - sa
                    mkVec4 (invDa * src.x + invSa * dst.x + sa * da * br) (invDa * src.y + invSa * dst.y + sa * da * bg) (invDa * src.z + invSa * dst.z + sa * da * bb) oa @>
            |> compilePostProcess
        | Exclusion ->
            compilePostProcessFrag
                <@ fun (uv: vec2) ->
                    let src = GpuMath.texture2D 0 uv
                    let dst = GpuMath.texture2D 1 uv
                    let sa = GpuMath.max' src.w 0.000001f
                    let da = GpuMath.max' dst.w 0.000001f
                    let cs_r = src.x / sa in let cs_g = src.y / sa in let cs_b = src.z / sa
                    let cd_r = dst.x / da in let cd_g = dst.y / da in let cd_b = dst.z / da
                    let br = cs_r + cd_r - 2.0f * cs_r * cd_r
                    let bg = cs_g + cd_g - 2.0f * cs_g * cd_g
                    let bb = cs_b + cd_b - 2.0f * cs_b * cd_b
                    let oa = sa + da * (1.0f - sa)
                    let invDa = 1.0f - da in let invSa = 1.0f - sa
                    mkVec4 (invDa * src.x + invSa * dst.x + sa * da * br) (invDa * src.y + invSa * dst.y + sa * da * bg) (invDa * src.z + invSa * dst.z + sa * da * bb) oa @>
            |> compilePostProcess

        // HSL blend modes: compiled from quotations with let-defined helper functions
        | Hue ->
            compilePostProcessFrag
                <@ fun (uv: vec2) ->
                    let hslLum (c: vec3) = 0.299f * c.x + 0.587f * c.y + 0.114f * c.z
                    let clipColor (c: vec3) =
                        let l = hslLum c
                        let n = GpuMath.min' c.x (GpuMath.min' c.y c.z)
                        let x = GpuMath.max' c.x (GpuMath.max' c.y c.z)
                        let c1 = (if n < 0.0f then (if l <> n then mkVec3s l + (c - mkVec3s l) * (l / (l - n)) else c) else c)
                        (if x > 1.0f then (if x <> l then mkVec3s l + (c1 - mkVec3s l) * ((1.0f - l) / (x - l)) else c1) else c1)
                    let setLum (c: vec3) (l: float32) =
                        let d = l - hslLum c
                        clipColor (c + mkVec3s d)
                    let hue2rgb (p: float32) (q: float32) (t: float32) =
                        let t1 = t + (if t < 0.0f then 1.0f else 0.0f)
                        let t2 = t1 - (if t1 > 1.0f then 1.0f else 0.0f)
                        (if t2 < 1.0f / 6.0f then p + (q - p) * 6.0f * t2
                         elif t2 < 0.5f then q
                         elif t2 < 2.0f / 3.0f then p + (q - p) * (2.0f / 3.0f - t2) * 6.0f
                         else p)
                    let rgbToHsl (c: vec3) =
                        let mx = GpuMath.max' c.x (GpuMath.max' c.y c.z)
                        let mn = GpuMath.min' c.x (GpuMath.min' c.y c.z)
                        let d = mx - mn
                        let l = (mx + mn) * 0.5f
                        let s = (if l <= 0.5f then d / (GpuMath.max' (mx + mn) 0.00001f) else d / (GpuMath.max' (2.0f - mx - mn) 0.00001f))
                        let h = (if mx = c.x then (c.y - c.z) / (GpuMath.max' d 0.00001f) + (if c.y < c.z then 6.0f else 0.0f)
                                 elif mx = c.y then (c.z - c.x) / (GpuMath.max' d 0.00001f) + 2.0f
                                 else (c.x - c.y) / (GpuMath.max' d 0.00001f) + 4.0f)
                        (if d < 0.00001f then mkVec3 0.0f 0.0f l else mkVec3 (h / 6.0f) s l)
                    let hslToRgb (hsl: vec3) =
                        let q = (if hsl.z < 0.5f then hsl.z * (1.0f + hsl.y) else hsl.z + hsl.y - hsl.z * hsl.y)
                        let p = 2.0f * hsl.z - q
                        (if hsl.y < 0.00001f then mkVec3s hsl.z
                         else mkVec3 (hue2rgb p q (hsl.x + 1.0f / 3.0f)) (hue2rgb p q hsl.x) (hue2rgb p q (hsl.x - 1.0f / 3.0f)))
                    let src = GpuMath.texture2D 0 uv
                    let dst = GpuMath.texture2D 1 uv
                    let sa = GpuMath.max' src.w 0.000001f
                    let da = GpuMath.max' dst.w 0.000001f
                    let cs = mkVec3 (src.x / sa) (src.y / sa) (src.z / sa)
                    let cd = mkVec3 (dst.x / da) (dst.y / da) (dst.z / da)
                    let hsS = rgbToHsl cs
                    let hsD = rgbToHsl cd
                    let br = setLum (hslToRgb (mkVec3 hsS.x hsD.y hsD.z)) (hslLum cd)
                    let oa = sa + da * (1.0f - sa)
                    let invDa = 1.0f - da in let invSa = 1.0f - sa
                    mkVec4 (invDa * src.x + invSa * dst.x + sa * da * br.x) (invDa * src.y + invSa * dst.y + sa * da * br.y) (invDa * src.z + invSa * dst.z + sa * da * br.z) oa @>
            |> compilePostProcess
        | Saturation ->
            compilePostProcessFrag
                <@ fun (uv: vec2) ->
                    let hslLum (c: vec3) = 0.299f * c.x + 0.587f * c.y + 0.114f * c.z
                    let clipColor (c: vec3) =
                        let l = hslLum c
                        let n = GpuMath.min' c.x (GpuMath.min' c.y c.z)
                        let x = GpuMath.max' c.x (GpuMath.max' c.y c.z)
                        let c1 = (if n < 0.0f then (if l <> n then mkVec3s l + (c - mkVec3s l) * (l / (l - n)) else c) else c)
                        (if x > 1.0f then (if x <> l then mkVec3s l + (c1 - mkVec3s l) * ((1.0f - l) / (x - l)) else c1) else c1)
                    let setLum (c: vec3) (l: float32) =
                        let d = l - hslLum c
                        clipColor (c + mkVec3s d)
                    let hue2rgb (p: float32) (q: float32) (t: float32) =
                        let t1 = t + (if t < 0.0f then 1.0f else 0.0f)
                        let t2 = t1 - (if t1 > 1.0f then 1.0f else 0.0f)
                        (if t2 < 1.0f / 6.0f then p + (q - p) * 6.0f * t2
                         elif t2 < 0.5f then q
                         elif t2 < 2.0f / 3.0f then p + (q - p) * (2.0f / 3.0f - t2) * 6.0f
                         else p)
                    let rgbToHsl (c: vec3) =
                        let mx = GpuMath.max' c.x (GpuMath.max' c.y c.z)
                        let mn = GpuMath.min' c.x (GpuMath.min' c.y c.z)
                        let d = mx - mn
                        let l = (mx + mn) * 0.5f
                        let s = (if l <= 0.5f then d / (GpuMath.max' (mx + mn) 0.00001f) else d / (GpuMath.max' (2.0f - mx - mn) 0.00001f))
                        let h = (if mx = c.x then (c.y - c.z) / (GpuMath.max' d 0.00001f) + (if c.y < c.z then 6.0f else 0.0f)
                                 elif mx = c.y then (c.z - c.x) / (GpuMath.max' d 0.00001f) + 2.0f
                                 else (c.x - c.y) / (GpuMath.max' d 0.00001f) + 4.0f)
                        (if d < 0.00001f then mkVec3 0.0f 0.0f l else mkVec3 (h / 6.0f) s l)
                    let hslToRgb (hsl: vec3) =
                        let q = (if hsl.z < 0.5f then hsl.z * (1.0f + hsl.y) else hsl.z + hsl.y - hsl.z * hsl.y)
                        let p = 2.0f * hsl.z - q
                        (if hsl.y < 0.00001f then mkVec3s hsl.z
                         else mkVec3 (hue2rgb p q (hsl.x + 1.0f / 3.0f)) (hue2rgb p q hsl.x) (hue2rgb p q (hsl.x - 1.0f / 3.0f)))
                    let src = GpuMath.texture2D 0 uv
                    let dst = GpuMath.texture2D 1 uv
                    let sa = GpuMath.max' src.w 0.000001f
                    let da = GpuMath.max' dst.w 0.000001f
                    let cs = mkVec3 (src.x / sa) (src.y / sa) (src.z / sa)
                    let cd = mkVec3 (dst.x / da) (dst.y / da) (dst.z / da)
                    let hsS = rgbToHsl cs
                    let hsD = rgbToHsl cd
                    let br = setLum (hslToRgb (mkVec3 hsD.x hsS.y hsD.z)) (hslLum cd)
                    let oa = sa + da * (1.0f - sa)
                    let invDa = 1.0f - da in let invSa = 1.0f - sa
                    mkVec4 (invDa * src.x + invSa * dst.x + sa * da * br.x) (invDa * src.y + invSa * dst.y + sa * da * br.y) (invDa * src.z + invSa * dst.z + sa * da * br.z) oa @>
            |> compilePostProcess
        | ColorMode ->
            compilePostProcessFrag
                <@ fun (uv: vec2) ->
                    let hslLum (c: vec3) = 0.299f * c.x + 0.587f * c.y + 0.114f * c.z
                    let clipColor (c: vec3) =
                        let l = hslLum c
                        let n = GpuMath.min' c.x (GpuMath.min' c.y c.z)
                        let x = GpuMath.max' c.x (GpuMath.max' c.y c.z)
                        let c1 = (if n < 0.0f then (if l <> n then mkVec3s l + (c - mkVec3s l) * (l / (l - n)) else c) else c)
                        (if x > 1.0f then (if x <> l then mkVec3s l + (c1 - mkVec3s l) * ((1.0f - l) / (x - l)) else c1) else c1)
                    let setLum (c: vec3) (l: float32) =
                        let d = l - hslLum c
                        clipColor (c + mkVec3s d)
                    let src = GpuMath.texture2D 0 uv
                    let dst = GpuMath.texture2D 1 uv
                    let sa = GpuMath.max' src.w 0.000001f
                    let da = GpuMath.max' dst.w 0.000001f
                    let cs = mkVec3 (src.x / sa) (src.y / sa) (src.z / sa)
                    let cd = mkVec3 (dst.x / da) (dst.y / da) (dst.z / da)
                    let br = setLum cs (hslLum cd)
                    let oa = sa + da * (1.0f - sa)
                    let invDa = 1.0f - da in let invSa = 1.0f - sa
                    mkVec4 (invDa * src.x + invSa * dst.x + sa * da * br.x) (invDa * src.y + invSa * dst.y + sa * da * br.y) (invDa * src.z + invSa * dst.z + sa * da * br.z) oa @>
            |> compilePostProcess
        | Luminosity ->
            compilePostProcessFrag
                <@ fun (uv: vec2) ->
                    let hslLum (c: vec3) = 0.299f * c.x + 0.587f * c.y + 0.114f * c.z
                    let clipColor (c: vec3) =
                        let l = hslLum c
                        let n = GpuMath.min' c.x (GpuMath.min' c.y c.z)
                        let x = GpuMath.max' c.x (GpuMath.max' c.y c.z)
                        let c1 = (if n < 0.0f then (if l <> n then mkVec3s l + (c - mkVec3s l) * (l / (l - n)) else c) else c)
                        (if x > 1.0f then (if x <> l then mkVec3s l + (c1 - mkVec3s l) * ((1.0f - l) / (x - l)) else c1) else c1)
                    let setLum (c: vec3) (l: float32) =
                        let d = l - hslLum c
                        clipColor (c + mkVec3s d)
                    let src = GpuMath.texture2D 0 uv
                    let dst = GpuMath.texture2D 1 uv
                    let sa = GpuMath.max' src.w 0.000001f
                    let da = GpuMath.max' dst.w 0.000001f
                    let cs = mkVec3 (src.x / sa) (src.y / sa) (src.z / sa)
                    let cd = mkVec3 (dst.x / da) (dst.y / da) (dst.z / da)
                    let br = setLum cd (hslLum cs)
                    let oa = sa + da * (1.0f - sa)
                    let invDa = 1.0f - da in let invSa = 1.0f - sa
                    mkVec4 (invDa * src.x + invSa * dst.x + sa * da * br.x) (invDa * src.y + invSa * dst.y + sa * da * br.y) (invDa * src.z + invSa * dst.z + sa * da * br.z) oa @>
            |> compilePostProcess
