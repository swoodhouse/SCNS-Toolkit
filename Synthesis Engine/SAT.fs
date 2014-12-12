// code adapted from Z3Fs - https://github.com/dungpa/Z3Fs

module SAT

open System.Collections.Generic
open Microsoft.Z3
open System

let private context = new Context(Dictionary())
let getContext() = context
let Solver() = getContext().MkSolver()
let inline (++) xs ys = Array.append xs ys

[<AbstractClass>]
type Theory() =
    abstract member Expr: Expr

type Microsoft.Z3.Statistics with
    member x.GetEnumerator() =
        (Seq.map (fun k -> k, x.[k]) x.Keys).GetEnumerator()

type Result =
| Const of Expr
| Func of FuncInterp
with override x.ToString() =
    match x with
    | Const expr -> sprintf "%O" expr
    | Func f -> sprintf "%O" f

/// Multiple indexers for evaluating formulas
type Microsoft.Z3.Model with
    member x.Item (index: Expr) = x.Eval(index, true)
    member x.Item (index: FuncDecl) =
        if index.DomainSize = 0u && index.Range.SortKind <> Z3_sort_kind.Z3_ARRAY_SORT // Taking care of array declaration
        then x.ConstInterp(index) |> Const
        else x.FuncInterp(index) |> Func
    member x.Evaluate(v: Theory, ?modelCompletion) =
        x.Evaluate(v.Expr, defaultArg modelCompletion false)

type Bool(e: BoolExpr) =
    inherit Theory()
    override x.Expr = e :> Expr
    override x.ToString() = sprintf "%O" e
    static member FromExpr (e: Expr) = Bool(e :?> BoolExpr)

let BoolExpr expr = Bool(expr)
let (|BoolExpr|) (b: Bool) = b.Expr :?> BoolExpr

type Microsoft.Z3.Solver with
    member x.Add([<ParamArray>] xs: _ []) =
        for (BoolExpr expr) in xs do
            x.Assert expr

[<AutoOpen>]
module internal BoolUtils =
    let inline mkBool b = getContext().MkBool(b)
    let inline mkAnd x y = getContext().MkAnd(x, y) |> BoolExpr
    let inline mkOr x y = getContext().MkOr(x, y) |> BoolExpr
    let inline mkNot x = getContext().MkNot(x) |> BoolExpr
    let inline mkImplies x y = getContext().MkImplies(x, y) |> BoolExpr
    let inline mkEquiv x y = getContext().MkEq(x, y) |> BoolExpr
    let inline mkTrue() = getContext().MkTrue() |> BoolExpr
    let inline mkFalse() = getContext().MkFalse() |> BoolExpr
    let inline mkDistinct (xs: Expr []) = getContext().MkDistinct xs |> BoolExpr
    let inline mkITE b expr1 expr2 = getContext().MkITE(b, expr1, expr2) :?> BoolExpr |> BoolExpr

type Bool with
    static member (&&.)(BoolExpr p, BoolExpr q) = mkAnd p q
    static member (&&.)(BoolExpr p, q) = mkAnd p (mkBool q)
    static member (&&.)(p, BoolExpr q) = mkAnd (mkBool p) q
    static member (||.)(BoolExpr p, BoolExpr q) = mkOr p q
    static member (||.)(BoolExpr p, q) = mkOr p (mkBool q)
    static member (||.)(p, BoolExpr q) = mkOr (mkBool p) q
    static member (!.) (BoolExpr p) = mkNot p
    static member (=>.)(BoolExpr p, BoolExpr q) = mkImplies p q
    static member (=>.)(BoolExpr p, q) = mkImplies p (mkBool q)
    static member (=>.)(p, BoolExpr q) = mkImplies (mkBool p) q
    static member (=.)(BoolExpr p, BoolExpr q) = mkEquiv p q
    static member (=.)(BoolExpr p, q) = mkEquiv p (mkBool q)
    static member (=.)(p, BoolExpr q) = mkEquiv (mkBool p) q
    static member Distinct xs = Array.map (fun (BoolExpr expr) -> expr :> Expr) xs |> mkDistinct
    static member If(BoolExpr b, BoolExpr expr1, BoolExpr expr2) = mkITE b expr1 expr2

/// Return a bool const with supplied name
let Bool(s: string) =
    let context = getContext()
    context.MkBoolConst s |> BoolExpr

let True = mkTrue()
let False = mkFalse()
let And (args : seq<Bool>) = Seq.reduce (&&.) args
let Or (args : seq<Bool>) = Seq.reduce (||.) args
let Implies (arg1: Bool, arg2: Bool) = arg1 =>. arg2
let Not (arg: Bool) = !. arg

let inline If (b: Bool, expr1: ^T, expr2: ^T) =
    (^T : (static member If : Bool * ^T * ^T -> Bool) (b, expr1, expr2))

    
type Z3 =
    static member Solve ([<ParamArray>] xs: _ []) =
        let solver = getContext().MkSolver()
        for (BoolExpr expr) in xs do
            solver.Assert expr
        let result = solver.Check()
        if result = Status.SATISFIABLE then
            let m = solver.Model
            printfn "["
            m.Decls
            |> Seq.map (fun d -> sprintf " %O = %O" d.Name m.[d])
            |> fun s -> String.Join(",\n", s)
            |> printfn "%s"
            printfn "]"
        else printfn "No solution"
        result

let toBool b = if b then True else False

type BitVec(expr: BitVecExpr) =
    inherit Theory()
    override x.Expr = expr :> Expr
    override x.ToString() = sprintf "%O" expr
    static member FromExpr (e: Expr) = BitVec(e :?> BitVecExpr)

let BitVecExpr expr = BitVec(expr)
let (|BitVecExpr|) (bv: BitVec) = bv.Expr :?> BitVecExpr

[<AutoOpen>]
module internal BitVecUtils =
    let inline mkBitVec (v: int) (size: uint32) = getContext().MkBV(v, size)
    let inline add x y = getContext().MkBVAdd(x, y) |> BitVecExpr
    let inline eq x y = getContext().MkEq(x, y) |> BoolExpr
    let inline ge x y = getContext().MkBVSGE(x, y) |> BoolExpr
    let inline ueq x y = getContext().MkDistinct(x, y) |> BoolExpr
    let inline le x y = getContext().MkBVSLE(x, y) |> BoolExpr

type BitVec with
    static member (+)(BitVecExpr x, BitVecExpr y) = add x y
    static member (+)(BitVecExpr x, y) = add x (mkBitVec y x.SortSize)
    static member (+)(x, BitVecExpr y) = add (mkBitVec x y.SortSize) y
    static member (=.)(BitVecExpr x, BitVecExpr y) = eq x y
    static member (=.)(BitVecExpr x, y) = eq x (mkBitVec y x.SortSize)
    static member (=.)(x, BitVecExpr y) = eq (mkBitVec x y.SortSize) y
    static member (>=.)(BitVecExpr x, BitVecExpr y) = ge x y
    static member (>=.)(BitVecExpr x, y) = ge x (mkBitVec y x.SortSize)
    static member (>=.)(x, BitVecExpr y) = ge (mkBitVec x y.SortSize) y
    static member (<>.)(BitVecExpr x, BitVecExpr y) = ueq x y
    static member (<>.)(BitVecExpr x, y) = ueq x (mkBitVec y x.SortSize)
    static member (<>.)(x, BitVecExpr y) = ueq (mkBitVec x y.SortSize) y
    static member (<=.)(BitVecExpr x, BitVecExpr y) = le x y
    static member (<=.)(BitVecExpr x, y) = le x (mkBitVec y x.SortSize)
    static member (<=.)(x, BitVecExpr y) = le (mkBitVec x y.SortSize) y

let BitVec(name: string, size: uint32) =
    let context = getContext()
    context.MkBVConst(name, size) |> BitVecExpr