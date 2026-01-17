// ----------------------------------------------------------------------------
// 03 - Type inference for binary operators and conditionals
// ----------------------------------------------------------------------------

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty =
  match ty with
  | TyVariable v -> v = vcheck
  | TyBool -> false
  | TyNumber -> false
  | TyList t -> occursCheck vcheck t

let rec substType (subst:Map<string, Type>) ty = 
  match ty with
  | TyVariable v -> subst |> Map.tryFind v |> Option.defaultValue ty
  | TyBool -> ty
  | TyNumber -> ty
  | TyList t -> TyList (substType subst t)

let substConstrs (subst:Map<string, Type>) (cs:list<Type * Type>) =
  cs |> List.map (fun (t1, t2) -> (substType subst t1, substType subst t2))
 
let rec solve cs =
  match cs with 
  | [] -> []
  | (TyNumber, TyNumber)::cs -> solve cs
  | (TyBool, TyBool)::cs -> solve cs
  | (TyList t1, TyList t2)::cs -> solve ((t1, t2)::cs)
  | (n, TyVariable v)::constraints 
  | (TyVariable v, n)::constraints ->
      if occursCheck v n then failwith "Cannot be solved (occurs check)"
      let constraints = substConstrs (Map.ofList [(v, n)]) constraints
      let subst = solve constraints
      let n = substType (subst |> Map.ofList) n 
      (v, n)::subst
    | _ -> failwith "Cannot be solved (type mismatch)"

// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

// Variable context to keep types of declared variables
// (those will typically be TyVariable cases, but don't have to)
type TypingContext = Map<string, Type>

let rec generate (ctx:TypingContext) e = 
  match e with 
  | Constant _ ->
      TyNumber, []

  | Binary("+", e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]

  | Binary("=", e1, e2) ->
      // TODO: Similar to the case for '+' but returns 'TyBool'
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyBool, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]

  | Binary(op, _, _) ->
      failwithf "Binary operator '%s' not supported." op

  | Variable v ->
      ctx[v], []

  | If(econd, etrue, efalse) ->
      let t1, s1 = generate ctx econd
      let t2, s2 = generate ctx etrue
      let t3, s3 = generate ctx efalse
      
      TyBool, s1 @ s2 @ s3 @ [ t1, TyBool; t2, t3 ]


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------


// Simple expressions: x = 10 + x
// Assuming x:'a, infers that 'a = int
let e1 = 
  Binary("=",   
    Variable("x"), 
    Binary("+", Constant(10), Variable("x")))

let t1, cs1 = 
  generate (Map.ofList ["x", TyVariable "a"]) e1

solve cs1

// Simple expressions: if x then 2 + 1 else y
// Assuming x:'a, y:'b, infers 'a = bool, 'b = int
let e2 = 
  If(Variable("x"), 
    Binary("+", Constant(2), Constant(1)),
    Variable("y"))

let t2, cs2 = 
  generate (Map.ofList ["x", TyVariable "a"; "y", TyVariable "b"]) e2

solve cs2

// Simple expressions: if x then 2 + 1 else x
// Cannot be solved, because 'x' used as 'int' and 'bool'
let e3 = 
  If(Variable("x"), 
    Binary("+", Constant(2), Constant(1)),
    Variable("x"))

let t3, cs3 = 
  generate (Map.ofList ["x", TyVariable "a"; "y", TyVariable "b"]) e3

solve cs3
