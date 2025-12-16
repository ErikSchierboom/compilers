// ----------------------------------------------------------------------------
// Adding simple data types
// ----------------------------------------------------------------------------

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression
  // NOTE: Added two types of expression for working with unions
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  | TyFunction of Type * Type
  | TyTuple of Type * Type
  | TyUnion of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty = 
  match ty with
  | TyVariable v -> v = vcheck
  | TyBool -> false
  | TyNumber -> false
  | TyList t -> occursCheck vcheck t
  | TyFunction(ta, tb) -> occursCheck vcheck ta || occursCheck vcheck tb
  | TyTuple(ta, tb) -> occursCheck vcheck ta || occursCheck vcheck tb
  | TyUnion(ta, tb) -> occursCheck vcheck ta || occursCheck vcheck tb

let rec substType (subst:Map<_, _>) ty = 
  match ty with
  | TyVariable v -> subst |> Map.tryFind v |> Option.defaultValue ty
  | TyBool -> ty
  | TyNumber -> ty
  | TyList t -> TyList (substType subst t)
  | TyFunction(ta, tb) -> TyFunction(substType subst ta, substType subst tb)
  | TyTuple(ta, tb) -> TyTuple(substType subst ta, substType subst tb)
  | TyUnion(ta, tb) -> TyUnion(substType subst ta, substType subst tb)

let substConstrs (subst:Map<string, Type>) (cs:list<Type * Type>) =
  cs |> List.map (fun (t1, t2) -> (substType subst t1, substType subst t2))
 
let rec solve constraints =
  match constraints with 
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
    | (TyFunction(ta1, tb1), TyFunction(ta2, tb2)):: constraints ->
      solve ((ta1, ta2)::(tb1, tb2)::constraints)
    | (TyTuple(ta1, tb1), TyTuple(ta2, tb2)):: constraints ->
      solve ((ta1, ta2)::(tb1, tb2)::constraints)
    | (TyUnion(ta1, tb1), TyUnion(ta2, tb2)):: constraints ->
      solve ((ta1, ta2)::(tb1, tb2)::constraints)
    | _ -> failwith "Cannot be solved (type mismatch)"


// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

let newTyVariable = 
  let mutable n = 0
  fun () -> n <- n + 1; TyVariable(sprintf "_a%d" n)

let rec generate (ctx:TypingContext) e = 
  match e with 
  | Constant _ ->
      TyNumber, []

  | Binary("+", e1, e2)
  | Binary("*", e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]      

  | Binary("=", e1, e2) ->
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

  | Let(v, e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate (Map.add v t1 ctx) e2
      t2, s1 @ s2
  
  | Lambda(v, e) ->
      let targ = newTyVariable()
      let t1, s1 = generate (Map.add v targ ctx) e
      TyFunction(targ, t1), s1

  | Application(e1, e2) ->
      let targ = newTyVariable()
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      targ, s1 @ s2 @ [ t1, TyFunction(t2, targ) ]

  | Tuple(e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyTuple(t1, t2), s1 @ s2

  | TupleGet(b, e) ->
      let targ1 = newTyVariable()
      let targ2 = newTyVariable()

      let t, s = generate ctx e
      if b then
        targ1, s @ [ t, TyTuple(targ1, targ2) ]
      else
        targ2, s @ [ t, TyTuple(targ1, targ2) ]

  | Match(e, v, e1, e2) ->
      let targ1 = newTyVariable()
      let targ2 = newTyVariable()
      
      let t, s = generate ctx e
      let t1, s1 = generate (Map.add v t ctx) e1
      let t2, s2 = generate (Map.add v t ctx) e2
      
      // TODO: As with tuples, we know the type of 'e' is some union,
      // but we do not know what. We need new type variables. When 
      // checking 'e1' and 'e2', add variable 'v' to the context!
      // Also note that the return types of 'e1' and 'e2' have to match.
      t1, s @ s1 @ s2 @ [ t, TyUnion(targ1, targ2); t1, t2 ]

  | Case(b, e) ->
      // TODO: Here, we know the type of 'e' is the type of one of 
      // the cases, but we still need a new type variable for the other.
      let targ = newTyVariable()
      
      let t, s = generate ctx e
      if b then
        TyUnion(t, targ), s
      else
        TyUnion(targ, t), s

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ

// Both cases are constrained because 'if' returns either one or the other
// * fun x -> if x = 0 then Case1(fun x -> x) else Case2(42)
Lambda("x", 
  If(Binary("=", Variable("x"), Constant(0)),
    Case(true, Lambda("x", Variable("x"))),
    Case(false, Constant(42))
  ))
|> infer

// No constraints to fix the second case type (case<number, 'a> -> number)
// * fun x -> match x with Case1 v -> v + 1 | Case2 _ -> 0 
Lambda("x", Match(Variable("x"), "v", 
  Binary("+", Variable("v"), Constant(1)),
  Constant(0)))
|> infer