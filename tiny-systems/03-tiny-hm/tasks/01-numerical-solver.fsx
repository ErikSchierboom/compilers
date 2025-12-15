// ----------------------------------------------------------------------------
// 01 - Complete the simple numerical constraint solver
// ----------------------------------------------------------------------------

type Number =
  | Zero
  | Succ of Number
  | Variable of string

let rec occursCheck (v:string) (n:Number) = 
  match n with
  | Zero -> false
  | Succ n -> occursCheck v n
  | Variable v' -> v = v'

let rec substitute (v:string) (subst:Number) (n:Number) =
  match n with
  | Zero -> Zero
  | Succ n' -> Succ (substitute v subst n')
  | Variable v' when v' = v -> subst
  | Variable _ -> n

let substituteConstraints (v:string) (subst:Number) (constraints:list<Number * Number>) =
  constraints |> List.map (fun (n1, n2) -> (substitute v subst n1, substitute v subst n2))

let substituteAll (subst:list<string * Number>) (n:Number) =
  List.fold (fun n (v, n') -> substitute v n' n) n subst

let rec solve constraints = 
  match constraints with 
  | [] -> []
  | (Succ n1, Succ n2)::constraints ->
      solve ((n1, n2)::constraints)
  | (Zero, Zero)::constraints -> solve constraints
  | (Succ _, Zero)::_ | (Zero, Succ _)::_ -> 
      failwith "Cannot be solved"
  | (n, Variable v)::constraints 
  | (Variable v, n)::constraints ->
      if occursCheck v n then failwith "Cannot be solved (occurs check)"
      let constraints = substituteConstraints v n constraints
      let subst = solve constraints
      let n = substituteAll subst n
      (v, n)::subst

// Should work: x = Zero
solve 
  [ Succ(Variable "x"), Succ(Zero) ]

// Should faild: S(Z) <> Z
solve 
  [ Succ(Succ(Zero)), Succ(Zero) ]

// Should fail: No 'x' such that S(S(x)) = S(Z)
solve 
  [ Succ(Succ(Variable "x")), Succ(Zero) ]

// Not done: Need to substitute x/Z in S(x)
solve 
  [ Succ(Variable "x"), Succ(Zero)
    Variable "y", Succ(Variable "x") ]

// Not done: Need to substitute z/Z in S(S(z))
solve 
  [ Variable "x", Succ(Succ(Variable "z"))
    Succ(Variable "z"), Succ(Zero) ]

// Not done: Need occurs check
solve
  [ Variable "x", Succ(Variable "x") ]
