// ----------------------------------------------------------------------------
// 04 - Generating and solving goals recursively
// ----------------------------------------------------------------------------

type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list

type Clause =
  { Head : Term
    Body : Term list }

type Program = Clause list

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------

let rec substitute (subst:Map<string, Term>) term =
  match term with
  | Atom t
  | Variable t ->
      Map.tryFind t subst |> Option.defaultValue term
  | Predicate (p, terms) -> 
      let newTerms = terms |> List.map (substitute subst)
      Predicate (p, newTerms)

let substituteSubst (newSubst:Map<string, Term>) (subst:list<string * Term>) = 
  subst
  |> List.map (fun (v, t) -> (v, substitute newSubst t))

let substituteTerms (subst:Map<string, Term>) (terms:list<Term>) = 
  terms |> List.map (substitute subst)

let rec unifyLists l1 l2 =
  match l1, l2 with 
  | [], [] -> 
      Some []
  | h1::t1, h2::t2 ->
      match unify h1 h2 with
      | Some s1 ->
        match unifyLists (substituteTerms (Map.ofList s1) t1) (substituteTerms (Map.ofList s1) t2) with
        | Some s2 -> 
            let s1' = substituteSubst (Map.ofList s2) s1
            Some (s1' @ s2)
        | None -> None 
      | _ ->  None
  | _ -> 
    None

and unify t1 t2 : option<list<string * Term>> = 
  match t1, t2 with 
  | Atom a1, Atom a2 when a1 = a2 -> Some []
  | Predicate (p1, t1), Predicate (p2, t2) when p1 = p2 -> unifyLists t1 t2
  | Variable v, term
  | term, Variable v -> Some [(v, term)]
  | _ -> None

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber = 
  let mutable n = 0
  fun () -> n <- n + 1; n

let rec freeVariables term =
  match term with
  | Atom _ -> []
  | Variable v -> [v]
  | Predicate (_, terms) -> List.collect freeVariables terms

let withFreshVariables (clause:Clause) : Clause =
  let freshVariables =
       (clause.Head::clause.Body)
       |> List.collect freeVariables 
       |> List.distinct
       |> List.map (fun v -> (v, Variable (v + string (nextNumber()))))
       |> Map.ofList
  
  { Head = substitute freshVariables clause.Head
    Body = substituteTerms freshVariables clause.Body }

let query (program:list<Clause>) (query:Term) : list<Clause * list<string * Term>> =
      program
      |> List.choose (fun clause ->
          let freshClause = withFreshVariables clause
          match unify freshClause.Head query with
          | Some subst -> Some (freshClause, subst)
          | None -> None)

let rec solve program subst goals = 
  match goals with 
  | g::goals -> 
      // TODO: We need to solve the goal (term) 'g'. To do so, find all 
      // matching clauses in the 'program' using 'query' and iterate over
      // the returned list using 'for clause, newSubst in matches do'.
      // For each possible solution, we need to add the 'clause.Body' to 
      // the list of 'goals' and apply the substitution 'newSubst' to the
      // new concatentated list of 'goals'. Then we need to apply the 
      // substitution 'newSubst' to the substitution 'subst' we have so far,
      // append the two and call 'solve' recursively with this new substitution
      // to solve the new goals.
      let matches = query program g
      for clause, newSubst in matches do
        let newGoals = goals @ clause.Body
        let substitutedGoals = substituteTerms (Map.ofList newSubst) newGoals
        let substitutedSubst = substituteSubst (Map.ofList newSubst) subst @ newSubst
        solve program substitutedSubst substitutedGoals 

  | [] -> 
    // TODO: We solved all goals, which means 'subst' is a possible solution!
    // Print 'subst' (either using printfn "%A" or in some nicer way).
    printfn $"%A{subst}"

// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

// Some information about the British royal family 
let family = [ 
  fact (Predicate("male", [Atom("William")]))
  fact (Predicate("female", [Atom("Diana")]))
  fact (Predicate("male", [Atom("Charles")]))
  fact (Predicate("male", [Atom("George")]))
  fact (Predicate("parent", [Atom("Diana"); Atom("William")]))
  fact (Predicate("parent", [Atom("Charles"); Atom("William")]))
  fact (Predicate("parent", [Atom("William"); Atom("George")]))
  rule (Predicate("father", [Variable("X"); Variable("Y")])) [
    Predicate("parent", [Variable("X"); Variable("Y")])
    Predicate("male", [Variable("X")])
  ]
]

// Query: father(X, William)
// Result #1: [ X -> Charles, ... ]
solve family [] [ Predicate("father", [Variable("X"); Atom("William")]) ]

// Query: father(X, Y)
// Result #1: [ X -> Charles, Y -> William, ... ]
// Result #2: [ X -> William, Y -> George, ... ]
solve family [] [ Predicate("father", [Variable("X"); Variable("Y")]) ]
