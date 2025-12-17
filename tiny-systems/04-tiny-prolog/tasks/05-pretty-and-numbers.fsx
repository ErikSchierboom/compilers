// ----------------------------------------------------------------------------
// 05 - Pretty printing & adding numbers to TinyProlog
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
// Pretty printing terms
// ----------------------------------------------------------------------------

let rec (|Number|_|) term = 
  match term with
  | Atom "zero" -> Some 0
  | Predicate ("succ", [nterm]) ->
    match nterm with
    | Number n -> Some (n + 1)
    | _ -> None
  | _ -> None

let rec formatTerm term = 
  match term with 
  // Simple cases for number, atom and variable are done already...
  | Number n -> string n
  | Atom s -> s
  | Variable v -> v
  | Predicate(p, items) ->
      items
      |> List.map formatTerm
      |> String.concat " "

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
    for var, term in subst do
      printfn $"%s{var} = %s{formatTerm term}"

// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

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

// Queries from previous step (now with readable output)
solve family [] [ Predicate("father", [Variable("X"); Atom("William")]) ]
solve family [] [ Predicate("father", [Variable("X"); Variable("Y")]) ]


// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

// Helper that generates a term representing a number
let rec num n =
  match n with
  | 0 -> Atom "zero"
  | _ -> Predicate ("succ", [num (n - 1)]) 

// Addition and equality testing for Peano arithmetic
// $ add(zero, X, X)
// $ add(succ(X), Y, succ(Z)) :- add(X, Y, Z)
// $ eq(X, X)
let nums = [
  fact (Predicate("add", [Atom("zero"); Variable("X"); Variable("X")]))
  rule (Predicate("add", [Predicate("succ", [ Variable("X") ]); Variable("Y"); Predicate("succ", [ Variable("Z")]) ])) [
    Predicate("add", [Variable("X"); Variable("Y"); Variable("Z")])
  ]
  fact (Predicate("eq", [Variable("X"); Variable("X")]))
]


// Query: add(2, 3, X)
// Output should include: 'X = 5' 
//   (and other variables resulting from recursive calls)
solve nums [] [ Predicate("add", [num 2; num 3; Variable("X")]) ]

// Query: add(2, X, 5)
// Output should include: 'X = 3' 
//   (we can use 'add' to calculate subtraction too!)
solve nums [] [ Predicate("add", [num 2; Variable("X"); num 5]) ]

// Query: add(2, Y, X)
// Output should include: 'Y = Z??' and 'X = succ(succ(Z??))' 
//   (with some number for ?? - indicating that this can be any term)
solve nums [] [ Predicate("add", [num 2; Variable("Y"); Variable("X")]) ]
