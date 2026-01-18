// ----------------------------------------------------------------------------
// 07 - Generating magic squares in TinyProlog
// ----------------------------------------------------------------------------

type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list
  | Call of Term * Term list

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
  | Call(term, terms) ->
      let newTerms = terms |> List.map (substitute subst)
      Call (substitute subst term, newTerms)

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

and unify t1 t2 = 
  // TODO: This is where we need a clever trick to handle 'Call'!
  // Unification can succeed if we have a predicate and a call with a
  // corresponding predicate as the first argument. So we can unify:
  //
  //   Predicate(p1, args1) ~ Call(Predicate(p2, args2a), args2b)
  //
  // When 'p1 = p2' and when we can unify 'args1 ~ args2a @ args2b'.
  match t1, t2 with 
  | Atom a1, Atom a2 when a1 = a2 -> Some []
  | Predicate (p1, t1), Predicate (p2, t2) when p1 = p2 -> unifyLists t1 t2
  | Variable v, term
  | term, Variable v -> Some [(v, term)]
  | Predicate (p1, t1), Call (Predicate(p2, t2), t3)
  | Call (Predicate(p2, t2), t3), Predicate (p1, t1) when p1 = p2 ->
      unifyLists t1 (t2 @ t3)
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

let rec (|List|_|) term : option<list<Term>> = 
  match term with
  | Atom "empty" -> Some []
  | Predicate ("cons", [h; tl]) ->
    match tl with
    | List l -> Some (h::l)
    | _ -> None
  | _ -> None
  
let rec formatTerm term = 
  match term with 
  | Number n -> string n
  | List l -> "[" + (l |> List.map formatTerm |> String.concat ", ") + "]"
  | Atom s -> s
  | Variable v -> v
  | Predicate(_, items) ->
      items
      |> List.map formatTerm
      |> String.concat " "
  | Call (term, terms) ->
      "Predicate(\"call\", " + formatTerm term + "," + (terms |> List.map formatTerm |> String.concat ", ") + ")"

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
  | Call (term, terms) ->  freeVariables term @ List.collect freeVariables terms

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

let rec solve program subst goals : seq<list<string * Term>> = seq {
  match goals with 
  | g::goals -> 
      let matches = query program g
      for clause, newSubst in matches do
        let newGoals = goals @ clause.Body
        let substitutedGoals = substituteTerms (Map.ofList newSubst) newGoals
        let substitutedSubst = substituteSubst (Map.ofList newSubst) subst @ newSubst
        yield! solve program substitutedSubst substitutedGoals 
  | [] ->
    yield subst
}

let run program query = 
  let vars = Set.ofSeq (freeVariables query)
  for subst in solve program [] [query] do
    for var, term in subst do
      if vars.Contains(var) then
        printfn $"{var} = {formatTerm term}"

// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

let rec num n =
  match n with
  | 0 -> Atom "zero"
  | _ -> Predicate ("succ", [num (n - 1)])

let nums = [
  fact (Predicate("add", [Atom("zero"); Variable("X"); Variable("X")]))
  rule (Predicate("add", [Predicate("succ", [ Variable("X") ]); Variable("Y"); Predicate("succ", [ Variable("Z")]) ])) [
    Predicate("add", [Variable("X"); Variable("Y"); Variable("Z")])
  ]
  fact (Predicate("eq", [Variable("X"); Variable("X")]))
]


// ----------------------------------------------------------------------------
// Working with lists
// ----------------------------------------------------------------------------

let rec makeList l : Term =
  match l with
  | [] -> Atom "empty"
  | hd::tl -> Predicate ("cons", [hd; makeList tl])

let append = [ 
  fact (Predicate("append", [Atom("empty"); Variable("X"); Variable("X") ]))
  rule (Predicate("append", [
    Predicate("cons", [Variable("X"); Variable("Y") ])
    Variable("Z"); Predicate("cons", [Variable("X"); Variable("W") ])
  ])) [
    Predicate("append", [ Variable("Y"); Variable("Z"); Variable("W") ])
  ]
]

let l1to4 = makeList [ for i in 1 .. 4 -> num i ]
let l5to9 = makeList [ for i in 5 .. 9 -> num i ]
let l1to9 = makeList [ for i in 1 .. 9 -> num i ]

// ----------------------------------------------------------------------------
// Call and maplist
// ----------------------------------------------------------------------------

// The Prolog 'call' operation takes a term and a list of arguments
// and supplies the arguments as additional arguments to the term.
// So, for example, calling 'call(add(1), 2, X)' becomes 'add(1, 2, X)'
run nums (Call(Predicate("add", [num 1]), [num 2; Variable "X"]))
run nums (Call(Predicate("add", [num 1; Variable "X"]), [num 5]))

// This can be used to implement the 'maplist' function:
// $ maplist(_, [], []).
// $ maplist(G,[X|Xs],[Y|Ys]) :- maplist(G,Xs,Ys), call(G,X,Y).
let maplist = [
  fact (Predicate("maplist", [ Variable("_"); Atom("empty"); Atom("empty") ]))
  rule (Predicate("maplist", [ 
    Variable("G")
    Predicate("cons", [ Variable("X"); Variable("Xs") ])
    Predicate("cons", [ Variable("Y"); Variable("Ys") ]);  
  ])) [
    Predicate("maplist", [ Variable("G"); Variable("Xs"); Variable("Ys") ])
    Call(Variable("G"), [ Variable("X"); Variable("Y") ])
  ]
]

// Query: maplist(add(10), l1to9, Y)
// Returns: Y -> [11; 12; ..; 19]
run (nums @ maplist) (Predicate("maplist", 
  [ Predicate("add", [num 10]); l1to9; Variable("Y") ]))