// ----------------------------------------------------------------------------
// 07 - Generating magic squares in TinyProlog
// ----------------------------------------------------------------------------

type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list
  // NOTE: Added 'Call' as a special kind of predicate
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
  // TODO: Add a case for 'Call' to substitution 
  failwith "implemented in step 2"

let substituteSubst (newSubst:Map<string, Term>) (subst:list<string * Term>) = 
  failwith "implemented in step 2"

let substituteTerms subst (terms:list<Term>) = 
  failwith "implemented in step 2"

let rec unifyLists l1 l2 = 
  failwith "implemented in steps 1 and 2"

and unify t1 t2 = 
  // TODO: This is where we need a clever trick to handle 'Call'!
  // Unification can succeed if we have a predicate and a call with a
  // corresponding predicate as the first argument. So we can unify:
  //
  //   Predicate(p1, args1) ~ Call(Predicate(p2, args2a), args2b)
  //
  // When 'p1 = p2' and when we can unify 'args1 ~ args2a @ args2b'.
  failwith "implemented in step 1"

// ----------------------------------------------------------------------------
// Pretty printing terms
// ----------------------------------------------------------------------------

let rec (|Number|_|) term = 
  failwith "implemented in step 5"

let rec (|List|_|) term : option<list<Term>> = 
  failwith "implemented in step 6"

let rec formatTerm term = 
  // TODO: You can format 'Call(args)' as 'Predicate("call", args)'
  failwith "implemented in step 5 and 6"

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber = 
  let mutable n = 0
  fun () -> n <- n + 1; n

let rec freeVariables term =  
  // TODO: Add a case for 'Call' when getting free variables
  failwith "implemented in step 3"

let withFreshVariables (clause:Clause) : Clause =
  failwith "implemented in step 3"

let query (program:list<Clause>) (query:Term) =
  failwith "implemented in step 3"


let rec solve program subst goals =
  failwith "implemented in steps 4 and 6"

let run program query = 
  failwith "implemented in step 6"

// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

let rec num n = failwith "implemented in step 5"

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
  failwith "implemented in step 6"

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