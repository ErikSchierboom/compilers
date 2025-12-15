// ----------------------------------------------------------------------------
// 07 - Generating magic squares in TinyProlog
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
  failwith "implemented in step 2"

let substituteSubst (newSubst:Map<string, Term>) (subst:list<string * Term>) = 
  failwith "implemented in step 2"

let substituteTerms subst (terms:list<Term>) = 
  failwith "implemented in step 2"

let rec unifyLists l1 l2 = 
  failwith "implemented in steps 1 and 2"

and unify t1 t2 = 
  failwith "implemented in step 1"

// ----------------------------------------------------------------------------
// Pretty printing terms
// ----------------------------------------------------------------------------

let rec (|Number|_|) term = 
  failwith "implemented in step 5"

let rec (|List|_|) term : option<list<Term>> = 
  failwith "implemented in step 6"

let rec formatTerm term = 
  failwith "implemented in step 5 and 6"

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber = 
  let mutable n = 0
  fun () -> n <- n + 1; n

let rec freeVariables term = 
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

let permutation =
  append @ [
    fact (Predicate("perm", [ Atom("empty"); Atom("empty") ]))
    rule (Predicate("perm", [ Variable("L"); Predicate("cons", [Variable("H"); Variable("T")]) ])) [
      Predicate("append", [ Variable("V"); Predicate("cons", [Variable("H"); Variable("U")]); Variable("L") ])
      Predicate("append", [ Variable("V"); Variable("U"); Variable("W") ])
      Predicate("perm", [ Variable("W"); Variable("T") ])
    ]
  ]

// DEMO: Generate all permutations of the list [1 .. 4]
run permutation (Predicate("perm", [l1to4; Variable("X")]))


// ----------------------------------------------------------------------------
// Generating magic squares
// ----------------------------------------------------------------------------

// Custom operator and a hlper function for equality & defining variables
let (.=.) a b = Predicate("eq", [a; b])
let var x = Variable(x)

// TinyProlog is too slow! But if we give it the numbers in an order
// that is close to being a magic square (first row is correct), it will 
// manage to generate a magic square sooner or later...
let l = [ 2;7;6; 1;3;4; 5;8;9 ]

let magic = permutation @ nums @ [
  rule (Predicate("add3", [ var "A"; var "B"; var "C"; var "S" ])) [
    Predicate("add", [ var "A"; var "B"; var "T" ])
    Predicate("add", [ var "T"; var "C"; var "S" ])
  ]
  rule (Predicate("magic", [ var "S"; var "X" ])) [
    yield Predicate("perm", [makeList [ for i in l -> num i ]; var "X"])
    yield var "X" .=. makeList [ var "A1"; var "A2"; var "A3"; var "B1"; 
      var "B2"; var "B3"; var "C1"; var "C2"; var "C3" ]    
    for a, b, c in [ 
      ("A1","A2","A3"); ("B1","B2","B3"); ("C1","C2","C3") 
      ("A1","B1","C1"); ("A2","B2","C2"); ("A3","B3","C3")
      ("A1","B2","C3"); ("A3","B2","C1") ] do
      yield Predicate("add3", [var a; var b; var c; var "S"]) 
  ]
]

run magic (Predicate("magic", [num 15; var "X"]))
