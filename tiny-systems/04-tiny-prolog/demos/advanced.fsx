// ACTIVE PATTERNS

// Odd/even complete pattern and tick/tock printing
// Square partial pattern and add to printing
// Squares recursive partial list pattern 
// SNIPPET: Simple numerical expressions
// Operator pattern and collect all constants

let (|Odd|Even|) n = 
  if n % 2 = 0 then Even n else Odd n

let (|Square|_|) n =
  let iroot = int(sqrt(float n))
  if iroot * iroot = n then Some iroot else None

let rec (|Squares|_|) l = 
  match l with 
  | [] -> Some([])
  | (Square h)::(Squares tl) -> Some(h::tl)
  | _ -> None

for i in 1 .. 10 do 
  match i with 
  | Square n -> printfn "square %d!!" n
  | Odd _ -> printfn "tick"
  | Even _ -> printfn "tock"

match [2;4;9;16;25;36] with 
| Squares _ -> printfn "all squares!"
| _ -> printfn "just numbers...."

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Unary of string * Expression

let e = 
  Binary("+", Unary("-", Constant(4)),
    Unary("-", Binary("*", Unary("-", 
      Constant(3)), Constant(2))))

let (|Operator|_|) e = 
  match e with 
  | Binary(_, e1, e2) -> Some([e1; e2])
  | Unary(_, e) -> Some([e])
  | Constant _ -> None

let rec constants e =
  match e with 
  | Constant n -> [n]
  | Operator es -> List.collect constants es

constants e 


// SEQUENCES

// Generate squares from 1 to 10
// Filter 1 to 10 to find just Squares
// Generate squares from 1 to infinity
// Modify the above to use list/array
let squaresS = 
  seq { for i in 0 .. 10 -> i * i }

let squaresL = 
  [ for i in 0 .. 10 -> i * i ]

let squaresSE = seq { 
  for i in 0 .. 10 do
    match i with 
    | Square n -> yield n 
    | _ -> () }

let rec numbers n = seq { 
  yield n 
  yield! numbers (n + 1)
}

numbers 0
numbers 0 |> Seq.map (fun n -> n * n)






