// ----------------------------------------------------------------------------
// 07 - Adding range selection and array values
// ----------------------------------------------------------------------------

type Location = Fixed of int | Normal of int
type RawAddress = int * int
type Address = Location * Location

type Value = 
  | Number of int
  | String of string
  | Error of string
  // NOTE: Range can be evaluated to a value, which is an array of values
  | Array of Value list

type Expr = 
  | Const of Value
  | Reference of Address
  | Function of string * Expr list
  // NOTE: Range specifies a region as two corners of a rectangle
  | Range of Address * Address

type CellNode = 
  { mutable Value : Value
    mutable Expr : Expr
    Updated : Event<unit> } 

type LiveSheet = Map<RawAddress, CellNode>

// ----------------------------------------------------------------------------
// Reactive evaluation and graph construction
// ----------------------------------------------------------------------------

let rec eval (sheet:LiveSheet) expr = 
  // TODO: This needs to be modified in two ways.
  //
  // * Handle the 'Range' case. You will need to extract the raw
  //   addresses of the two corners and iterate over all cells in the range.
  //   You can then use 'List.tryMapAll' (from the lecture) to get all the
  //   values. If they are all there, return 'Array', otherwise 'Error'.
  //
  // * Add 'SUM' function that takes one argument which is 'Array'
  //   (defined by range) and sums all elements of the array. This only
  //   works if they are all numerical. Use 'List.tryMapAll' here too!
  //
  failwith "implemented in step 1 and 3"

let rec collectReferences expr = 
  failwith "implemented in step 4"

let makeNode (sheet:LiveSheet) expr = 
  failwith "implemented in step 3 and 4"

let updateNode addr (sheet:LiveSheet) expr = 
  failwith "implemented in step 4"

let makeSheet list = 
  failwith "implemented in step 3"

// ----------------------------------------------------------------------------
// Drag down expansion
// ----------------------------------------------------------------------------

let relocateLocation (loc:Location) (by:int) : Location = 
  failwith "implemented in step 6"

let rec relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) (srcExpr:Expr) = 
  // TODO: The relocation needs to be applied to Ranges too!
  // (Relocate the corners of the range using the same mechanism
  // that you are using to relocate the references.)
  failwith "implemented in step 2 and 6"

let expand (srcCol, srcRow) (tgtCol, tgtRow) (sheet:LiveSheet) : LiveSheet = 
  failwith "implemented in step 2 and 3"

// ----------------------------------------------------------------------------
// Rendering sheets as HTML
// ----------------------------------------------------------------------------

open System.IO
open System.Diagnostics

let displayValue (v:Value) : string =
  failwith "implemented in step 5"
  
let display (sheet:LiveSheet) = 
  failwith "implemented in step 5"

// ----------------------------------------------------------------------------
// Helpers and continents demo
// ----------------------------------------------------------------------------

let raddr (s:string) = 
  failwith "implemented in step 1"

let addr (s:string) = 
  failwith "implemented in step 6"


let continents = 
  [ "Asia", 4753079, 31033; 
    "Africa", 1460481, 29648; 
    "Europe", 740433, 22134; 
    "North America", 604182, 21330; 
    "South America", 439719, 17461; 
    "Australia/Oceania", 46004, 8486; 
    "Antarctica", 0, 13720 ]

let wsheet0 = 
  [ yield raddr "A1", Const(String "Continent")
    yield raddr "B1", Const(String "Population (thousands)")
    yield raddr "C1", Const(String "Area (thousands km^2)")
    for i, (cont, pop, area) in Seq.indexed continents do
      yield raddr $"A{i+2}", Const(String cont)
      yield raddr $"B{i+2}", Const(Number pop)
      yield raddr $"C{i+2}", Const(Number area)
    yield raddr "A9", Const(String "World")
    
    // NOTE: We can now use our new SUM function here!
    yield raddr "B9", Function("SUM", [ Range(addr "B2", addr "B8") ])
    yield raddr "C9", Function("SUM", [ Range(addr "C2", addr "C8") ])

    yield raddr "D1", Const(String "Population (%)")
    yield raddr "D2", Function("/", [ 
        Function("*", [ Reference(addr "B2"); Const(Number 100) ])
        Reference(addr "$B$9")
      ])
    yield raddr "E1", Const(String "Area (%)")
    yield raddr "E2", Function("/", [ 
        Function("*", [ Reference(addr "C2"); Const(Number 100) ])
        Reference(addr "$C$9")
      ])
    yield raddr "F1", Const(String "Density (pop/km^2)")
    yield raddr "F2", Function("/", [ Reference(addr "B2"); Reference(addr "C2") ])
  ]
  |> makeSheet

// Display the initial sheet
display wsheet0

// Now expand all the calculations
let wsheet = 
  wsheet0
  |> expand (raddr "D2") (raddr "D9")
  |> expand (raddr "E2") (raddr "E9")
  |> expand (raddr "F2") (raddr "F9")

// ...and display the resulting sheet!
display wsheet