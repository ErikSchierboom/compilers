// ----------------------------------------------------------------------------
// 06 - Absolute and relative addresses
// ----------------------------------------------------------------------------

// NOTE: Location can be either fixed (absolute) or normal (relative)
// Address is used in 'Reference' and can be either. Raw address is
// actual location in a sheet and this remains just a pair of numbers.
type Location = Fixed of int | Normal of int
type RawAddress = int * int
type Address = Location * Location

type Value = 
  | Number of int
  | String of string
  | Error of string
  
type Expr = 
  | Const of Value
  | Reference of Address
  | Function of string * Expr list

type CellNode = 
  { mutable Value : Value
    mutable Expr : Expr
    Updated : Event<unit> } 

// NOTE: Sheet is now indexed by raw address
type LiveSheet = Map<RawAddress, CellNode>

// ----------------------------------------------------------------------------
// Reactive evaluation and graph construction
// ----------------------------------------------------------------------------

let rec eval (sheet:LiveSheet) expr = 
  failwith "implemented in step 1 and 3"

let rec collectReferences expr : list<RawAddress> = 
  // TODO: Modify the function to return a list of raw addresses!
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
  // TODO: Implement this helper which relocates only relative locations.
  // It makes updating relocateReferences easier!
  failwith "not implemented"


let rec relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) (srcExpr:Expr) = 
  // TODO: This needs to be updated to only relocate relative references!
  failwith "implemented in step 2"


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

let raddr (s:string) : RawAddress = 
  // TODO: Use the original parsing code here for parsing raw addresses
  failwith "implemented in step 1"


let addr (s:string) : Address = 
  // TODO: This is tricky to get right. See the test cases below.
  // You can use regex magic, or have a bunch of nested ifs - 
  // starting with a check if s.[0] = '$' etc. You could also convert
  // string to list using List.ofSeq and use pattern matching.
  failwith "not implemented"


addr "C10" = (Normal 3, Normal 10)
addr "$C10" = (Fixed 3, Normal 10)
addr "C$10" = (Normal 3, Fixed 10)
addr "$C$10" = (Fixed 3, Fixed 10)


let continents = 
  [ "Asia", 4753079, 31033; 
    "Africa", 1460481, 29648; 
    "Europe", 740433, 22134; 
    "North America", 604182, 21330; 
    "South America", 439719, 17461; 
    "Australia/Oceania", 46004, 8486; 
    "Antarctica", 0, 13720 ]

let wsheet0 = 
  [ // Column headers
    yield raddr "A1", Const(String "Continent")
    yield raddr "B1", Const(String "Population (thousands)")
    yield raddr "C1", Const(String "Area (thousands km^2)")
    
    // Fill rows of the data table
    for i, (cont, pop, area) in Seq.indexed continents do
      yield raddr $"A{i+2}", Const(String cont)
      yield raddr $"B{i+2}", Const(Number pop)
      yield raddr $"C{i+2}", Const(Number area)
    
    // Add summary row for the world
    yield raddr "A9", Const(String "World")
    yield raddr "B9", Const(Number 8043898) 
    yield raddr "C9", Const(Number 143812)

    // Add relative population
    yield raddr "D1", Const(String "Population (%)")
    yield raddr "D2", Function("/", [ 
        Function("*", [ Reference(addr "B2"); Const(Number 100) ])
        Reference(addr "$B$9")
      ])

    // Add relative area
    yield raddr "E1", Const(String "Area (%)")
    yield raddr "E2", Function("/", [ 
        Function("*", [ Reference(addr "C2"); Const(Number 100) ])
        Reference(addr "$C$9")
      ])

    // Add density of the region
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