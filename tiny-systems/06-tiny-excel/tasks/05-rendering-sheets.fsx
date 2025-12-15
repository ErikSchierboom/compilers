// ----------------------------------------------------------------------------
// 05 - Rendering sheets as HTML
// ----------------------------------------------------------------------------

type Address = int * int

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

type LiveSheet = Map<Address, CellNode>

// ----------------------------------------------------------------------------
// Reactive evaluation and graph construction
// ----------------------------------------------------------------------------

let rec eval (sheet:LiveSheet) expr = 
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

let rec relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) (srcExpr:Expr) = 
  failwith "implemented in step 2"

let expand (srcCol, srcRow) (tgtCol, tgtRow) (sheet:LiveSheet) : LiveSheet = 
  failwith "implemented in step 2 and 3"

// ----------------------------------------------------------------------------
// Rendering sheets as HTML
// ----------------------------------------------------------------------------

open System.IO
open System.Diagnostics

let displayValue (v:Value) : string =
  // TODO: Turn the given value into a string representing HTML
  // You can use the following to create an error string in red.
  "<span class='e'>not implemented</span>"
  
let display (sheet:LiveSheet) = 
  // TODO: Find the greates row and column index
  let maxCol = 10 
  let maxRow = 10 

  let f = Path.GetTempFileName() + ".html"
  use wr = new StreamWriter(File.OpenWrite(f))
  wr.Write("""<html><head>
      <style>
        * { font-family:sans-serif; margin:0px; padding:0px; border-spacing:0; } 
        th, td { border:1px solid black; border-collapse:collapse; padding:4px 10px 4px 10px }
        body { padding:50px } .e { color: red; } 
        th { background:#606060; color:white; } 
      </style>
    </head><body><table>""")

  // TODO: Write column headings
  wr.Write("<tr><th></th>")
  for col in 1 .. maxCol do 
    wr.Write("<th> ?? </th>")
  wr.Write("</tr>")

  // TODO: Write row headings and data
  for row in 1 .. maxRow do 
    wr.Write($"<tr><th> ?? </th>")
    for col in 1 .. maxCol do 
      wr.Write("<td> !! </td>")
    wr.Write("</tr>")
  wr.Write("</table></body></html>")
  wr.Close()
  Process.Start(f)


// ----------------------------------------------------------------------------
// Helpers and test cases
// ----------------------------------------------------------------------------

let addr (s:string) = 
  failwith "implemented in step 1"

// NOTE: Let's visualize the Fibbonacci spreadsheet from Step 2!
let fib =  
  [ addr "A1", Const(Number 0) 
    addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A1"); Reference(addr "A2")]) ]
  |> makeSheet
  |> expand (addr "A3") (addr "A10")
display fib

// NOTE: Let's visualize the Factorial spreadsheet from Step 2!
let fac = 
  [ addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A2"); Const(Number 1)])
    addr "B1", Const(Number 1)
    addr "B2", Function("*", [Reference(addr "A2"); Reference(addr "B1")]) ] 
  |> makeSheet
  |> expand (addr "A3") (addr "A11")
  |> expand (addr "B2") (addr "B11")
display fac

// NOTE: Let's visualize the Temp convertor spreadsheet from Step 4! 
let tempConv = 
  [ addr "A1", Const(String "F to C")
    addr "B1", Const(Number 0) 
    addr "C1", 
      Function("/", [ 
        Function("*", [ 
          Function("-", [ Reference(addr "B1"); Const(Number 32) ])
          Const(Number 5) ])
        Const(Number 9) ]) 
    addr "A2", Const(String "C to F")
    addr "B2", Const(Number 0) 
    addr "C2", Const(Error "implemented in step 4") ]
  |> makeSheet
display tempConv
