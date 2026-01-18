// ----------------------------------------------------------------------------
// 04 - Reactive event-based computation
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
    // NOTE: Added event that will be triggered when the 
    // expression and value of the node is changed.
    Updated : Event<unit> } 

type LiveSheet = Map<Address, CellNode>

// ----------------------------------------------------------------------------
// Reactive evaluation and graph construction
// ----------------------------------------------------------------------------

let rec eval (sheet:LiveSheet) expr = 
  failwith "implemented in step 1 and 3"
  

let rec collectReferences (expr:Expr) : Address list = 
  // TODO: Collect the addresses of all references that appear in the 
  // expression 'expr'. This needs to call itself recursively for all
  // arguments of 'Function' and concatenate the returned lists.
  // HINT: This looks nice if you use 'List.collect'.
  failwith "not implemented"


let makeNode (sheet:LiveSheet) expr = 
  // TODO: Add handling of 'Update' events!
  //
  // * When creating a node, we need to create a new event and 
  //   set it as the 'Updated' event of the returned node.
  // * We then need to define 'update' function that will be triggered
  //   when any of the cells on which this one depends change. In the 
  //   function, re-evaluate the formula, set the new value and trigger
  //   our Updated event to notify other cells.
  // * Before returning, use 'collectReferences' to find all cells on which
  //   this one depends and add 'update' as the handler of their 
  //   'Updated' event
  //
  failwith "partly implemented in step 3"


let updateNode addr (sheet:LiveSheet) expr = 
  // TODO: For now, we ignore the fact that the new expression may have
  // different set of references than the one we are replacing. 
  // So, we can just get the node, set the new expression and value
  // and trigger the Updated event!
  failwith "not implemented"


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
// Helpers and test cases
// ----------------------------------------------------------------------------

let addr (s:string) = 
  failwith "implemented in step 1"

// Simple spreadsheet that performs conversion between Celsius and Fahrenheit
// To convert F to C, we put value in F into B1 and read the result in C1
// To convert C to F, we put value in C into B2 and read the result in C2
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
    // TODO: Add formula for Celsius to Fahrenheit conversion to 'C2'
    addr "C2", Const(Error "not implemented") ]
  |> makeSheet

// Fahrenheit to Celsius conversions

// Should return: -17
updateNode (addr "B1") tempConv (Const(Number 0))
eval tempConv (Reference(addr "C1"))
// Should return: 0
updateNode (addr "B1") tempConv (Const(Number 32))
eval tempConv (Reference(addr "C1"))
// Should return: 37
updateNode (addr "B1") tempConv (Const(Number 100))
eval tempConv (Reference(addr "C1"))

// Celsius to Fahrenheit conversions

// Should return: 32
updateNode (addr "B2") tempConv (Const(Number 0))
eval tempConv (Reference(addr "C2"))
// Should return: 212
updateNode (addr "B2") tempConv (Const(Number 100))
eval tempConv (Reference(addr "C2"))
// Should return: 100
updateNode (addr "B2") tempConv (Const(Number 38))
eval tempConv (Reference(addr "C2"))

