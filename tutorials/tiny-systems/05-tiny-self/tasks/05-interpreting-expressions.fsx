// ----------------------------------------------------------------------------
// 05 - Representing and interpreting expressions
// ----------------------------------------------------------------------------

type Slot = 
  { Name : string
    Contents : Objekt
    IsParent : bool } 

and Objekt = 
  { mutable Slots : Slot list 
    mutable Code : Objekt option
    mutable Special : Special option }

and Special = 
  | String of string
  | Native of (Objekt -> Objekt)

#load "objekt-visualizer.fs"
open TinySelf

// ----------------------------------------------------------------------------
// Helpers for creating things that we will often need
// ----------------------------------------------------------------------------

let makeCodeObject slots code = 
  { Code = Some code; Special = None; Slots = slots }
let makeObject slots = 
  { Code = None; Special = None; Slots = slots }
let makeSpecialObject special = 
  { Code = None; Special = Some special; Slots = [] }

let makeSlot (n:string) contents = 
  if n.EndsWith("*") then failwith "Non-parent slot names should not end with '*'."
  { Name = n; Contents = contents; IsParent = false }
let makeParentSlot (n:string) contents = 
  if not (n.EndsWith("*")) then failwith "Parent slot names should end with '*'."
  { Name = n; Contents = contents; IsParent = true }

let makeNativeMethod f =
  makeCodeObject [] (makeSpecialObject (Native(f)))

// ----------------------------------------------------------------------------
// Lookup and message sending
// ----------------------------------------------------------------------------

let rec lookup (msg:string) (obj:Objekt) : list<Objekt * Slot> = 
  failwith "TODO - implemented in step 3"

let eval (slotValue:Objekt) (args:Objekt) (instance:Objekt) : Objekt =
  failwith "TODO - implemented in step 3"

let send (msg:string) (args:Objekt) (instance:Objekt) : Objekt =
  failwith "TODO - implemented in step 3"

// ----------------------------------------------------------------------------
// Helpers for testing & object construction
// ----------------------------------------------------------------------------

let empty : Objekt = failwith "TODO - implemented in step 2"
let getStringValue (obj:Objekt) : string = failwith "TODO - implemented in step 2"
let printCode : Objekt = failwith "TODO - implemented in step 2"

// ----------------------------------------------------------------------------
// Assignment slots
// ----------------------------------------------------------------------------

let assignmentMethod n = makeNativeMethod (fun arcd -> 
  failwith "TODO: implemented in step 3" )

let makeAssignmentSlot n = 
  { Name = n + ":"; Contents = assignmentMethod n; IsParent = false }

// ----------------------------------------------------------------------------
// Primitive types - Booleans, strings and blocks
// ----------------------------------------------------------------------------

let makeBoolean b : Objekt = failwith "TODO - implemented in step 4"
let trueObj = makeBoolean true
let falseObj = makeBoolean false

let equalsCode = makeNativeMethod (fun arcd -> 
  failwith "TODO - implemented in step 4")

let rec appendCode = makeNativeMethod (fun arcd -> 
  failwith "TODO - implemented in step 3")

and stringPrototype = makeObject [
  makeSlot "print" printCode  
  makeSlot "append" appendCode  
  makeSlot "equals" equalsCode 
]
and makeString s = 
  makeObject [ 
    makeSlot "value" (makeSpecialObject (String s)) 
    makeParentSlot "string*" stringPrototype
  ]

let makeBlock f = makeObject [ 
  makeSlot "run" (makeNativeMethod (fun _ -> f()))
]

// ----------------------------------------------------------------------------
// Representing and interpreting expressions
// ----------------------------------------------------------------------------

let exprSend (msg:string) (args:list<string * Objekt>) (target:Objekt) = 
  makeObject [
    makeSlot "exprkind" (makeString "send")
    // TODO: Add slots:
    // - 'message' - the msg string (turn that to Self string)
    // - 'target' - storing the target expression object
    // - 'args' - object with arguments stored as slots (using List.map)
  ]
let exprConst obj = 
  makeObject [
    makeSlot "exprkind" (makeString "const")
    // TODO: Add 'value' slot storing the constant value
  ]  
let exprBlock body = 
  makeObject [
    makeSlot "exprkind" (makeString "block")
    // TODO: Add 'body' slot storing the expression representing the body
  ]  


let rec evalExpr expr =
  let kind = expr |> send "exprkind" empty |> getStringValue
  match kind with 
  | "const" -> 
      // Get the value by sending 'value' to 'expr' and return it!
      failwith "TODO - not implemented"
  | "block" ->
      // Create block using 'makeBlock'. When run, the block should
      // evaluate the 'body' of the expression recursively using 'evalExpr'
      // (to get the body, send a 'body' message to the 'expr')
      failwith "TODO - not implemented"
  | "send" ->
      // Get the name of the message by sending 'message' and using 'getStringValue'
      // Recursively evaluate the 'target' using 'evalExpr'
      // Recursively evaluate values of all arguments using 'evalExpr'
      // (this is a bit long - you need to List.map over slots of 'args')
      //
      // THEN: target |> send msg args
      //
      failwith "TODO - not implemented"
  | _ -> 
      failwithf "Unknown expression kind: %s" kind

// ----------------------------------------------------------------------------
// Tests - trivial hello world
// ----------------------------------------------------------------------------

let helloCode1 = 
  (exprConst (makeString "Hello "))
  |> exprSend "append" [ "other", exprConst (makeString "world!") ]
  |> exprSend "print" []

// Visualise object tree to a given depth. If there are too many
// objects and the window is small, this fails. Try setting the 
// limit to smaller number if this happens!
Vis.printObjectTreeLimit 3 helloCode1
helloCode1 |> evalExpr |> ignore


// ----------------------------------------------------------------------------
// Prisoner's dilemma
// ----------------------------------------------------------------------------

let betray = makeString "betray"
let coop = makeString "cooperate"

let rnd = System.Random()
let player1 = if rnd.Next(2) = 0 then betray else coop
let player2 = if rnd.Next(2) = 0 then betray else coop


let code = 
  // TODO: Reimplement the code to evaluate Prisoner's dilemma rules
  // using the new representation. This should use only 'exprConst'
  // 'exprSend' and 'exprBlock'!
  failwith "TODO - not implemented"
  
Vis.printObjectTreeLimit 3 code
evalExpr code
