// ----------------------------------------------------------------------------
// 06 - Adding 'new', 'self' and 'method' expressions
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
  failwith "TODO - implemented in step 5"
let exprConst (obj:Objekt) : Objekt = 
  failwith "TODO - implemented in step 5"
let exprBlock (body:Objekt) : Objekt = 
  failwith "TODO - implemented in step 5"

let exprString (s:string) : Objekt = 
  // TODO: Implement helper for string literal expressions. This is just 
  // creating a constant (exprConst) from the given string value.
  failwith "TODO - not implemented"


let exprSelf : Objekt = 
  // TODO: Add new type of expression! This represents access to the 
  // object itself (self / this). See notes below in 'evalExpr'.
  failwith "TODO - not implemented"

let exprMethod (body:Objekt) : Objekt = 
  // TODO: Add new type of expression! This represents a 
  // method object. See notes below in 'evalExpr'.
  failwith "TODO - not implemented"

let exprNew (slots:list<string * Objekt>) : Objekt = 
  // TODO: Add a new type of expression! This represents creation of an
  // object with a given list of slots and expressions for the slots.
  //
  // NOTE #1: You will need to store the expressions of slots somehow.
  // This can be done by creating an object that stores them as slots
  // (or by adding them to the current object as slots after 'exprkind')
  //
  // NOTE #2: Later, we will want to recognize 'name*' and 'name:' syntax
  // for creating parent slots and assignment slots. But now, we just need
  // to store the name (so that we can create parent/assignment slot in 
  // 'evalExpr'). As 'makeSlot' checks that the name does *not* end with '*',
  // you need some kind of escaping. E.g. do '.Replace("*","_ASTERISK_")' 
  // here when storing the expression and then reverse the replace in 'evalExpr'.
  failwith "TODO - not implemented"


let rec evalExpr (arcd:Objekt) (expr:Objekt) : Objekt =
  // TODO: We added 'arcd' (activation record) argument to the 'evalExpr'
  // function, because we need this when evaluating the 'self' expression!
  // 
  // You need to handle three new kinds of expressions:
  //
  // * 'self' - This should return the receiver from the activation record
  //   (review the 'eval' implementation where the activation record is created)
  //
  // * 'new' - This is a bit like the handling of 'send'. You need to recursively
  //   evaluate all the expressions that we want to assign as slots to the 
  //   object. Then, based on the name, create the right kind of slot!
  //   We want to support 'name*' for parent slots and 'name:'. For this,
  //   you need to create both normal slot (name) and assignment slot (name:)
  //
  // * 'method' - This is very similar to the 'block' case, but you need to
  //   return method using 'makeNativeMethod'. The method takes a different 
  //   activation record than the one passed to 'evalExpr'! This way it can
  //   access the object in which it exists (not the acrd of the code that
  //   defined it...)
  failwith "TODO - not implemented"

let makeSelfMethod expr = 
  // NOTE: Here we now call 'evalExpr' with 'arcd' as argument!
  makeNativeMethod (fun arcd -> evalExpr arcd expr)


// ----------------------------------------------------------------------------
// Tests - trivial hello world (same as before)
// ----------------------------------------------------------------------------

let helloCode1 = 
  exprString "Hello "
  |> exprSend "append" [ "other", exprString "world!" ]
  |> exprSend "print" []

Vis.printObjectTreeLimit 3 helloCode1
helloCode1 |> evalExpr empty |> ignore


let helloCode2 = 
  exprString "hello"
  |> exprSend "append" [ "other", exprString " " ]
  |> exprSend "append" [ "other", exprString "world" ]
  |> exprSend "append" [ "other", exprString "!" ]
  |> exprSend "print" []

//Vis.printObjectTreeLimit 3 helloCode2
helloCode2 |> evalExpr empty |> ignore


// ----------------------------------------------------------------------------
// Tests - over-engineered prototype-based Hello world
// ----------------------------------------------------------------------------

// This is the same demo as in Step 3, but now the entire demo is written
// as Self expressions that we later evaluate using 'evalExpr'. In other
// words, all the code below just constructs Self program that, when run,
// creates the appropriate Self objects!

let greeter = exprNew [
  "greeting", exprString "Hello"
  "greet", exprMethod (
    let who = exprSend "who" [] exprSelf
    let sp, ex = exprString " ", exprString "!"
    exprSend "greeting" [] exprSelf
    |> exprSend "append" ["other", sp]
    |> exprSend "append" ["other", who]
    |> exprSend "append" ["other", ex]
    |> exprSend "print" []
  ) ]

let helloWorld = exprNew [
  "greeter*", greeter 
  "who:", exprString "world"
]

let helloMatfyz = exprNew [
  "greeter*", greeter
  "who", exprString "Matfyz"
]

// Create the Self objects and send them the right messages
// Use 'Vis' to make sure that you get the correct objects!

let greeterObj = greeter |> evalExpr empty
let helloWorldObj = helloWorld |> evalExpr empty
let helloMatfyzObj = helloMatfyz |> evalExpr empty

Vis.printObjectTreeLimit 2 helloWorldObj
Vis.printObjectTreeLimit 2 helloMatfyzObj
Vis.printObjectTreeLimit 2 greeterObj

helloWorldObj |> send "greet" empty |> ignore
helloMatfyzObj |> send "greet" empty |> ignore

// This works because 'helloWorld' has an assignment slot
helloWorldObj |> send "who:" (makeObject [ makeSlot "new" (makeString "svete") ]) 
// This does not work because 'helloMatfyz' has only regular slot
helloMatfyzObj |> send "who:" (makeObject [ makeSlot "new" (makeString "CVUT") ]) 
