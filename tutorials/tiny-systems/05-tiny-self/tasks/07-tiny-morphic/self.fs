module TinySelf.Core
// ----------------------------------------------------------------------------
// 07 - Implementing a Tiny Morphic framework
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
  failwith "TODO - implemented in step 6"
let exprSelf : Objekt = 
  failwith "TODO - implemented in step 6"
let exprMethod (body:Objekt) : Objekt = 
  failwith "TODO - implemented in step 6"
let exprNew (slots:list<string * Objekt>) : Objekt = 
  failwith "TODO - implemented in step 6"

let rec evalExpr (arcd:Objekt) (expr:Objekt) : Objekt =
  failwith "TODO - implemented in step 6"

let makeSelfMethod expr = 
  makeNativeMethod (fun arcd -> evalExpr arcd expr)


// ----------------------------------------------------------------------------
// Creating some web-based visualizers!
// ----------------------------------------------------------------------------

// NOTE: Add 'render' method for string - it is rendered as just string
// (We mutate the prototype so that we can do it here, at the end of the file)
stringPrototype.Slots <- 
  makeSlot "render" (makeSelfMethod exprSelf)
  :: stringPrototype.Slots


// NOTE: Add 'render' method for animals - they produce more complex HTML
let cat = makeObject [
  makeSlot "sound" (makeString "Meow")
  makeSlot "say" (makeSelfMethod (
    exprSelf  |> exprSend "sound" [] |> exprSend "print" []
  ))
  makeSlot "render" (makeSelfMethod (
    exprNew [
      "tag", exprString "div"
      "children", exprNew [
        "1", exprNew [ 
          "tag", exprString "h3" 
          "children", exprNew [ "1", exprSelf |> exprSend "name" [] ]
        ]
        // TODO: Modify the rendering code for cats!
        // This should also display the 'sound' that the cat makes
        // and you can display the cat picture by constructing 
        // <img src='...'> using the URL from the 'image' slot.
      ]
    ]
  ))
]

// Some sample cats
let larry = makeObject [ 
  makeParentSlot "parent*" cat
  makeSlot "name" (makeString "Larry")
  makeSlot "image" (makeString "img/larry.jpg")
]
let cheshire = makeObject [ 
  makeParentSlot "parent*" cat
  makeSlot "sound" (makeString "We are all mad!")
  makeSlot "name" (makeString "Cheshire cat")
  makeSlot "image" (makeString "img/cheshire.jpg")
]
let mog = makeObject [ 
  makeParentSlot "parent*" cat
  makeSlot "name" (makeString "Mog")
  makeSlot "image" (makeString "img/mog.jpg")
]

let animals = makeObject [
  makeSlot "larry" larry
  makeSlot "cheshire" cheshire
  makeSlot "mog" mog
]


// Another demo - just a simple TinySelf hello world object 
let greeterPrototype = makeObject [
  makeSlot "greet" (makeSelfMethod (
    exprSelf |> exprSend "greeting" [] |> exprSend "print" []
  ))
]

let greeter = makeObject [
  makeParentSlot "parent*" greeterPrototype
  makeSlot "greeting" (makeString "Hello world!")
]


// Demo object that provides access to other things 
let demo = makeObject [
  makeSlot "animals" animals
  makeSlot "greeter" greeter
]

