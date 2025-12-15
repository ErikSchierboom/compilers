// Mutable type definition. We do
// not need to make everything mutable.
type Objekt = 
  { mutable Slots : Slot list }

and Slot = 
  { Contents : Objekt 
    IsParent : bool 
    Name : string }

// Helpers to help us add slots to objects
let addSlot name contents obj = 
  let ns = 
    { Contents = contents; Name = name; 
      IsParent = false }
  obj.Slots <- ns::obj.Slots

let addParentSlot name contents obj = 
  let ns = 
    { Contents = contents; Name = name; 
      IsParent = true }
  obj.Slots <- ns::obj.Slots

// Example of working with slots and objects
let empty = { Slots = [] }
let cat = { Slots = [] }
let name = { Slots = [] }
let cheshire = { Slots = [] }

// If you run this repeatedly, it will add 
// multiple copies of the same slot!
cheshire |> addSlot "name" name
cheshire |> addParentSlot "parent*" cat

let parents = 
  cheshire.Slots 
  |> List.filter (fun s -> s.IsParent)

match parents with 
| [ { Contents = o } ] -> printfn "Parent: %A" o
| _ -> ()