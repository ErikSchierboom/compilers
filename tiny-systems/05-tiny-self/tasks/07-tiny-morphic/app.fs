module TinySelf.App
open Browser.Dom
open TinySelf.Core

// ----------------------------------------------------------------------------
// Helpers for creating HTML elements using DOM
// ----------------------------------------------------------------------------

let html tag attrs children =
  let res = document.createElement(tag)
  for f in attrs do f res
  for c in children do console.log(c); res.appendChild(c) |> ignore
  res :> Browser.Types.Node

let attr k v (el:Browser.Types.HTMLElement) = 
  el.setAttribute(k, v)

let click f (el:Browser.Types.HTMLElement) =
    el.addEventListener("click", fun _ -> f())

let tool ico f =
  html "a" [ attr "href" "javascript:;"; attr "class" "tool"; 
    click (fun () -> f ())] [ html "i" [attr "class" ("fa " + ico) ] [] ]    

let text s =
  document.createTextNode(s) : Browser.Types.Node

// ----------------------------------------------------------------------------
// Rendering of objects. If the object has 'render' method, it can be 
// rendered using 'renderHtml' - the 'render' method returns objects
// of a known structure (with 'tag' and 'children' slots)
// For other objects, we render them using 'renderList' which is just
// a list of slots. The 'render' function decides what to use.
// ----------------------------------------------------------------------------

// Mutable list of objects displayed on the screen
// For each we store a bool representing whether we
// want to force the raw view (even if there is a visualizer)
let mutable objects : list<bool * Objekt> = []

let (|Lookup|) slot obj = lookup slot obj 

let rec renderHtml (obj:Objekt) = 
  match obj with
  | Lookup "value" [ _, { Contents = { Special = Some(String s) } } ] -> 
      text s
  | Lookup "tag" [_, { Contents = tag }] ->
    let children = 
      match obj with 
      | Lookup "children" [_, { Contents = children }] -> children
      | _ -> empty 
    let attrs = 
      match obj with 
      | Lookup "attrs" [_, { Contents = attrs }] -> attrs
      | _ -> empty 
    html (getStringValue tag) 
      [ for a in attrs.Slots -> attr a.Name (getStringValue a.Contents) ] 
      [ for c in children.Slots do
          if not c.IsParent then yield renderHtml c.Contents ]
  | o -> 
    console.log(o)
    failwithf "renderHtml: wrong HTML object"  

let rec renderList obj = 
  html "ul" [] [
    for s in obj.Slots ->
      html "li" [] [
        html "a" [ 
          attr "href" "javascript:;"
          click (fun () -> openObject false s.Contents)] [ text s.Name ]
      ]
    match obj.Code with 
    | Some code ->
        yield html "li" [] [
          html "a" [
            attr "href" "javascript:;"
            click (fun () -> openObject false code)] [ text "(code)" ]
        ]
    | _ -> ()
  ]

and render i raw (obj:Objekt) = 
  html "div" [attr "class" "box"] [
    let close = tool "fa-xmark" (fun () -> closeObject i)
    let browseRaw = tool "fa-search" (fun () -> openObject true obj)
    match obj with 
    | Lookup "render" [_] when not raw -> 
        yield html "div" [attr "class" "header"] [ close; browseRaw ]
        yield html "div" [attr "class" "body"] [ renderHtml (obj |> send "render" empty) ]
    | _ -> 
        yield html "div" [attr "class" "header"] [ close ]
        yield html "div" [attr "class" "body"] [ renderList obj ]
    ]

// ----------------------------------------------------------------------------
// When we open/close object, we just remove all DOM and recreate everything
// (which is a bit wasteful but works without dependencies...)
// ----------------------------------------------------------------------------
          
and update () = 
  let el = document.getElementById("out")
  while el.children.length > 0 do el.removeChild(el.children.[0]) |> ignore
  for i, (raw, o) in Seq.indexed objects do 
    printfn "rendering %A" (i, raw, o)
    render i raw o |> el.appendChild |> ignore

and openObject raw o = 
  objects <- objects @ [ raw, o ]
  update () 

and closeObject i = 
  objects <- 
    [ for j, o in Seq.indexed objects do
        if i <> j then yield o ]
  update ()

// Open some demo objects!
printfn "Is it on??"
openObject false demo