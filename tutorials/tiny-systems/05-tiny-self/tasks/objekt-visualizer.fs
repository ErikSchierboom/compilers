module TinySelf.Vis

/// Represents a rectangular object to be drawn on screen
/// (The length of all lines should be Width and the
/// number of lines should be Height)
type private Sprite = 
  { Lines : string list
    Width : int
    Height : int }

/// We put all objects we draw into this cache so that we
/// can give them a unique stable number (index in the cache)
let private objektTable = ResizeArray<Objekt>()

/// Get a unique stable number for an object (and add 
/// it to the objektTable if it is not already there).
/// Note that we use Object.ReferenceEquals to make sure
/// each object is visualized (and also, structural equality
/// does not work on types that contain functions)
let private getObjektNumber obj = 
  let mutable found = -1
  for i in 0 .. objektTable.Count-1 do 
    if System.Object.ReferenceEquals(objektTable[i], obj) then found <- i
  if found = -1 then 
    objektTable.Add(obj)
    found <- objektTable.Count - 1
  found

/// Pretty print object and return a sprite with a table of 
/// its (i) number, (ii) slots, (iii) code and/or (iv) string value
let private formatObjekt obj =
  let objno = "#" + (getObjektNumber obj).ToString()
  let lines = obj.Slots |> List.map (fun s -> 
    sprintf "%s = #%d" s.Name (getObjektNumber s.Contents))
  let optCode = obj.Code |> Option.map (fun c -> sprintf "#code = %d" (getObjektNumber c))
  let optSpec = obj.Special |> Option.map (function String s -> sprintf "\"%s\"" s | Native _ -> "#func")
  let maxLen = List.map String.length (objno :: (Option.toList optCode) @ (Option.toList optSpec) @ lines) |> List.max
  let line = String.replicate maxLen "-"
  let formatted = 
    [ yield "+-" + line + "-+" 
      yield sprintf "| %s |" (objno.PadRight(maxLen))
      yield "+-" + line + "-+" 
      for l in lines -> "| " + l.PadRight(maxLen) + " |"
      yield "+-" + line + "-+" 
      if optCode.IsSome then
        yield "| " + optCode.Value.PadRight(maxLen) + " |"
        yield "+-" + line + "-+" 
      if optSpec.IsSome then
        yield "| " + optSpec.Value.PadRight(maxLen) + " |"
        yield "+-" + line + "-+" ]
  { Lines = formatted; Width = maxLen + 4; Height = formatted.Length }

/// Returns all objects reachable from the given object
let rec private getObjectTree depth obj = seq {
  yield obj
  if depth > 0 then
    for s in obj.Slots do yield! getObjectTree (depth-1) s.Contents
    if obj.Code.IsSome then yield! getObjectTree (depth-1) obj.Code.Value }

/// Position sprites in the terminal window from left top;
/// add them to the right for as long as they fit on screen
/// and then start a new row.
let private printSprites (sprites:seq<Sprite>) = 
  let sprites = ResizeArray<_>(sprites)
  let mutable left = 0
  let mutable top = 0
  let mutable currentHeight = 0
  System.Console.Clear()
  for sprite in sprites do
    if left + sprite.Width > System.Console.WindowWidth then
      left <- 0
      top <- top + currentHeight + 1
    for i, l in List.indexed sprite.Lines do
      System.Console.CursorLeft <- left
      System.Console.CursorTop <- top + i
      System.Console.Write(l)
    currentHeight <- max currentHeight sprite.Height
    left <- left + sprite.Width + 2
  System.Console.CursorLeft <- 0
  System.Console.CursorTop <- top + currentHeight + 1


/// Visualize all objects that can be reached from the given object.
let printObjectTree obj =
  getObjectTree 100000 obj 
  |> Seq.distinctBy getObjektNumber 
  |> Seq.map formatObjekt 
  |> printSprites

/// Visualize all objects that can be reached from the given object.
/// Set the maximum depth to a given number.
let printObjectTreeLimit n obj =
  getObjectTree n obj 
  |> Seq.distinctBy getObjektNumber 
  |> Seq.map formatObjekt 
  |> printSprites

/// Visualize all objects that can be reached from any of the given 
/// object. The given objects will be listed first.
let printObjectsTree objs =
  seq { yield! objs 
        for obj in objs do yield! getObjectTree 100000 obj }
  |> Seq.distinctBy getObjektNumber 
  |> Seq.map formatObjekt 
  |> printSprites
