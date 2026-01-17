// ----------------------------------------------------------------------------
// 04 - Random function and (not quite correct) POKE
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Print of Expression
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  // NOTE: Clear clears the screen and Poke(x, y, e) puts a string 'e' at 
  // the console location (x, y). In C64, the actual POKE writes to a given
  // memory location, but we only use it for screen access here.
  | Clear
  | Poke of Expression * Expression * Expression

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value>
    Random: System.Random
    }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value = 
  match value with
  | StringValue s -> printfn $"%s{s}"
  | NumberValue n -> printfn $"%i{n}"
  | BoolValue b -> printfn $"{b}"

let getLine state line =
  match state.Program |> List.tryFind (fun (n, _) -> n = line) with
  | Some (_, cmd) -> cmd
  | None -> failwith "line not found"

let addLine state (line, cmd) =
  List.partition (fun (n, _) -> n < line) state.Program
  |> fun (before, after) ->
    match after with
    | (after_line,_)::rest when after_line = line -> { state with Program = (before @ [(line, cmd)] @ rest) }
    | _ -> { state with Program = (before @ [(line, cmd)] @ after) }

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let binaryRelOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let binaryNumOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> NumberValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let binaryBoolOp f args = 
  match args with 
  | [BoolValue a; BoolValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two boolean arguments"

let rec evalExpression state expr = 
  match expr with
  | Const v -> v
  | Function("-", [e1; e2]) ->
    let v1 = evalExpression state e1
    let v2 = evalExpression state e2
    binaryNumOp (-) [v1; v2]
  | Function("=", [e1; e2]) ->
    let v1 = evalExpression state e1
    let v2 = evalExpression state e2
    binaryRelOp (=) [v1; v2]
  | Function("<", [e1; e2]) ->
    let v1 = evalExpression state e1
    let v2 = evalExpression state e2
    binaryRelOp (<) [v1; v2]
  | Function(">", [e1; e2]) ->
    let v1 = evalExpression state e1
    let v2 = evalExpression state e2
    binaryRelOp (>) [v1; v2]
  | Function("||", [e1; e2]) ->
    let v1 = evalExpression state e1
    let v2 = evalExpression state e2
    binaryBoolOp (||) [v1; v2]
  | Function("RND", [e]) ->
    let v = evalExpression state e
    match v with
    | NumberValue n -> NumberValue(state.Random.Next(n))
    | _ -> failwith "expected a numerical argument to RND"
  | Variable v ->
      match Map.tryFind v state.Variables with
      | Some value -> value
      | None -> failwith $"undefined variable: %s{v}"
  | _ -> failwith "invalid expression"

let rec runCommand state (line, cmd) =
  match cmd with 
  | Run ->
      let first = List.head state.Program    
      runCommand state first

  | Print(expr) ->
      let value = evalExpression state expr
      printValue value
      runNextLine state line
  | Goto(line) ->
      let cmd = getLine state line
      runCommand state (line, cmd)
      
  | Assign(v, e) ->
    let newState = { state with Variables = Map.add v (evalExpression state e) state.Variables }
    runNextLine newState line
    
  | If(e, cmd) ->
    let cond = evalExpression state e
    match cond with
    | BoolValue true -> runCommand state (line, cmd)
    | BoolValue false -> runNextLine state line
    | _ -> failwith "invalid condition"
  
  | Clear ->
      System.Console.Clear()
      runNextLine state line
  | Poke (x, y, e) ->
    let v1 = evalExpression state x
    let v2 = evalExpression state y
    let v3 = evalExpression state e
    match v1, v2, v3 with
    | NumberValue x, NumberValue y, StringValue s ->
      System.Console.SetCursorPosition(x, y)
      System.Console.Write(s)
      runNextLine state line
    | _ -> failwith "invalid arguments for POKE"

and runNextLine state line = 
  match state.Program |> List.tryFind (fun (n, _) -> n > line) with
  | Some (next_line, next_cmd) -> runCommand state (next_line, next_cmd)
  | None -> state

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) =
  match line with
  | Some ln -> addLine state (ln, cmd)
  | None -> runCommand state (System.Int32.MaxValue, cmd)

let runInputs state cmds =  
  List.fold runInput state cmds

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// NOTE: Writing all the BASIC expressions is quite tedious, so this is a 
// very basic (and terribly elegant) trick to make our task a bit easier.
// We define a couple of shortcuts and custom operators to construct expressions.
// With these, we can write e.g.: 
//  'Function("RND", [Const(NumberValue 100)])' as '"RND" @ [num 100]' or 
//  'Function("-", [Variable("I"); Const(NumberValue 1)])' as 'var "I" .- num 1'
let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []; Variables = Map.empty; Random = System.Random.Shared }

// NOTE: Random stars generation. This has hard-coded max width and height (60x20)
// but you could use 'System.Console.WindowWidth'/'Height' here to make it nicer.
let stars = 
  [ Some 10, Clear
    Some 20, Poke("RND" @ [num System.Console.WindowWidth], "RND" @ [num System.Console.WindowHeight], str "*")
    Some 30, Assign("I", num 100)
    Some 40, Poke("RND" @ [num System.Console.WindowWidth], "RND" @ [num System.Console.WindowHeight], str " ")
    Some 50, Assign("I", var "I" .- num 1)
    Some 60, If(var "I" .> num 1, Goto(40)) 
    Some 100, Goto(20)
    None, Run
  ]

// NOTE: Make the cursor invisible to get a nicer stars animation
System.Console.CursorVisible <- false
runInputs empty stars |> ignore
