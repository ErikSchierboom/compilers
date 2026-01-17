// ----------------------------------------------------------------------------
// 05 - A few more functions and operators
// ----------------------------------------------------------------------------
module TinyBASIC

open System

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  | Clear
  | Poke of Expression * Expression * Expression
  // NOTE: Input("X") reads a number from console and assigns it to X;
  // Stop terminates the program; I also modified Print to take a list of
  // expressions instead of just one (which is what C64 supports too).
  | Print of Expression list
  | Input of string 
  | Stop

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value> 
    Random : System.Random }

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
  | Function("MIN", [e1; e2]) ->
    let v1 = evalExpression state e1
    let v2 = evalExpression state e2
    binaryNumOp min [v1; v2]
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

  | Print(expressions) ->
      expressions
      |> List.map (evalExpression state)
      |> List.iter printValue

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
      Console.Clear()
      runNextLine state line
  | Poke (x, y, e) ->
    let v1 = evalExpression state x
    let v2 = evalExpression state y
    let v3 = evalExpression state e
    match v1, v2, v3 with
    | NumberValue x, NumberValue y, StringValue s ->
      Console.SetCursorPosition(x, y)
      Console.Write(s)
      runNextLine state line
    | _ -> failwith "invalid arguments for POKE"

  | Input x ->
      match Int32.TryParse(Console.ReadLine()) with
      | true, n ->
        let newVariables = state.Variables |> Map.add x (NumberValue n)
        runNextLine { state with Variables = newVariables } line
      | false, _ ->
        runCommand state (line, cmd)
  | Stop -> state

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

let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []; Variables = Map.empty; Random = System.Random() }

// NOTE: A simple game you should be able to run now! :-)
let nim = 
  [ Some 10, Assign("M", num 20)
    Some 20, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 30, Print [ str "PLAYER 1: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 40, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 50, Input("P")
    Some 60, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 40)
    Some 70, Assign("M", var "M" .- var "P")
    Some 80, If(var "M" .= num 0, Goto 200)
    Some 90, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 100, Print [ str "PLAYER 2: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 110, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 120, Input("P")
    Some 130, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 110)
    Some 140, Assign("M", var "M" .- var "P")
    Some 150, If(var "M" .= num 0, Goto 220)
    Some 160, Goto 20
    Some 200, Print [str "PLAYER 1 WINS!"]
    Some 210, Stop
    Some 220, Print [str "PLAYER 2 WINS!"]
    Some 230, Stop
    None, Run
  ]

runInputs empty nim |> ignore
