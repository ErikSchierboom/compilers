// ----------------------------------------------------------------------------
// 03 - Add variables, conditionals and integer values
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

type VariableContext = Map<string, Value>

type State = 
  { Program : list<int * Command>;
    Variables: VariableContext
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

let rec evalExpression state expr =
  match expr with
  | Const v -> v
  | Function("-", [e1; e2]) ->
    let v1 = evalExpression state e1
    let v2 = evalExpression state e2
    match v1, v2 with
    | NumberValue n1, NumberValue n2 -> NumberValue (n1 - n2)
    | _ -> failwith "invalid arguments for '-' function"
  | Function("=", [e1; e2]) ->
    let v1 = evalExpression state e1
    let v2 = evalExpression state e2
    BoolValue (v1 = v2)
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

let empty = { Program = []; Variables = Map.empty }

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let testVariables = 
  [ Some 10, Assign("S", Const(StringValue "HELLO WORLD\n")) 
    Some 20, Assign("I", Const(NumberValue 1))
    Some 30, Assign("B", Function("=", [Variable("I"); Const(NumberValue 1)]))
    Some 40, Print(Variable "S") 
    Some 50, Print(Variable "I") 
    Some 60, Print(Variable "B")
    None, Run ]

// NOTE: Simpler test program without 'If" (just variables and '=' function) 
runInputs empty testVariables |> ignore

let helloTen = 
  [ Some 10, Assign("I", Const(NumberValue 10))
    Some 20, If(Function("=", [Variable("I"); Const(NumberValue 1)]), Goto(60))
    Some 30, Print (Const(StringValue "HELLO WORLD\n")) 
    Some 40, Assign("I", Function("-", [ Variable("I"); Const(NumberValue 1) ]))
    Some 50, Goto 20
    Some 60, Print (Const(StringValue "")) 
    None, Run ]

// NOTE: Prints hello world ten times using conditionals
runInputs empty helloTen |> ignore
