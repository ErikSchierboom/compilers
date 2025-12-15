// ----------------------------------------------------------------------------
// 02 - Implement interactive program editing
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string

type Expression = 
  | Const of Value

type Command = 
  | Print of Expression
  | Run 
  | Goto of int

type State = 
  { Program : list<int * Command> }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value = 
  match value with
  | StringValue s -> printfn $"%s{s}"

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

let rec evalExpression expr =
  match expr with
  | Const value -> value

let rec runCommand state (line, cmd) =
  match cmd with 
  | Run ->
      let first = List.head state.Program    
      runCommand state first

  | Print(expr) ->
      let value = evalExpression expr
      printValue value
      runNextLine state line
  | Goto(line) ->
      let cmd = getLine state line
      runCommand state (line, cmd)

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

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let empty = { Program = [] }


runInputs empty helloOnce |> ignore
runInputs empty helloInf |> ignore
