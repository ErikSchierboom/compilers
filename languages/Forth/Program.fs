open System

type Token =
    | Number of int
    | Word of string
    
type State = {
    Stack: int list
    Words: Map<string, State -> State>
}

let private stateOperation (op: int list -> int list) (state: State): State =
    { state with Stack = op state.Stack }

let private binaryMathOperation (op: int -> int -> int) (state: State): State =
    match state.Stack with
    | right :: left :: tail -> { state with Stack = op left right :: tail }
    | _ -> failwith "Invalid stack"

let private initialState = {
    Stack = []
    Words = Map.ofList [
        "+",    binaryMathOperation (+)
        "-",    binaryMathOperation (-)
        "*",    binaryMathOperation (*)
        "/",    binaryMathOperation (/)
        "mod",  binaryMathOperation (%)
        "swap", stateOperation (function | x :: y :: tail -> y :: x :: tail | _ -> failwith "Invalid stack")
        "over", stateOperation (function | x :: y :: tail -> y :: x :: y :: tail | _ -> failwith "Invalid stack")
        "rot",  stateOperation (function | x :: y :: z :: tail -> y :: z :: x :: tail | _ -> failwith "Invalid stack")
        "drop", stateOperation List.tail
    ]
}

let evaluate (input: string): State =
    let rec inner (position: int) (state: State) =
        if position >= input.Length then
            state
        else
            let c = input[position]

            if Char.IsWhiteSpace c then
                inner (position + 1) state
            elif Char.IsDigit c then
                let numberOfDigits = input[position..] |> Seq.takeWhile Char.IsDigit |> Seq.length
                let number = int input[position..position + numberOfDigits - 1]
                inner (position + numberOfDigits) { state with Stack = number :: state.Stack }
            else
                let numberOfChars = input[position..] |> Seq.takeWhile (not << Char.IsWhiteSpace) |> Seq.length
                let word = input[position..position + numberOfChars - 1]
                match state.Words.TryFind word with
                | Some f -> inner (position + numberOfChars) (f state)
                | None -> failwith $"Unknown word: {word}"
                
    inner 0 initialState
                
let tokens = evaluate "1 2 + 3 * 4 swap 7 rot"
printfn $"%A{tokens}"
