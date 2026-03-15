open System

type Token =
    | Number of int
    | Word of string

let scan (input: string): Token list =
    let rec inner (position: int) (tokens: Token list) =
        if position >= input.Length then
            List.rev tokens
        else
            let c = input[position]

            if Char.IsWhiteSpace c then
                inner (position + 1) tokens
            elif Char.IsDigit c then
                let numberOfDigits = input[position..] |> Seq.takeWhile Char.IsDigit |> Seq.length
                let number = Number (int input[position..position + numberOfDigits - 1])
                inner (position + numberOfDigits) (number :: tokens)
            else
                let numberOfChars = input[position..] |> Seq.takeWhile (not << Char.IsWhiteSpace) |> Seq.length
                let word = Word (input[position..position + numberOfChars - 1])
                inner (position + numberOfChars) (word :: tokens)
                
    inner 0 []
                
let tokens = scan "123 abc + mod"
printfn $"%A{tokens}"
