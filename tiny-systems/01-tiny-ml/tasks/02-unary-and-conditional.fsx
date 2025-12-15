// ----------------------------------------------------------------------------
// 02 - Add unary operators (-) and conditional
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression

type VariableContext = 
  Map<string, Value>

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evaluate (ctx:VariableContext) e =
  match e with 
  | Constant n -> ValNum n
  | Binary(op, e1, e2) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      match v1, v2 with 
      | ValNum n1, ValNum n2 -> 
          match op with 
          | "+" -> ValNum(n1 + n2)
          | "*" -> ValNum(n1 * n2)
          | _ -> failwith "unsupported binary operator"
  | Variable(v) ->
      match ctx.TryFind v with 
      | Some res -> res
      | _ -> failwith ("unbound variable: " + v)
  | Unary(op, e) ->
      let v = evaluate ctx e
      match v with
      | ValNum n ->
        match op with
        | "-" -> ValNum(-n)
        | _ -> failwith "unsupported unary operator"
    | If(c, t, f) ->
      let v = evaluate ctx c
      match v with
      | ValNum 1 -> evaluate ctx t
      | _ -> evaluate ctx f

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Arithmetic with unary operator: (1*2) + (-(-20 * 2))
let euo = 
  Binary("+",
    Binary("*", Constant(1), Constant(2)),
    Unary("-", Binary("*", Constant(-20), Constant(2)))
  )
evaluate Map.empty euo

// Conditional expression: if 1 then 42 else 0
let eif1 = 
  If(Constant(1), 
    Constant(42), 
    Constant(0)
  )
evaluate Map.empty eif1

// Conditional expression: if 5+(-4) then 21*2 else 0
let eif2 = 
  If(Binary("+", Constant(5), Unary("-", Constant(4))), 
    Binary("*", Constant(21), Constant(2)), 
    Constant(0)
  )
evaluate Map.empty eif2

// Conditional expression: if 5+4 then 0 else 21*2 
let eif3 = 
  If(Binary("+", Constant(5), Constant(4)), 
    Constant(0),
    Binary("*", Constant(21), Constant(2))
  )
evaluate Map.empty eif3


