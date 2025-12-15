// ----------------------------------------------------------------------------
// 06 - Add more data types - unions
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
  | ValCase of bool * Value

and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression

and VariableContext = 
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
      | _ -> failwith "invalid argument of binary operator"
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
      | _ -> failwith "unary operator applied to non-numeric value"
    | If(c, t, f) ->
      let v = evaluate ctx c
      match v with
      | ValNum 1 -> evaluate ctx t
      | ValNum _ -> evaluate ctx f
      | _ -> failwith "if condition is non-numeric value"
  | Lambda(v, e) ->
      ValClosure(v, e, ctx)
  | Application(e1, e2) ->
      let v1 = evaluate ctx e1
      match v1 with
      | ValClosure(v, e, ctx') ->
        evaluate (Map.add v (evaluate ctx e2) ctx') e
      | _ -> failwith "application applied to non-function value"
  | Let(v, e1, e2) ->
    evaluate (Map.add v (evaluate ctx e1) ctx) e2

  | Tuple(e1, e2) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      ValTuple(v1, v2)
  | TupleGet(b, e) ->
      let v = evaluate ctx e
      match v with
      | ValTuple(v1, v2) -> if b then v1 else v2
      | _ -> failwith "tuple get applied to non-tuple value"

  | Match(e, v, e1, e2) ->
      let ve = evaluate ctx e
      match ve with
      | ValCase(b, v') -> if b then evaluate (Map.add v v' ctx) e1 else evaluate (Map.add v v' ctx) e2
      | _ -> failwith "match applied to non-union value"
  | Case(b, e) ->
      let v = evaluate ctx e
      ValCase(b, v)

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Data types - creating a union value
let ec1 =
  Case(true, Binary("*", Constant(21), Constant(2)))
evaluate Map.empty ec1

// Data types - working with union cases
//   match Case1(21) with Case1(x) -> x*2 | Case2(x) -> x*100
//   match Case2(21) with Case1(x) -> x*2 | Case2(x) -> x*100
let ec2 = 
  Match(Case(true, Constant(21)), "x", 
    Binary("*", Variable("x"), Constant(2)),
    Binary("*", Variable("x"), Constant(100))
  )
evaluate Map.empty ec2

let ec3 = 
  Match(Case(false, Constant(21)), "x", 
    Binary("*", Variable("x"), Constant(2)),
    Binary("*", Variable("x"), Constant(100))
  )
evaluate Map.empty ec3
