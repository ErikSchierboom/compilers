namespace Arya;

public class Interpreter
{
    private readonly List<Expression> _expressions;

    private Interpreter(List<Expression> expressions) => _expressions = expressions;

    public static Value? Evaluate(string code)
    {
        var expressions = Parser.Parse(code);
        return new Interpreter(expressions).Evaluate();
    }

    private Value? Evaluate()
    {
        Value? result = null;
        
        foreach (var expression in _expressions)
            result = Evaluate(expression);

        return result;
    }

    private Value Evaluate(Expression expression)
    {
        switch (expression)
        {
            case ArrayLiteralExpression arrayLiteral:
                var elements = arrayLiteral.Elements.Select(Evaluate).ToArray();
                return new Array(elements);
            case BinaryExpression binary:
                var left = Evaluate(binary.Left);
                var right = Evaluate(binary.Right);
                
                switch (binary.Operator.Type, left, right)
                {
                    case (TokenType.Plus, Integer l, Integer r): 
                        return new Integer(l.Value + r.Value);
                    case (TokenType.Plus, Integer l, Array r): 
                        return Array.BinaryOp(l, r, (li, ri) => li + ri);
                    case (TokenType.Plus, Array l, Integer r): 
                        return Array.BinaryOp(l, r, (li, ri) => li + ri);
                    case (TokenType.Plus, Array l, Array r): 
                        return Array.BinaryOp(l, r, (li, ri) => li + ri);
                    case (TokenType.Star, Integer l, Integer r): 
                        return new Integer(l.Value * r.Value);
                    case (TokenType.PlusPlus, Integer l, Integer r): 
                        return new Integer(l.Value + r.Value);
                    case (TokenType.PlusPlus, Array l, Array r): 
                        return Array.Append(l, r);
                    case (TokenType.PlusPlus, String l, String r): 
                        return new String(l.Value + r.Value);
                    case (TokenType.PlusPlus, Value l, Array r): 
                        return Array.Append(l, r);
                    case (TokenType.PlusPlus, Array l, Value r): 
                        return Array.Append(l, r);
                    default:
                        throw new ArgumentOutOfRangeException(nameof(binary.Operator.Type));
                }
            case LiteralExpression literal:
                return literal.Value.Type switch
                {
                    TokenType.String => new String((string)literal.Value.Literal!),
                    TokenType.Number => new Integer((int)literal.Value.Literal!),
                    _ => throw new ArgumentOutOfRangeException(nameof(literal.Value.Type))
                };
            case ParenthesizedExpression parenthesized:
                return Evaluate(parenthesized.Expression);
            default:
                throw new ArgumentOutOfRangeException(nameof(expression));
        }
    }
}
