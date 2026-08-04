namespace Arya;

public class Interpreter
{
    private readonly List<Expression> _expressions;

    private Interpreter(List<Expression> expressions) => _expressions = expressions;

    public static Array? Evaluate(string code)
    {
        var expressions = Parser.Parse(code);
        return new Interpreter(expressions).Evaluate();
    }

    private Array? Evaluate()
    {
        Array? result = null;
        
        foreach (var expression in _expressions)
            result = Evaluate(expression);

        return result;
    }

    private Array Evaluate(Expression expression)
    {
        switch (expression)
        {
            case ArrayLiteralExpression arrayLiteral:
                var elements = arrayLiteral.Elements.Select(Evaluate).ToArray();
                return Array.Merge(elements);
            case BinaryExpression binary:
                var left = Evaluate(binary.Left);
                var right = Evaluate(binary.Right);
                
                switch (binary.Operator.Type)
                {
                    case TokenType.Plus: return left.Add(right);
                    case TokenType.Star: return left.Multiply(right);
                    case TokenType.PlusPlus: return left.Append(right);
                    default:
                        throw new ArgumentOutOfRangeException(nameof(binary.Operator.Type));
                }
            case LiteralExpression literal:
                // TODO: check for type
                return new Array((int)literal.Value.Literal!);
            case ParenthesizedExpression parenthesized:
                return Evaluate(parenthesized.Expression);
            default:
                throw new ArgumentOutOfRangeException(nameof(expression));
        }
    }
}
