namespace BoomScript;

public sealed class Interpreter
{
    private readonly SourceText _sourceText;

    private Interpreter(SourceText sourceText) => _sourceText = sourceText;
    
    public static int Evaluate(SourceText sourceText) => 
        new Interpreter(sourceText).Evaluate();
    
    private int Evaluate()
    {
        var expressions = Parser.Parse(_sourceText);
        var environment = Environment.Default();
        return expressions.Aggregate(0, (_, expression) => Evaluate(expression, environment));
    }

    private int Evaluate(Expression expression, Environment environment)
    {
        switch (expression)
        {
            case AssignmentExpression assignmentExpression:
                var value = Evaluate(assignmentExpression.Value, environment);
                environment[assignmentExpression.Identifier] = value;
                return value;
            case BinaryExpression binaryExpression:
                var left = Evaluate(binaryExpression.Left, environment);
                var right = Evaluate(binaryExpression.Right, environment);
                switch (binaryExpression.Operator.Kind)
                {
                    case TokenKind.Plus:
                        return left + right;
                    case TokenKind.Star:
                        return left * right;
                    default:
                        throw new ArgumentOutOfRangeException();
                }
            case IntegerExpression integerExpression:
                return integerExpression.Value;
            case NameExpression nameExpression:
                return environment[nameExpression.Identifier];
            default:
                throw new ArgumentOutOfRangeException(nameof(expression));
        }
    }

    private record Environment(Dictionary<string, int> Variables, Environment? Parent)
    {
        public static Environment Default() => new(new(), null);

        public int this[string index]
        {
            get => Variables[index];
            set => Variables[index] = value;
        }
    }
}