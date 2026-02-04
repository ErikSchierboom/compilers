namespace BoomScript;

public sealed class Interpreter
{
    private readonly SourceText _sourceText;
    private readonly Expression[] _expressions;

    private Interpreter(SourceText sourceText)
    {
        _sourceText = sourceText;
        _expressions = Parser.Parse(_sourceText);
    }

    public static object? Evaluate(SourceText sourceText) => 
        new Interpreter(sourceText).Evaluate();
    
    private object? Evaluate()
    {   
        var environment = Environment.Default();
        
        object? returnValue = null;

        foreach (var expression in _expressions)
            returnValue = Evaluate(expression, environment);

        return returnValue;
    }

    private object Evaluate(Expression expression, Environment environment)
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
                        if (left is int plusLeftOperand && right is int plusRightOperand)
                            return plusLeftOperand + plusRightOperand;
                        
                        throw new InvalidOperationException("Operands must be integers.");
                    case TokenKind.Star:
                        if (left is int multiplyLeftOperand && right is int multiplyRightOperand)
                            return multiplyLeftOperand + multiplyRightOperand;
                        
                        throw new InvalidOperationException("Operands must be integers.");
                    case TokenKind.Greater:
                        if (left is int greaterLeftOperand && right is int greaterRightOperand)
                            return greaterLeftOperand > greaterRightOperand;
                        
                        throw new InvalidOperationException("Operands must be integers.");
                    case TokenKind.Less:
                        if (left is int lessLeftOperand && right is int lessRightOperand)
                            return lessLeftOperand < lessRightOperand;
                        
                        throw new InvalidOperationException("Operands must be integers.");
                    default:
                        throw new ArgumentOutOfRangeException();
                }
            case LiteralExpression literalExpression:
                return literalExpression.Value;
            case VariableExpression nameExpression:
                return environment[nameExpression.Identifier];
            default:
                throw new ArgumentOutOfRangeException(nameof(expression));
        }
    }

    private record Environment(Dictionary<string, object> Variables, Environment? Parent)
    {
        public static Environment Default() => new(new(), null);

        public object this[string index]
        {
            get => Variables[index];
            set => Variables[index] = value;
        }
    }
}