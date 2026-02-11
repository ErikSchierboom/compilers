namespace BoomScript;

public sealed class Interpreter
{
    private readonly BoundProgram _program;

    private Interpreter(BoundProgram program) => _program = program;

    public static object? Evaluate(BoundProgram program) => 
        new Interpreter(program).Evaluate();
    
    private object? Evaluate()
    {   
        var environment = Environment.Default();
        
        object? returnValue = null;

        foreach (var expression in _program.Expressions)
            returnValue = Evaluate(expression, environment);

        return returnValue;
    }

    private object Evaluate(BoundExpression expression, Environment environment)
    {
        switch (expression)
        {
            case BoundAssignmentExpression assignmentExpression:
                var value = Evaluate(assignmentExpression.Value, environment);
                environment[assignmentExpression.Identifier] = value;
                return value;
            case BoundBinaryExpression binaryExpression:
                var left = Evaluate(binaryExpression.Left, environment);
                var right = Evaluate(binaryExpression.Right, environment);
                return binaryExpression.Operator switch
                {
                    { Kind: BoundBinaryOperatorKind.Add, LeftType: BoundType.Int, RightType: BoundType.Int } => (int)left + (int)right,
                    { Kind: BoundBinaryOperatorKind.Mul, LeftType: BoundType.Int, RightType: BoundType.Int } => (int)left * (int)right,
                    { Kind: BoundBinaryOperatorKind.Greater, LeftType: BoundType.Int, RightType: BoundType.Int } => (int)left > (int)right,
                    { Kind: BoundBinaryOperatorKind.Less, LeftType: BoundType.Int, RightType: BoundType.Int } => (int)left < (int)right,
                    _ => throw new ArgumentOutOfRangeException()
                };
            case BoundLiteralExpression literalExpression:
                return literalExpression.Value;
            case BoundVariableExpression nameExpression:
                return environment[nameExpression.Identifier];
            default:
                throw new ArgumentOutOfRangeException(nameof(expression));
        }
    }

    private record Environment(Dictionary<string, object> Variables)
    {
        public static Environment Default() => new(new Dictionary<string, object>());

        public object this[string index]
        {
            get => Variables[index];
            set => Variables[index] = value;
        }
    }
}