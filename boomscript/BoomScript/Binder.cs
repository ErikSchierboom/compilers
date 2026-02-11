namespace BoomScript;

public enum BoundType
{
    Unit,
    Int,
    Bool,
}

public enum BoundBinaryOperatorKind
{
    Add,
    Mul,
    Greater,
    Less,
}

public record BoundBinaryOperator(
    BoundBinaryOperatorKind Kind,
    BoundType LeftType,
    BoundType RightType,
    BoundType ResultType);

public abstract record BoundExpression(BoundType Type);

public sealed record BoundLiteralExpression(object Value, BoundType Type) : BoundExpression(Type);

public sealed record BoundBinaryExpression(BoundExpression Left, BoundBinaryOperator Operator, BoundExpression Right) : BoundExpression(Operator.ResultType);

public sealed record BoundVariableExpression(string Identifier, BoundType Type) : BoundExpression(Type);

public sealed record BoundAssignmentExpression(string Identifier, BoundExpression Value) : BoundExpression(Value.Type);

public sealed record BoundProgram(BoundExpression[] Expressions, BoundType Type): BoundExpression(Type);

public sealed class Binder
{
    private readonly Expression[] _expressions;
    private readonly Dictionary<string, BoundType> _variables = new();

    private static readonly Dictionary<TokenKind, BoundBinaryOperator[]> _binaryOperators = new()
    {
        [TokenKind.Plus] = [new(BoundBinaryOperatorKind.Add, BoundType.Int, BoundType.Int, BoundType.Int)],
        [TokenKind.Star] = [new(BoundBinaryOperatorKind.Mul, BoundType.Int, BoundType.Int, BoundType.Int)],
        [TokenKind.Greater] = [new(BoundBinaryOperatorKind.Greater, BoundType.Int, BoundType.Int, BoundType.Bool)],
        [TokenKind.Less] = [new(BoundBinaryOperatorKind.Less, BoundType.Int, BoundType.Int, BoundType.Bool)],
    };

    private Binder(Expression[] expressions) => _expressions = expressions;

    public static BoundProgram Bind(Expression[] expressions) => new Binder(expressions).BindProgram();

    private BoundProgram BindProgram()
    {
        var boundExpressions = _expressions.Select(BindExpression).ToArray();
        return new BoundProgram(boundExpressions, boundExpressions.LastOrDefault()?.Type ?? BoundType.Unit);
    }

    private BoundExpression BindExpression(Expression expression) =>
        expression switch
        {
            AssignmentExpression assignmentExpression => BindAssignmentExpression(assignmentExpression),
            BinaryExpression binaryExpression => BindBinaryExpression(binaryExpression),
            LiteralExpression literalExpression => BindLiteralExpression(literalExpression),
            VariableExpression variableExpression => BindVariableExpression(variableExpression),
            _ => throw new ArgumentOutOfRangeException(nameof(expression))
        };

    private BoundAssignmentExpression BindAssignmentExpression(AssignmentExpression assignmentExpression)
    {
        var boundExpression = BindExpression(assignmentExpression.Value);
        _variables[assignmentExpression.Identifier] = boundExpression.Type;
        return new BoundAssignmentExpression(assignmentExpression.Identifier, boundExpression);
    }

    private BoundBinaryExpression BindBinaryExpression(BinaryExpression binaryExpression)
    {
        var boundLeft = BindExpression(binaryExpression.Left);
        var boundRight = BindExpression(binaryExpression.Right);

        var boundBinaryOperator = _binaryOperators[binaryExpression.Operator.Kind]
            .FirstOrDefault(op => op.LeftType == boundLeft.Type && op.RightType == boundRight.Type) ??
                                  throw new InvalidOperationException("No binary operator matches given arguments.");
        
        return new BoundBinaryExpression(boundLeft, boundBinaryOperator, boundRight);
    }

    private BoundLiteralExpression BindLiteralExpression(LiteralExpression literalExpression) =>
        literalExpression.Value switch
        {
            int i => new BoundLiteralExpression(i, BoundType.Int),
            bool b => new BoundLiteralExpression(b, BoundType.Bool),
            _ => throw new ArgumentOutOfRangeException(nameof(literalExpression.Value), literalExpression.Value, null)
        };

    private BoundVariableExpression BindVariableExpression(VariableExpression variableExpression)
    {
        if (_variables.TryGetValue(variableExpression.Identifier, out var type))
            return new BoundVariableExpression(variableExpression.Identifier, type);
        
        throw new InvalidOperationException($"Unbound variable '{variableExpression.Identifier}'.");
    }
}