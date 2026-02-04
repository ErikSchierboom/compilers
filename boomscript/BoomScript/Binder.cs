namespace BoomScript;

public enum Type
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
    Type LeftType,
    Type RightType,
    Type ResultType);

public abstract record BoundExpression(Type Type);

public sealed record BoundLiteralExpression(object Value, Type Type) : BoundExpression(Type);

public sealed record BoundBinaryExpression(BoundExpression Left, BoundBinaryOperator Operator, BoundExpression Right) : BoundExpression(Operator.ResultType);

public sealed record BoundVariableExpression(string Identifier, Type Type) : BoundExpression(Type);

public sealed record BoundAssignmentExpression(string Identifier, BoundExpression Value) : BoundExpression(Value.Type);

public sealed record BoundProgram(BoundExpression[] Expressions, Type Type): BoundExpression(Type);

public sealed class Binder
{
    private readonly Expression[] _expressions;
    private readonly Dictionary<string, Type> _variables = new();

    private static readonly Dictionary<TokenKind, BoundBinaryOperator[]> _binaryOperators = new()
    {
        [TokenKind.Plus] = [new(BoundBinaryOperatorKind.Add, Type.Int, Type.Int, Type.Int)],
        [TokenKind.Star] = [new(BoundBinaryOperatorKind.Mul, Type.Int, Type.Int, Type.Int)],
        [TokenKind.Greater] = [new(BoundBinaryOperatorKind.Greater, Type.Int, Type.Int, Type.Bool)],
        [TokenKind.Less] = [new(BoundBinaryOperatorKind.Less, Type.Int, Type.Int, Type.Bool)],
    };

    private Binder(Expression[] expressions) => _expressions = expressions;

    public static BoundProgram Bind(Expression[] expressions) => new Binder(expressions).BindProgram();

    private BoundProgram BindProgram()
    {
        var boundExpressions = _expressions.Select(BindExpression).ToArray();
        return new BoundProgram(boundExpressions, boundExpressions.LastOrDefault()?.Type ?? Type.Unit);
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
            int i => new BoundLiteralExpression(i, Type.Int),
            bool b => new BoundLiteralExpression(b, Type.Bool),
            _ => throw new ArgumentOutOfRangeException(nameof(literalExpression.Value), literalExpression.Value, null)
        };

    private BoundVariableExpression BindVariableExpression(VariableExpression variableExpression)
    {
        if (_variables.TryGetValue(variableExpression.Identifier, out var type))
            return new BoundVariableExpression(variableExpression.Identifier, type);
        
        throw new InvalidOperationException($"Unbound variable '{variableExpression.Identifier}'.");
    }
}