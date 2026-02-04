namespace BoomScript;

public enum Type
{
    Int,
    Bool
}

public enum BoundBinaryOperatorKind
{
    Add,
    Mul,
    Greater,
    Less,
}

public record BoundBinaryOperator(
    TokenKind TokenKind,
    BoundBinaryOperatorKind Kind,
    Type LeftType,
    Type RightType,
    Type ResultType)
{
    public BoundBinaryOperator Bind(TokenKind tokenKind, Type leftType, Type rightType) =>
        _binaryOperators.FirstOrDefault(op =>
            op.TokenKind == tokenKind && op.LeftType == leftType && op.RightType == rightType) ??
        throw new InvalidOperationException("No binary operator matches given arguments.");

    private static readonly List<BoundBinaryOperator> _binaryOperators =
    [
        new(TokenKind.Plus, BoundBinaryOperatorKind.Add, Type.Int, Type.Int, Type.Int),
        new(TokenKind.Star, BoundBinaryOperatorKind.Mul, Type.Int, Type.Int, Type.Int),
        new(TokenKind.Greater, BoundBinaryOperatorKind.Greater, Type.Int, Type.Int, Type.Bool),
        new(TokenKind.Less, BoundBinaryOperatorKind.Less, Type.Int, Type.Int, Type.Bool)
    ];
}

public abstract record BoundExpression(Type Type, Expression Expression);

public sealed record BoundLiteralExpression(object Value, Type Type, Expression Expression) : BoundExpression(Type, Expression);

public sealed record BoundVariableExpression(string Identifier, Type Type, Expression Expression) : BoundExpression(Type, Expression);

public sealed record BoundBinaryExpression(BoundExpression Left, BoundBinaryOperator Operator, BoundExpression Right, Expression Expression) : BoundExpression(Operator.ResultType, Expression);

public sealed record BoundAssignmentExpression(string Identifier, BoundExpression Value, Expression Expression) : BoundExpression(Value.Type, Expression);

public class Binder
{
    
}