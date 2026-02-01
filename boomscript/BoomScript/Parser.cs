namespace BoomScript;

public abstract record Expression(TextSpan Span);

public sealed record IntegerExpression(int Value, TextSpan Span) : Expression(Span);

public sealed record BinaryExpression(Expression Left, BinaryOperatorKind Operator, Expression Right, TextSpan Span) : Expression(Span);

public sealed record AssignmentExpression(string Identifier, Expression Value, TextSpan Span) : Expression(Span);

public enum BinaryOperatorKind
{
    Plus
}

public sealed class Parser(SourceText sourceText)
{
    private Expression[] Parse()
    {
        throw new NotImplementedException();
    }
    
    public static Expression[] Parse(SourceText sourceText) => new Parser(sourceText).Parse();
}