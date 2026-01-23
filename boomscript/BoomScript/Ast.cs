namespace BoomScript;

public record SyntaxTree(SourceText Text)
{
}

public enum SyntaxKind
{
    // Tokens
    NumberToken,
    PlusToken,
    MinusToken,
    StarToken,
    SlashToken,
    EndOfFileToken,
    BadToken,
    OpenParenthesisToken,
    CloseParenthesisToken,
    
    // Nodes
    BinaryExpression,
    LiteralExpression,
    ParenthesizedExpression,
    ExpressionStatement,
    CompilationUnit
}

public abstract record SyntaxElement(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span)
{
    public TextLocation Location => field ??= new TextLocation(Tree.Text, Span);   
}

public sealed record SyntaxToken(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span) : SyntaxElement(Tree, Kind, Span);
public abstract record SyntaxNode(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span) : SyntaxElement(Tree, Kind, Span);

public sealed record CompilationUnit(Statement[] Statements, SyntaxToken EndOfFileToken, SyntaxTree Tree, TextSpan Span) : SyntaxNode(Tree, SyntaxKind.CompilationUnit, Span);

public abstract record Expression(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span): SyntaxNode(Tree, Kind, Span);
public sealed record BinaryExpression(Expression Left, SyntaxToken OperatorToken, Expression Right, SyntaxTree Tree, TextSpan Span) : Expression(Tree, SyntaxKind.BinaryExpression, Span);
public sealed record LiteralExpression(SyntaxToken LiteralToken, object? Value, SyntaxTree Tree, TextSpan Span) : Expression(Tree, SyntaxKind.LiteralExpression, Span);
public sealed record ParenthesizedExpression(SyntaxToken OpenParenthesisToken, Expression Expression, SyntaxToken ClosesParenthesisToken, SyntaxTree Tree, TextSpan Span) : Expression(Tree, SyntaxKind.ParenthesizedExpression, Span);

public abstract record Statement(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span): SyntaxNode(Tree, Kind, Span);
public sealed record ExpressionStatement(Expression Expression, SyntaxTree Tree, TextSpan Span) : Statement(Tree, SyntaxKind.ExpressionStatement, Span);
