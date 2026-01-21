namespace BoomScript;

public record SyntaxTree(SourceText Text)
{
}

public enum SyntaxKind
{
    // Trivia
    LineBreakTrivia,
    WhitespaceTrivia,
    
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
    ParenthesizedExpression
}

public abstract record SyntaxNode(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span);

public abstract record ExpressionSyntax(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span): SyntaxNode(Tree, Kind, Span);
public sealed record BinaryExpression(ExpressionSyntax Left, SyntaxToken OperatorToken, ExpressionSyntax Right, SyntaxTree Tree, TextSpan Span) : ExpressionSyntax(Tree, SyntaxKind.BinaryExpression, Span);
public sealed record LiteralExpression(SyntaxToken LiteralToken, object? value, SyntaxTree Tree, TextSpan Span) : ExpressionSyntax(Tree, SyntaxKind.LiteralExpression, Span);
public sealed record ParenthesizedExpression(SyntaxToken OpenParenthesisToken, ExpressionSyntax Expression, SyntaxToken ClosesParenthesisToken, SyntaxTree Tree, TextSpan Span) : ExpressionSyntax(Tree, SyntaxKind.ParenthesizedExpression, Span);

public sealed record SyntaxToken(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span, SyntaxTrivia[] LeadingTrivia, SyntaxTrivia[] TrailingTrivia);

public sealed record SyntaxTrivia(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span);