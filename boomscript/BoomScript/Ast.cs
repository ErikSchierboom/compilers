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
    EndOfFileToken,
    BadToken,
}

public abstract record SyntaxNode(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span);

public sealed record SyntaxToken(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span, SyntaxTrivia[] LeadingTrivia, SyntaxTrivia[] TrailingTrivia);

public sealed record SyntaxTrivia(SyntaxTree Tree, SyntaxKind Kind, TextSpan Span);