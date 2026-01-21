namespace BoomScript.Tests;

public sealed class LexingTests
{
    [Fact]
    public void TokensHaveLeadingAndTrailingTrivia()
    {
        var tree = new SyntaxTree(new SourceText(" 123 \t"));
        var lexer = new Lexer(tree);

        var token = lexer.Lex();

        Assert.Equal([
            new SyntaxTrivia(tree, SyntaxKind.WhitespaceTrivia, new TextSpan(0, 1))
        ], token.LeadingTrivia);
        Assert.Equal([
            new SyntaxTrivia(tree, SyntaxKind.WhitespaceTrivia, new TextSpan(4, 2))
        ], token.TrailingTrivia);
    }
}