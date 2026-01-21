namespace BoomScript.Tests;

public sealed class LexingTests
{
    public sealed class TriviaTests
    {
        [Fact]
        public void LeadingAndTrailingWhitespace()
        {
            var lexer = CreateLexer(" 123 \t");

            var token = lexer.Lex();

            Assert.Equal([
                new SyntaxTrivia(lexer.Tree, SyntaxKind.WhitespaceTrivia, new TextSpan(0, 1))
            ], token.LeadingTrivia);
            Assert.Equal([
                new SyntaxTrivia(lexer.Tree, SyntaxKind.WhitespaceTrivia, new TextSpan(4, 2))
            ], token.TrailingTrivia);
        }
    
        [Fact]
        public void OnlyLeadingWhitespace()
        {
            var lexer = CreateLexer(" 123");

            var token = lexer.Lex();

            Assert.Equal([
                new SyntaxTrivia(lexer.Tree, SyntaxKind.WhitespaceTrivia, new TextSpan(0, 1))
            ], token.LeadingTrivia);
            Assert.Empty(token.TrailingTrivia);
        }

        [Fact]
        public void OnlyTrailingWhitespace()
        {
            var lexer = CreateLexer("123 \t");

            var token = lexer.Lex();

            Assert.Empty(token.LeadingTrivia);
            Assert.Equal([
                new SyntaxTrivia(lexer.Tree, SyntaxKind.WhitespaceTrivia, new TextSpan(3, 2))
            ], token.TrailingTrivia);
        }

        [Fact]
        public void TrailingLineBreak()
        {
            var lexer = CreateLexer("123\r\n");

            var token = lexer.Lex();

            Assert.Empty(token.LeadingTrivia);
            Assert.Equal([
                new SyntaxTrivia(lexer.Tree, SyntaxKind.LineBreakTrivia, new TextSpan(3, 2))
            ], token.TrailingTrivia);
        }
        
        [Fact]
        public void LineBreaksAreTrailingTrivia()
        {
            var lexer = CreateLexer("123\r\n456");

            var firstToken = lexer.Lex();
            var secondToken = lexer.Lex();

            Assert.Equal([
                new SyntaxTrivia(lexer.Tree, SyntaxKind.LineBreakTrivia, new TextSpan(3, 2))
            ], firstToken.TrailingTrivia);
            Assert.Empty(secondToken.LeadingTrivia);
        }

        private static Lexer CreateLexer(string text) => new(new SyntaxTree(new SourceText(text)));
    }
}