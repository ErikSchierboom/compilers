namespace BoomScript;

public enum SyntaxTokenKind
{
    // Literals
    Number,
    Identifier,
    
    // Operators
    Plus,
    Equals,
    
    // Punctuation
    EndOfFile,
}

public record SyntaxToken(SyntaxTokenKind Kind, TextSpan Span);

public class Lexer(SourceText sourceText)
{
    private SyntaxToken[] Lex()
    {
        var tokens = new List<SyntaxToken>();
        var position = 0;

        while (position < sourceText.Length)
        {
            switch (sourceText[position])
            {
                case var c when char.IsWhiteSpace(c):
                    position++;
                    break;
                case '+':
                    tokens.Add(new SyntaxToken(SyntaxTokenKind.Plus, new TextSpan(position, 1)));
                    position++;
                    break;
                case '=':
                    tokens.Add(new SyntaxToken(SyntaxTokenKind.Equals, new TextSpan(position, 1)));
                    position++;
                    break;
                case var c when char.IsDigit(c):
                    var digitStartPosition = position;

                    while (position < sourceText.Length && char.IsDigit(sourceText[position]))
                        position++;
                    
                    tokens.Add(new SyntaxToken(SyntaxTokenKind.Number, new TextSpan(digitStartPosition, position - digitStartPosition)));
                    break;
                case var c when char.IsAsciiLetter(c):
                    var identifierStartPosition = position;

                    while (position < sourceText.Length && char.IsAsciiLetterOrDigit(sourceText[position]))
                        position++;
                 
                    tokens.Add(new SyntaxToken(SyntaxTokenKind.Identifier, new TextSpan(identifierStartPosition, position - identifierStartPosition)));
                    break;
                default:
                    // TODO: add error handling
                    throw new InvalidOperationException("Unknown token");
            }
        }
        
        tokens.Add(new SyntaxToken(SyntaxTokenKind.EndOfFile, new TextSpan(sourceText.Length, 0)));
        
        return tokens.ToArray();
    }

    public static SyntaxToken[] Lex(SourceText sourceText) => new Lexer(sourceText).Lex();
}