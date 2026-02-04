namespace BoomScript;

public enum TokenKind
{
    // Literals
    Number,
    Identifier,
    
    // Operators
    Plus,
    Star,
    Equals,
    Greater,
    Less,
    
    // Punctuation
    EndOfFile,
}

public sealed record Token(TokenKind Kind, TextSpan Span);

public sealed class Lexer
{
    private readonly SourceText _sourceText;

    private Lexer(SourceText sourceText) => _sourceText = sourceText;

    private Token[] Lex()
    {
        var tokens = new List<Token>();
        var position = 0;

        while (position < _sourceText.Length)
        {
            switch (_sourceText[position])
            {
                case var c when char.IsWhiteSpace(c):
                    position++;
                    break;
                case '+':
                    tokens.Add(new Token(TokenKind.Plus, new TextSpan(position, 1)));
                    position++;
                    break;
                case '*':
                    tokens.Add(new Token(TokenKind.Star, new TextSpan(position, 1)));
                    position++;
                    break;
                case '=':
                    tokens.Add(new Token(TokenKind.Equals, new TextSpan(position, 1)));
                    position++;
                    break;
                case '>':
                    tokens.Add(new Token(TokenKind.Greater, new TextSpan(position, 1)));
                    position++;
                    break;
                case '<':
                    tokens.Add(new Token(TokenKind.Less, new TextSpan(position, 1)));
                    position++;
                    break;
                case var c when char.IsDigit(c):
                    var digitStartPosition = position;

                    while (position < _sourceText.Length && char.IsDigit(_sourceText[position]))
                        position++;
                    
                    tokens.Add(new Token(TokenKind.Number, new TextSpan(digitStartPosition, position - digitStartPosition)));
                    break;
                case var c when char.IsAsciiLetter(c):
                    var identifierStartPosition = position;

                    while (position < _sourceText.Length && char.IsAsciiLetterOrDigit(_sourceText[position]))
                        position++;
                 
                    tokens.Add(new Token(TokenKind.Identifier, new TextSpan(identifierStartPosition, position - identifierStartPosition)));
                    break;
                default:
                    // TODO: add error handling
                    throw new InvalidOperationException("Unknown token");
            }
        }
        
        tokens.Add(new Token(TokenKind.EndOfFile, new TextSpan(_sourceText.Length, 0)));

        return tokens.ToArray();
    }

    public static Token[] Lex(SourceText sourceText) => new Lexer(sourceText).Lex();
}