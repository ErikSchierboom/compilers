namespace Gleamy;

internal sealed class Scanner(string source)
{
    private int _position;

    private static readonly Dictionary<string, TokenType> Keywords = new()
    {
        ["let"]   = TokenType.LetKeyword,
        ["fn"]    = TokenType.FnKeyword,
        ["match"] = TokenType.MatchKeyword,
        ["case"]  = TokenType.CaseKeyword,
        ["Int"]   = TokenType.IntKeyword,
    };
    
    public List<Token> Scan()
    {
        var tokens = new List<Token>();
        
        while (!IsEndOfFile)
        {
            switch (Current)
            {
                case ' ' or '\n' or '\r' or '\t':
                    _position++;
                    break;
                case '+':
                    tokens.Add(new Token(TokenType.Plus, "+"));
                    _position++;
                    break;
                case '*':
                    tokens.Add(new Token(TokenType.Star, "*"));
                    _position++;
                    break;
                case '/':
                    tokens.Add(new Token(TokenType.Slash, "/"));
                    _position++;
                    break;
                case ',':
                    tokens.Add(new Token(TokenType.Comma, ","));
                    _position++;
                    break;
                case ':':
                    tokens.Add(new Token(TokenType.Colon, ":"));
                    _position++;
                    break;
                case ';':
                    tokens.Add(new Token(TokenType.Semicolon, ";"));
                    _position++;
                    break;
                case '_':
                    tokens.Add(new Token(TokenType.Underscore, "_"));
                    _position++;
                    break;
                case '(':
                    tokens.Add(new Token(TokenType.OpenParen, "("));
                    _position++;
                    break;
                case ')':
                    tokens.Add(new Token(TokenType.CloseParen, ")"));
                    _position++;
                    break;
                case '{':
                    tokens.Add(new Token(TokenType.OpenBracket, "{"));
                    _position++;
                    break;
                case '}':
                    tokens.Add(new Token(TokenType.CloseBracket, "}"));
                    _position++;
                    break;
                case '=':
                    if (Match('>'))
                        tokens.Add(new Token(TokenType.EqualGreaterThan, "=>"));
                    else
                        tokens.Add(new Token(TokenType.Equal, "="));
                    _position++;
                    break;
                case '-':
                    if (Match('>'))
                        tokens.Add(new Token(TokenType.MinusGreaterThan, "->"));
                    else
                        tokens.Add(new Token(TokenType.Minus, "-"));
                    _position++;
                    break;
                case >= '0' and <= '9':
                    var numberStartPosition = _position;
                    while (Current is >= '0' and <= '9')
                        _position++;
                    
                    tokens.Add(new Token(TokenType.Number, source[numberStartPosition.._position]));
                    break;
                case >= 'a' and <= 'z' or >= 'A' and <= 'Z':
                    var identifierStartPosition = _position;
                    while (Current is >= 'a' and <= 'z' or >= 'A' and <= 'Z')
                        _position++;

                    var text = source[identifierStartPosition.._position];
                    if (Keywords.TryGetValue(text, out var tokenType))
                        tokens.Add(new Token(tokenType, text));    
                    else
                        tokens.Add(new Token(TokenType.Identifier, text));
                    break;
                default:
                    throw new InvalidOperationException($"Unknown character: '{source[_position]}'");
            }
        }
        
        tokens.Add(new Token(TokenType.Eof, ""));
        
        return tokens;
    }

    private bool Match(char expected)
    {
        if (Next != expected)
            return false;
        
        _position++;
        return true;
    }

    private bool IsEndOfFile => _position >= source.Length;
    
    private char Current => _position < source.Length ? source[_position] : '\0';
    private char Next => _position < source.Length - 1 ? source[_position + 1] : '\0';
}

internal sealed record Token(TokenType Type, string Text);

internal enum TokenType
{
    Number,
    Identifier,
    
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Equal,
    EqualGreaterThan,
    Minus,
    MinusGreaterThan,
    Colon,
    Semicolon,
    Comma,
    Underscore,
    Plus,
    Star,
    Slash,
    
    LetKeyword,
    FnKeyword,
    MatchKeyword,
    CaseKeyword,
    IntKeyword,

    Eof,
}
