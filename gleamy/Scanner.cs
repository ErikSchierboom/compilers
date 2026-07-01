namespace Gleamy;

internal sealed class Scanner(string source)
{
    private int _position;

    private static readonly Dictionary<string, (TokenType TokenType, object? Literal)> Keywords = new()
    {
        ["let"]   = (TokenType.LetKeyword, null),
        ["fn"]    = (TokenType.FnKeyword, null),
        ["match"] = (TokenType.MatchKeyword, null),
        ["case"]  = (TokenType.CaseKeyword, null),
        ["true"]  = (TokenType.TrueKeyword, true),
        ["false"] = (TokenType.FalseKeyword, false),
        ["Bool"]  = (TokenType.BoolKeyword, null),
        ["Int"]   = (TokenType.IntKeyword, null),
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
                case '%':
                    tokens.Add(new Token(TokenType.Percent, "%"));
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
                        tokens.Add(new Token(TokenType.EqualGreater, "=>"));
                    else if (Match('='))
                        tokens.Add(new Token(TokenType.EqualEqual, "=="));
                    else
                        tokens.Add(new Token(TokenType.Equal, "="));
                    _position++;
                    break;
                case '!':
                    if (Match('='))
                        tokens.Add(new Token(TokenType.BangEqual, "!="));
                    else
                        tokens.Add(new Token(TokenType.Bang, "!"));
                    _position++;
                    break;
                case '-':
                    if (Match('>'))
                        tokens.Add(new Token(TokenType.MinusGreater, "->"));
                    else
                        tokens.Add(new Token(TokenType.Minus, "-"));
                    _position++;
                    break;
                case '>':
                    if (Match('='))
                        tokens.Add(new Token(TokenType.GreaterEqual, ">="));
                    else
                        tokens.Add(new Token(TokenType.Greater, ">"));
                    _position++;
                    break;
                case '<':
                    if (Match('='))
                        tokens.Add(new Token(TokenType.LessEqual, "<="));
                    else
                        tokens.Add(new Token(TokenType.Less, "<"));
                    _position++;
                    break;
                case >= '0' and <= '9':
                    var numberStartPosition = _position;
                    while (Current is >= '0' and <= '9')
                        _position++;

                    var numberString = source[numberStartPosition.._position];
                    tokens.Add(new Token(TokenType.Number, numberString, int.Parse(numberString)));
                    break;
                case >= 'a' and <= 'z' or >= 'A' and <= 'Z':
                    var identifierStartPosition = _position;
                    while (Current is >= 'a' and <= 'z' or >= 'A' and <= 'Z')
                        _position++;

                    var text = source[identifierStartPosition.._position];
                    if (Keywords.TryGetValue(text, out var tokenTypeAndLiteral))
                        tokens.Add(new Token(tokenTypeAndLiteral.TokenType, text, tokenTypeAndLiteral.Literal));    
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

internal sealed record Token(TokenType Type, string Text, object? Literal = null);

internal enum TokenType
{
    Number,
    Identifier,
    
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    EqualEqual,
    EqualGreater,
    Minus,
    MinusGreater,
    Bang,
    BangEqual,
    Colon,
    Semicolon,
    Comma,
    Underscore,
    Percent,
    Plus,
    Star,
    Slash,
    
    LetKeyword,
    FnKeyword,
    MatchKeyword,
    CaseKeyword,
    IntKeyword,
    BoolKeyword,
    TrueKeyword,
    FalseKeyword,

    Eof,
}
