namespace Gleamy;

internal sealed class Scanner
{
    private readonly string _source;
    private int _position;

    private Scanner(string source) => _source = source;

    private static readonly Dictionary<string, (TokenType TokenType, object? Literal)> _keywords = new()
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

    public static List<Token> Scan(string source) =>
        new Scanner(source).Scan();

    private List<Token> Scan()
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
                case '^':
                    tokens.Add(new Token(TokenType.Caret, "^"));
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
                case '~':
                    tokens.Add(new Token(TokenType.Tilde, "~"));
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
                case '&':
                    if (Match('&'))
                    {
                        tokens.Add(new Token(TokenType.AmpersandAmpersand, "&&"));
                        _position++;
                    }
                    else
                    {
                        tokens.Add(new Token(TokenType.Ampersand, "&"));
                        _position++;
                    }
                    break;
                case '|':
                    if (Match('|'))
                    {
                        tokens.Add(new Token(TokenType.PipePipe, "||"));
                        _position++;
                    }
                    else
                    {
                        tokens.Add(new Token(TokenType.Pipe, "|"));
                        _position++;
                    }
                    break;
                case '=':
                    if (Match('>'))
                    {
                        tokens.Add(new Token(TokenType.EqualGreater, "=>"));
                        _position++;
                    }
                    else if (Match('='))
                    {
                        tokens.Add(new Token(TokenType.EqualEqual, "=="));
                        _position++;
                    }
                    else
                    {
                        tokens.Add(new Token(TokenType.Equal, "="));
                        _position++;
                    }
                    break;
                case '!':
                    if (Match('='))
                    {
                        tokens.Add(new Token(TokenType.BangEqual, "!="));
                        _position++;
                    }
                    else
                    {
                        tokens.Add(new Token(TokenType.Bang, "!"));
                        _position++;
                    }
                    break;
                case '-':
                    if (Match('>'))
                    {
                        tokens.Add(new Token(TokenType.MinusGreater, "->"));
                        _position++;
                    }
                    else
                    {
                        tokens.Add(new Token(TokenType.Minus, "-"));
                        _position++;
                    }
                    break;
                case '>':
                    if (Match('>'))
                    {
                        tokens.Add(new Token(TokenType.GreaterGreater, ">>"));
                        _position++;
                    }
                    else if (Match('='))
                    {
                        tokens.Add(new Token(TokenType.GreaterEqual, ">="));
                        _position++;
                    }
                    else
                    {
                        tokens.Add(new Token(TokenType.Greater, ">"));
                        _position++;
                    }
                    break;
                case '<':
                    if (Match('<'))
                    {
                        tokens.Add(new Token(TokenType.LessLess, "<<"));
                        _position++;
                    }
                    else if (Match('='))
                    {
                        tokens.Add(new Token(TokenType.LessEqual, "<="));
                        _position++;
                    }
                    else
                    {
                        tokens.Add(new Token(TokenType.Less, "<"));
                        _position++;
                    }
                    break;
                case >= '0' and <= '9':
                    var numberStartPosition = _position;
                    while (Current is >= '0' and <= '9')
                        _position++;

                    var numberString = _source[numberStartPosition.._position];
                    tokens.Add(new Token(TokenType.Number, numberString, int.Parse(numberString)));
                    break;
                case >= 'a' and <= 'z' or >= 'A' and <= 'Z':
                    var identifierStartPosition = _position;
                    while (Current is >= 'a' and <= 'z' or >= 'A' and <= 'Z' or '_')
                        _position++;

                    var text = _source[identifierStartPosition.._position];
                    if (_keywords.TryGetValue(text, out var tokenTypeAndLiteral))
                        tokens.Add(new Token(tokenTypeAndLiteral.TokenType, text, tokenTypeAndLiteral.Literal));    
                    else
                        tokens.Add(new Token(TokenType.Identifier, text));
                    break;
                default:
                    throw new InvalidOperationException($"Unknown character: '{_source[_position]}'");
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

    private bool IsEndOfFile => _position >= _source.Length;
    
    private char Current => _position < _source.Length ? _source[_position] : '\0';
    private char Next => _position < _source.Length - 1 ? _source[_position + 1] : '\0';
}

internal sealed record Token(TokenType Type, string Text, object? Literal = null);

internal enum TokenType
{
    // Literals
    Number,
    Identifier,
    
    // Symbols
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Less,
    LessLess,
    LessEqual,
    Greater,
    GreaterGreater,
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
    Ampersand,
    AmpersandAmpersand,
    Pipe,
    PipePipe,
    Caret,
    Tilde,
    
    // Keywords
    LetKeyword,
    FnKeyword,
    MatchKeyword,
    CaseKeyword,
    IntKeyword,
    BoolKeyword,
    TrueKeyword,
    FalseKeyword,

    // Synthetic
    Eof,
}
