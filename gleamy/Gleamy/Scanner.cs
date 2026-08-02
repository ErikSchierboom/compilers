using System.Text;

namespace Gleamy;

internal sealed class Scanner
{
    private static readonly Dictionary<string, (TokenType TokenType, object? Literal)> _keywords = new()
    {
        ["let"]   = (TokenType.LetKeyword, null),
        ["fn"]    = (TokenType.FnKeyword, null),
        ["match"] = (TokenType.MatchKeyword, null),
        ["case"]  = (TokenType.CaseKeyword, null),
        ["true"]  = (TokenType.TrueKeyword, true),
        ["false"] = (TokenType.FalseKeyword, false),
        ["Bool"]  = (TokenType.BoolKeyword, null),
        ["Char"]  = (TokenType.CharKeyword, null),
        ["Int"]   = (TokenType.IntKeyword, null),
    };
    
    private readonly string _source;
    private readonly StringBuilder _stringValue = new();
    private int _position;

    private Scanner(string source) => _source = source;

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
                    _position++;
                    tokens.Add(new Token(TokenType.Plus, "+"));
                    break;
                case '*':
                    _position++;
                    tokens.Add(new Token(TokenType.Star, "*"));
                    break;
                case '/':
                    _position++;
                    tokens.Add(new Token(TokenType.Slash, "/"));
                    break;
                case '^':
                    _position++;
                    tokens.Add(new Token(TokenType.Caret, "^"));
                    break;
                case ',':
                    _position++;
                    tokens.Add(new Token(TokenType.Comma, ","));
                    break;
                case ':':
                    _position++;
                    tokens.Add(new Token(TokenType.Colon, ":"));
                    break;
                case ';':
                    _position++;
                    tokens.Add(new Token(TokenType.Semicolon, ";"));
                    break;
                case '%':
                    _position++;
                    tokens.Add(new Token(TokenType.Percent, "%"));
                    break;
                case '~':
                    _position++;
                    tokens.Add(new Token(TokenType.Tilde, "~"));
                    break;
                case '_':
                    _position++;
                    tokens.Add(new Token(TokenType.Underscore, "_"));
                    break;
                case '(':
                    _position++;
                    tokens.Add(new Token(TokenType.OpenParen, "("));
                    break;
                case ')':
                    _position++;
                    tokens.Add(new Token(TokenType.CloseParen, ")"));
                    break;
                case '{':
                    _position++;
                    tokens.Add(new Token(TokenType.OpenBrace, "{"));
                    break;
                case '}':
                    _position++;
                    tokens.Add(new Token(TokenType.CloseBrace, "}"));
                    break;
                case '[':
                    _position++;
                    tokens.Add(new Token(TokenType.OpenBracket, "["));
                    break;
                case ']':
                    _position++;
                    tokens.Add(new Token(TokenType.CloseBracket, "]"));
                    break;
                case '&':
                    _position++;
                    
                    if (Match('&'))
                    {
                        tokens.Add(new Token(TokenType.AmpersandAmpersand, "&&"));
                        break;
                    }

                    tokens.Add(new Token(TokenType.Ampersand, "&"));
                    break;
                case '|':
                    _position++;
                    
                    if (Match('|'))
                    {
                        tokens.Add(new Token(TokenType.PipePipe, "||"));
                        break;
                    }

                    tokens.Add(new Token(TokenType.Pipe, "|"));
                    break;
                case '=':
                    _position++;
                    
                    if (Match('>'))
                    {
                        tokens.Add(new Token(TokenType.EqualGreater, "=>"));
                        break;
                    }

                    if (Match('='))
                    {
                        tokens.Add(new Token(TokenType.EqualEqual, "=="));
                        break;
                    }

                    tokens.Add(new Token(TokenType.Equal, "="));
                    break;
                case '!':
                    _position++;
                    
                    if (Match('='))
                    {
                        tokens.Add(new Token(TokenType.BangEqual, "!="));
                        break;
                    }

                    tokens.Add(new Token(TokenType.Bang, "!"));
                    break;
                case '-':
                    _position++;
                    
                    if (Match('>'))
                    {
                        tokens.Add(new Token(TokenType.MinusGreater, "->"));
                        break;
                    }

                    tokens.Add(new Token(TokenType.Minus, "-"));
                    break;
                case '>':
                    _position++;
                    
                    if (Match('>'))
                    {
                        tokens.Add(new Token(TokenType.GreaterGreater, ">>"));
                        break;
                    }

                    if (Match('='))
                    {
                        tokens.Add(new Token(TokenType.GreaterEqual, ">="));
                        break;
                    }

                    tokens.Add(new Token(TokenType.Greater, ">"));
                    break;
                case '<':
                    _position++;
                    
                    if (Match('<'))
                    {
                        tokens.Add(new Token(TokenType.LessLess, "<<"));
                        break;
                    }

                    if (Match('='))
                    {
                        tokens.Add(new Token(TokenType.LessEqual, "<="));
                        break;
                    }

                    tokens.Add(new Token(TokenType.Less, "<"));
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
                    {
                        tokens.Add(new Token(tokenTypeAndLiteral.TokenType, text, tokenTypeAndLiteral.Literal));
                        break;
                    }

                    tokens.Add(new Token(TokenType.Identifier, text));
                    break;
                case '\'':
                    var charStartPosition = _position;
                    _position++;
                    
                    if (Match('\\'))
                    {
                        if (Match('n'))
                        {
                            Consume('\'');
                            tokens.Add(new Token(TokenType.Char, "'\n'", '\n'));
                            break;
                        }
                    
                        if (Match('r'))
                        {
                            Consume('\'');
                            tokens.Add(new Token(TokenType.Char, "'\r'", '\r'));
                            break;
                        }
                        
                        if (Match('t'))
                        {
                            Consume('\'');
                            tokens.Add(new Token(TokenType.Char, "'\t'", '\t'));
                            break;
                        }
                        
                        throw new InvalidOperationException($"Unknown escape sequence: '\\{Current}'");
                    }

                    var currentChar = Current; 
                    _position++;
                    Consume('\'');
                    tokens.Add(new Token(TokenType.Char, _source[charStartPosition.._position], currentChar));
                    break;
                
                case '"':
                    var stringStartPosition = _position;
                    _position++;
                    
                    _stringValue.Clear();
                    while (!Match('"'))
                    {
                        if (Match('\\'))
                        {
                            if (Match('n'))
                            {
                                _stringValue.Append('\n');
                                continue;
                            }
                    
                            if (Match('r'))
                            {
                                _stringValue.Append('\r');
                                continue;
                            }
                        
                            if (Match('t'))
                            {
                                _stringValue.Append('\t');
                                continue;
                            }
                        
                            throw new InvalidOperationException($"Unknown escape sequence: '\\{Current}'");
                        }
                        
                        _stringValue.Append(Current);
                        _position++;
                    }
                   
                    tokens.Add(new Token(TokenType.String, _source[stringStartPosition.._position], _stringValue.ToString()));
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
        if (Current != expected)
            return false;
        
        _position++;
        return true;
    }
    
    private void Consume(char expected)
    {
        if (Current != expected)
            throw new InvalidOperationException($"Expected '{expected}' but found '{Next}'");
        
        _position++;
    }

    private bool IsEndOfFile => _position >= _source.Length;
    
    private char Current => _position < _source.Length ? _source[_position] : '\0';
    private char Next => _position < _source.Length - 1 ? _source[_position + 1] : '\0';
}

internal sealed record Token(TokenType Type, string Text, object? Literal = null);

internal enum TokenType
{
    // Literals
    Identifier,
    Number,
    Char,
    String,
    
    // Symbols
    Ampersand,
    AmpersandAmpersand,
    Bang,
    BangEqual,
    Caret,
    CloseBrace,
    CloseBracket,
    CloseParen,
    Colon,
    Comma,
    Equal,
    EqualEqual,
    EqualGreater,
    Greater,
    GreaterEqual,
    GreaterGreater,
    Less,
    LessEqual,
    LessLess,
    Minus,
    MinusGreater,
    OpenBrace,
    OpenBracket,
    OpenParen,
    Percent,
    Pipe,
    PipePipe,
    Plus,
    Semicolon,
    Slash,
    Star,
    Tilde,
    Underscore,
    
    // Keywords
    BoolKeyword,
    CaseKeyword,
    CharKeyword,
    FalseKeyword,
    FnKeyword,
    IntKeyword,
    LetKeyword,
    MatchKeyword,
    TrueKeyword,

    // Synthetic
    Eof,
}
