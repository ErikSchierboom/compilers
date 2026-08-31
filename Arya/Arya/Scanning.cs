using System.Text;

namespace Arya;

public sealed class Scanner
{
    private readonly string _source;
    private readonly StringBuilder _stringValue = new();
    private int _position;

    private Scanner(string source) => _source = source;

    public static List<Token> Scan(string source) => new Scanner(source).Scan();

    private List<Token> Scan()
    {
        var tokens = new List<Token>();

        while (!IsEndOfFile)
        {
            switch (Current)
            {
                case ' ' or '\n' or '\r' or '\t':
                    Advance();
                    break;
                case '+':
                    Advance();
                    if (Match('+'))
                        tokens.Add(new Token(TokenType.PlusPlus, "++"));
                    else
                        tokens.Add(new Token(TokenType.Plus, "+"));
                    break;
                case '-':
                    Advance();
                    if (Match('>'))
                        tokens.Add(new Token(TokenType.MinusGreater, "->"));
                    else
                        tokens.Add(new Token(TokenType.Minus, "-"));
                    break;
                case ',':
                    Advance();
                    tokens.Add(new Token(TokenType.Comma, ","));
                    break;
                case '*':
                    Advance();
                    tokens.Add(new Token(TokenType.Star, "*"));
                    break;
                case '/':
                    Advance();
                    tokens.Add(new Token(TokenType.Slash, "/"));
                    break;
                case '%':
                    Advance();
                    tokens.Add(new Token(TokenType.Percent, "%"));
                    break;
                case '&':
                    Advance();
                    tokens.Add(new Token(TokenType.Ampersand, "&"));
                    break;
                case '!':
                    Advance();
                    if (Match('='))
                        tokens.Add(new Token(TokenType.BangEqual, "!="));
                    else
                        tokens.Add(new Token(TokenType.Bang, "!"));
                    break;
                case '|':
                    Advance();
                    tokens.Add(new Token(TokenType.Pipe, "|"));
                    break;
                case '@':
                    Advance();
                    tokens.Add(new Token(TokenType.At, "@"));
                    break;
                case ';':
                    Advance();
                    tokens.Add(new Token(TokenType.SemiColon, ";"));
                    break;
                case ':':
                    Advance();
                    tokens.Add(new Token(TokenType.Colon, ":"));
                    break;
                case '=':
                    Advance();
                    if (Match('='))
                        tokens.Add(new Token(TokenType.EqualEqual, "=="));
                    else
                        tokens.Add(new Token(TokenType.Equal, "="));
                    break;
                case '<':
                    Advance();
                    if (Match('<'))
                        tokens.Add(new Token(TokenType.LessLess, "<<"));
                    else if (Match('='))
                        tokens.Add(new Token(TokenType.LessEqual, "<="));
                    else
                        tokens.Add(new Token(TokenType.Less, "<"));
                    break;
                case '>':
                    Advance();
                    if (Match('>'))
                        tokens.Add(new Token(TokenType.GreaterGreater, ">>"));
                    else if (Match('='))
                        tokens.Add(new Token(TokenType.GreaterEqual, ">>"));
                    else
                        tokens.Add(new Token(TokenType.Greater, ">"));
                    break;
                case '[':
                    Advance();
                    tokens.Add(new Token(TokenType.OpenBracket, "["));
                    break;
                case ']':
                    Advance();
                    tokens.Add(new Token(TokenType.CloseBracket, "]"));
                    break;
                case '(':
                    Advance();
                    tokens.Add(new Token(TokenType.OpenParen, "("));
                    break;
                case ')':
                    Advance();
                    tokens.Add(new Token(TokenType.CloseParen, ")"));
                    break;
                case '{':
                    Advance();
                    tokens.Add(new Token(TokenType.OpenBrace, "{"));
                    break;
                case '}':
                    Advance();
                    tokens.Add(new Token(TokenType.CloseBrace, "}"));
                    break;
                case '#':
                    var placeholderStartPosition = _position;
                    Advance();

                    while (Current is >= '0' and <= '9')
                        Advance();

                    var placeholderString = _source[placeholderStartPosition.._position];
                    var placeholderIndex = placeholderString.Length > 1
                        ? int.Parse(placeholderString[1..])
                        : 0;

                    tokens.Add(new Token(TokenType.Placeholder, placeholderString, placeholderIndex));
                    break;
                case >= '0' and <= '9':
                    var numberStartPosition = _position;

                    while (Current is >= '0' and <= '9')
                        Advance();

                    var numberString = _source[numberStartPosition.._position];
                    var number = int.Parse(numberString);

                    tokens.Add(new Token(TokenType.Number, numberString, number));
                    break;
                case >= 'a' and <= 'z' or >= 'A' and <= 'Z':
                    var identifierStartPosition = _position;
                    while (Current is >= 'a' and <= 'z' or >= 'A' and <= 'Z' or '_')
                        Advance();

                    var text = _source[identifierStartPosition.._position];
                    switch (text)
                    {
                        case "true":
                            tokens.Add(new Token(TokenType.Boolean, text, true));
                            break;
                        case "false":
                            tokens.Add(new Token(TokenType.Boolean, text, false));
                            break;
                        default:
                            tokens.Add(new Token(TokenType.Identifier, text));
                            break;
                    }
                    break;
                case '\'':
                    var charStartPosition = _position;
                    Advance();

                    char charValue;

                    if (Match('\\'))
                    {
                        if (Match('n'))
                            charValue = '\n';
                        else if (Match('r'))
                            charValue = '\r';
                        else if (Match('t'))
                            charValue = '\t';
                        else if (Match('\\'))
                            charValue = '\\';
                        else if (Match('\''))
                            charValue = '\'';
                        else
                            throw new InvalidOperationException($"Unknown escape sequence: '\\{Current}'");
                    }
                    else
                    {
                        charValue = Current;
                        Advance();
                    }

                    Consume('\'');

                    tokens.Add(new Token(TokenType.Char, _source[charStartPosition.._position], charValue));
                    break;
                case '"':
                    var stringStartPosition = _position;
                    Advance();

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

                            if (Match('\\'))
                            {
                                _stringValue.Append('\\');
                                continue;
                            }

                            throw new InvalidOperationException($"Unknown escape sequence: '\\{Current}'");
                        }

                        _stringValue.Append(Current);
                        Advance();
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

        Advance();
        return true;
    }

    private void Consume(char expected)
    {
        if (Current != expected)
            throw new InvalidOperationException($"Expected '{expected}' but found '{Next}'");

        Advance();
    }

    private void Advance() => _position++;

    private bool IsEndOfFile => _position >= _source.Length;

    private char Current => _position < _source.Length ? _source[_position] : '\0';
    private char Next => _position < _source.Length - 1 ? _source[_position + 1] : '\0';
}

public sealed record Token(TokenType Type, string Text, object? Literal = null);

public enum TokenType
{
    // Literals
    Number,
    String,
    Char,
    Boolean,
    Identifier,
    Placeholder,

    // Symbols
    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Ampersand,
    Bang,
    BangEqual,
    Plus,
    PlusPlus,
    Minus,
    MinusGreater,
    Star,
    Slash,
    Percent,
    Pipe,
    At,
    Comma,
    Colon,
    SemiColon,
    Less,
    LessLess,
    LessEqual,
    Greater,
    GreaterGreater,
    GreaterEqual,
    Equal,
    EqualEqual,

    // Synthetic
    Eof,
}
