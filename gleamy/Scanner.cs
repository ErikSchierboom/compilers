namespace Gleamy;

internal sealed class Scanner(string source)
{
    private static readonly Dictionary<string, TokenType> Keywords = new()
    {
        ["let"] = TokenType.LetKeyword,
        ["fn"] = TokenType.FnKeyword,
        ["match"] = TokenType.MatchKeyword,
        ["case"] = TokenType.CaseKeyword,
        ["Int"] = TokenType.IntKeyword,
    };
    
    public List<Token> Scan()
    {
        var tokens = new List<Token>();
        
        var position = 0;
        while (position < source.Length)
        {
            switch (source[position])
            {
                case ' ' or '\t' or '\r':
                    position++;
                    break;
                case '\n':
                    tokens.Add(new Token(TokenType.Newline, "\n"));
                    position++;
                    break;
                case '+':
                    tokens.Add(new Token(TokenType.Plus, "+"));
                    position++;
                    break;
                case '*':
                    tokens.Add(new Token(TokenType.Star, "*"));
                    position++;
                    break;
                case '/':
                    tokens.Add(new Token(TokenType.Slash, "/"));
                    position++;
                    break;
                case ',':
                    tokens.Add(new Token(TokenType.Comma, ","));
                    position++;
                    break;
                case ':':
                    tokens.Add(new Token(TokenType.Colon, ":"));
                    position++;
                    break;
                case '_':
                    tokens.Add(new Token(TokenType.Underscore, "_"));
                    position++;
                    break;
                case '(':
                    tokens.Add(new Token(TokenType.OpenParen, "("));
                    position++;
                    break;
                case ')':
                    tokens.Add(new Token(TokenType.CloseParen, ")"));
                    position++;
                    break;
                case '{':
                    tokens.Add(new Token(TokenType.OpenBracket, "{"));
                    position++;
                    break;
                case '}':
                    tokens.Add(new Token(TokenType.CloseBracket, "}"));
                    position++;
                    break;
                case '=':
                    if (position < source.Length - 1 || source[position + 1] == '>')
                    {
                        tokens.Add(new Token(TokenType.EqualGreaterThan, "=>"));
                        position += 2;
                    }
                    else
                    {
                        tokens.Add(new Token(TokenType.Equal, "="));
                        position++;
                    }
                    break;
                case '-':
                    if (position < source.Length - 1 || source[position + 1] == '>')
                    {
                        tokens.Add(new Token(TokenType.MinusGreaterThan, "->"));
                        position += 2;
                    }
                    else
                    {
                        tokens.Add(new Token(TokenType.Minus, "-"));
                        position++;
                    }
                    break;
                case >= '0' and <= '9':
                    var numberStartPosition = position;
                    while (position < source.Length && source[position] is >= '0' and <= '9')
                        position++;
                    
                    tokens.Add(new Token(TokenType.Number, source[numberStartPosition..position]));
                    break;
                case >= 'a' and <= 'z' or >= 'A' and <= 'Z':
                    var identifierStartPosition = position;
                    while (position < source.Length && source[position] is >= 'a' and <= 'z' or >= 'A' and <= 'Z')
                        position++;

                    var text = source[identifierStartPosition..position];
                    if (Keywords.TryGetValue(text, out var tokenType))
                        tokens.Add(new Token(tokenType, text));    
                    else
                        tokens.Add(new Token(TokenType.Identifier, text));
                    break;
                default:
                    throw new InvalidOperationException($"Unknown character: '{source[position]}'");
            }
        }
        
        tokens.Add(new Token(TokenType.Eof, ""));
        
        return tokens;
    }
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
    
    Newline,
    Eof,
}
