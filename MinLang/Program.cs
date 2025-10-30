const string code = "1 2 + 34 *";

var lexer = new Lexer(code);

while (true)
{
    var token = lexer.Lex();
    if (token.Kind == TokenKind.EndOfFile)
        break;
    
    Console.WriteLine(token);
}

public enum TokenKind
{
    Invalid,
    Number,
    Plus,
    Star,
    EndOfFile
}

public record Token(TokenKind Kind, string Text);

public class Lexer(string source)
{
    private int _position;
    
    public Token Lex()
    {
        SkipWhitespace();
        
        var start = _position;

        TokenKind kind;
        switch (Current)
        {
            case '\0':
                kind = TokenKind.EndOfFile;
                break;
            case '+':
                kind = TokenKind.Plus;
                _position++;
                break;
            case '*':
                kind = TokenKind.Star;
                _position++;
                break;
            case >= '0' and <= '9':
                kind = LexNumber();
                break;
            default:
                kind = TokenKind.Invalid;
                _position++;
                break;
        }

        var text = _position > source.Length ? "" : source[start.._position];
        return new Token(kind, text);
    }

    private char Current
    {
        get
        {
            if (_position < source.Length)
                return source[_position];
            
            return '\0';
        }
    }

    private TokenKind LexNumber()
    {
        while (char.IsDigit(Current))
            _position++;
        
        return TokenKind.Number;
    }
    
    private void SkipWhitespace()
    {
        while (char.IsWhiteSpace(Current))
            _position++;
    }
}

public abstract record Node;
public record AddExpression(Node Left, Node Right) : Node;
public record MultiplyExpression(Node Left, Node Right) : Node;

class Parser
{
}

class Compiler
{
}

class Interpreter
{
}