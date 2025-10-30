const string code = "1 212 + 34 *";

var lexer = new Lexer();
foreach (var token in lexer.Lex(code))
    Console.WriteLine(token);

public enum TokenKind
{
    Invalid,
    Number,
    Plus,
    Star,
    EndOfFile
}

public record Token(TokenKind Kind, string Text);

public record Lexer(string Source)
{
    public List<Token> Lex()
    {
        var tokens = new List<Token>();
        var start = 0;
        var current = 0;

        while (current < Source.Length)
        {
            start = current;

            switch (Source[current])
            {
                case '+':
                    current++;
                    tokens.Add(new Token(TokenKind.Plus, "+"));
                    break;
                case '*':
                    current++;
                    tokens.Add(new Token(TokenKind.Star, "*"));
                    break;
                case ' ' or '\t' or '\r' or '\n':
                    current++;
                    break;
                case >= '0' and <= '9':
                    while (char.IsDigit(Source[current]))
                        current++;

                    tokens.Add(new Token(TokenKind.Number, Source[start..current]));
                    break;
                default:
                    tokens.Add(new Token(TokenKind.Invalid, Source[start..current]));
                    break;
            }
        }
        
        tokens.Add(new Token(TokenKind.EndOfFile, ""));
        return tokens;
    }
}

public abstract record Node;
public record NumberLiteral(int Value) : Node;
public record AddExpression(Node Left, Node Right) : Node;
public record MultiplyExpression(Node Left, Node Right) : Node;

public record Parser(List<Token> Tokens)
{
    public Node Parse()
    {
        throw new NotImplementedException();
    }

    private Node Term()
    {
        throw new NotImplementedException();
    }
    
    private Node Literal(Token token)
    {
        return token.Kind switch
        {
            TokenKind.Number => new NumberLiteral(int.Parse(token.Text)),
            _ => throw new InvalidOperationException("Unexpected token")
        };
    }
}

class Compiler
{
}

class Interpreter
{
}