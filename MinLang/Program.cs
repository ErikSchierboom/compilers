const string code = "1 + 212 * 34";

var lexer = new Lexer(code);
foreach (var token in lexer.Lex())
    Console.WriteLine(token);

var parser = new Parser(lexer.Lex());
foreach (var node in parser.Parse())
    Console.WriteLine(node);

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
        var current = 0;

        while (current < Source.Length)
        {
            var start = current;

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
                    while (current < Source.Length && char.IsDigit(Source[current]))
                        current++;
                    
                    tokens.Add(new Token(TokenKind.Number, Source[start..current]));
                    break;
                default:
                    current++;
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
    private int index = 0;
    
    public List<Node> Parse()
    {
        var nodes = new List<Node>();
        
        while (!IsEndOfFile)
            nodes.Add(Factor());
        
        return nodes;
    }
    
    private Node Factor()
    {
        Node expr = Term();

        return Match(TokenKind.Star) 
            ? new MultiplyExpression(expr, Term()) 
            : expr;
    }

    private Node Term()
    {
        var expr = Primary();   
        
        return Match(TokenKind.Plus) 
            ? new AddExpression(expr, Primary()) 
            : expr;
    }
    
    private Node Primary()
    {
        return Match(TokenKind.Number)
            ? new NumberLiteral(int.Parse(PreviousToken.Text))
            : throw new InvalidOperationException("Unexpected token");
    }

    private bool Match(TokenKind kind)
    {
        if (Token.Kind == kind)
        {
            index++;
            return true;
        }

        return false;
    }
    
    private bool IsEndOfFile => Token.Kind == TokenKind.EndOfFile;

    private Token Token => Tokens[index];
    private Token PreviousToken => Tokens[index - 1];
}

class Compiler
{
}

class Interpreter
{
}