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

public class Lexer
{
    public List<Token> Lex(string source)
    {
        var tokens = new List<Token>();
        var start = 0;
        var current = 0;

        while (current < source.Length)
        {
            start = current;

            switch (source[current])
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
                    while (char.IsDigit(source[current]))
                        current++;

                    tokens.Add(new Token(TokenKind.Number, source[start..current]));
                    break;
                default:
                    tokens.Add(new Token(TokenKind.Invalid, source[start..current]));
                    break;
            }
        }
        
        tokens.Add(new Token(TokenKind.EndOfFile, ""));
        return tokens;
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