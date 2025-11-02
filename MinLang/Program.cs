const string code = "1 + 212 * 34";

var lexer = new Lexer();
var parser = new Parser();
var compiler = new Compiler();
var runtime = new Runtime();
var interpreter = new Interpreter();

var tokens = lexer.Lex(code);
var expression = parser.Parse(tokens);
var instructions = compiler.Compile(expression);

Console.WriteLine("Interpreted:");
Console.WriteLine(interpreter.Evaluate(expression));

Console.WriteLine("Compiled:");
Console.WriteLine(runtime.Run(instructions));

public enum TokenKind
{
    Number,
    Plus,
    Star,

    Error,
    EndOfFile
}

public record Token(TokenKind Kind, string Text);

public class Lexer
{
    public List<Token> Lex(string source)
    {
        var tokens = new List<Token>();
        var current = 0;

        while (current < source.Length)
        {
            var start = current;
            
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
                    while (current < source.Length && char.IsDigit(source[current]))
                        current++;
                    
                    tokens.Add(new Token(TokenKind.Number, source[start..current]));
                    break;
                default:
                    current++;
                    tokens.Add(new Token(TokenKind.Error, source[start..current]));
                    break;
            }
        }
        
        tokens.Add(new Token(TokenKind.EndOfFile, ""));
        return tokens;
    }
}

public abstract record Expression;
public record LiteralExpression(Token Value) : Expression;
public record BinaryExpression(Expression Left, Token Operator, Expression Right) : Expression;

public class Parser
{
    private List<Token> _tokens = [];
    private int _position;
    
    public Expression Parse(List<Token> tokens)
    {
        _tokens = tokens;
        return Factor();
    }
    
    private Expression Factor()
    {
        var expr = Term();

        return AdvanceIf(TokenKind.Star) 
            ? new BinaryExpression(expr, PreviousToken, Term()) 
            : expr;
    }

    private Expression Term()
    {
        var expr = Primary();   
        
        return AdvanceIf(TokenKind.Plus) 
            ? new BinaryExpression(expr,  PreviousToken, Primary()) 
            : expr;
    }
    
    private LiteralExpression Primary() =>
        AdvanceIf(TokenKind.Number)
            ? new LiteralExpression(PreviousToken)
            : throw new InvalidOperationException("Unexpected token");

    private bool AdvanceIf(TokenKind kind)
    {
        if (Token.Kind == kind)
        {
            _position++;
            return true;
        }

        return false;
    }

    private Token Token => _tokens[_position];
    private Token PreviousToken => _tokens[_position - 1];
}

class Interpreter
{
    public int Evaluate(Expression expression) =>
        expression switch
        {
            BinaryExpression { Operator.Kind: TokenKind.Plus } binExpr => Evaluate(binExpr.Left) +
                                                                          Evaluate(binExpr.Right),
            BinaryExpression { Operator.Kind: TokenKind.Star } binExpr => Evaluate(binExpr.Left) *
                                                                          Evaluate(binExpr.Right),
            LiteralExpression { Value.Kind: TokenKind.Number } litEpr => int.Parse(litEpr.Value.Text),
            _ => throw new InvalidOperationException("Unexpected expression")
        };
}

class Compiler
{
    public List<Instruction> Compile(Expression expression)
    {
        var instructions = new List<Instruction>();
       
        switch (expression)
        {
            case BinaryExpression binaryExpression:
                instructions.AddRange(Compile(binaryExpression.Left));
                instructions.AddRange(Compile(binaryExpression.Right));
                
                switch (binaryExpression.Operator.Kind)
                {
                    case TokenKind.Plus:
                        instructions.Add(new AddInstruction());
                        break;
                    case TokenKind.Star:
                        instructions.Add(new MulInstruction());
                        break;
                    default:
                        throw new InvalidOperationException("Unexpected operator token");
                }
                break;
            case LiteralExpression numericLiteralExpression:
                switch (numericLiteralExpression.Value.Kind)
                {
                    case TokenKind.Number:
                        instructions.Add(new LoadNumberInstruction(int.Parse(numericLiteralExpression.Value.Text)));
                        break;
                    default:
                        throw new InvalidOperationException("Unecxpected literal token");
                }
                break;
            default:
                throw new ArgumentOutOfRangeException(nameof(expression));
        }

        return instructions;
    }
}

abstract record Instruction
{
    public abstract void Execute(Stack<int> stack);
}

record LoadNumberInstruction(int Value) : Instruction
{
    public override void Execute(Stack<int> stack)
    {
        stack.Push(Value);
    }
}

record AddInstruction : Instruction
{
    public override void Execute(Stack<int> stack)
    {
        var right = stack.Pop();
        var left = stack.Pop();
        stack.Push(left + right);
    }
}

record MulInstruction : Instruction
{
    public override void Execute(Stack<int> stack)
    {
        var right = stack.Pop();
        var left = stack.Pop();
        stack.Push(left * right);
    }
}

class Runtime
{
    public int Run(List<Instruction> instructions)
    {
        var stack = new Stack<int>();
        
        foreach (var instruction in instructions)
            instruction.Execute(stack);

        return stack.Pop();
    }
}
