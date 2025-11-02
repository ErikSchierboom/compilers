const string code = """
                    var x = 1;
                    var y = 212;
                    var z = x + y * 34;
                    """;

var lexer = new Lexer();
var tokens = lexer.Lex(code);

var parser = new Parser(tokens);
var statements = parser.Parse();
// var compiler = new Compiler();
// var instructions = compiler.Compile(expression);
// var runtime = new Runtime();
// var interpreter = new Interpreter();
// Console.WriteLine("Interpreted:");
// Console.WriteLine(interpreter.Evaluate(expression));
//
// Console.WriteLine("Compiled:");
// Console.WriteLine(runtime.Run(instructions));

public enum TokenKind
{
    Number,
    Identifier,
    
    Semicolon,
    Plus,
    Star,
    Equal,
    
    Var,

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
                case ';':
                    current++;
                    tokens.Add(new Token(TokenKind.Semicolon, ";"));
                    break;
                case '=':
                    current++;
                    tokens.Add(new Token(TokenKind.Equal, "="));
                    break;
                case ' ' or '\t' or '\r' or '\n':
                    current++;
                    break;
                case >= '0' and <= '9':
                    while (current < source.Length && char.IsDigit(source[current]))
                        current++;
                    
                    tokens.Add(new Token(TokenKind.Number, source[start..current]));
                    break;
                case >= 'a' and <= 'z' or >= 'A' and <= 'Z':
                    while (current < source.Length && char.IsAsciiLetterOrDigit(source[current]))
                        current++;
                    
                    var identifier = source[start..current];
                    if (identifier == "var")
                        tokens.Add(new Token(TokenKind.Var, identifier));
                    else
                        tokens.Add(new Token(TokenKind.Identifier, identifier));
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

public abstract record Node;

public abstract record Statement : Node;
public record AssignmentStatement(Token Name, Expression Initializer) : Statement;
public record ExpressionStatement(Expression Expression) : Statement;

public abstract record Expression : Node;
public record VariableExpression(Token Name) : Expression;
public record LiteralExpression(Token Value) : Expression;
public record BinaryExpression(Expression Left, Token Operator, Expression Right) : Expression;

public class Parser(List<Token> tokens)
{
    private int _position;

    public List<Statement> Parse()
    {
        var statements = new List<Statement>();

        while (!IsEndOfFile)
            statements.Add(Statement());
        
        return statements;
    }

    private bool IsEndOfFile => Token.Kind == TokenKind.EndOfFile;

    private Statement Statement()
    {
        if (Match(TokenKind.Var))
            return VariableDeclarationStatement();

        return ExpressionStatement();
    }

    private Statement VariableDeclarationStatement()
    {
        Consume(TokenKind.Identifier);
        var name = PreviousToken;
        Consume(TokenKind.Equal);
        var initializer = Expression();
        Consume(TokenKind.Semicolon);
        
        return new AssignmentStatement(name, initializer);
    }

    private Statement ExpressionStatement()
    {
        var expression = Expression();
        Consume(TokenKind.Semicolon);
        return new ExpressionStatement(expression);
    }

    private Expression Expression()
    {
        var expr = Term();

        return Match(TokenKind.Star) 
            ? new BinaryExpression(expr, PreviousToken, Term()) 
            : expr;
    }

    private Expression Term()
    {
        var expr = Primary();   
        
        return Match(TokenKind.Plus) 
            ? new BinaryExpression(expr,  PreviousToken, Primary()) 
            : expr;
    }
    
    private Expression Primary()
    {
        if (Match(TokenKind.Number))
            return new LiteralExpression(PreviousToken);
        
        if (Match(TokenKind.Identifier))
            return new VariableExpression(PreviousToken);
        
        throw new InvalidOperationException("Unexpected token");
    }

    private bool Match(TokenKind kind)
    {
        if (Token.Kind == kind)
        {
            _position++;
            return true;
        }

        return false;
    }
    
    private void Consume(TokenKind kind)
    {
        if (Token.Kind != kind)
            throw new InvalidOperationException($"Expected '{kind}' token");
        
        _position++;
    }

    private Token Token => tokens[_position];
    private Token PreviousToken => tokens[_position - 1];
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
