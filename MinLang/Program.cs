﻿const string code = "1 + 212 * 34";

var lexer = new Lexer();
var parser = new Parser();
var compiler = new Compiler();
var runtime = new Runtime();

var tokens = lexer.Lex(code);
var nodes = parser.Parse(tokens);
var instructions = compiler.Compile(nodes);
Console.WriteLine(runtime.Run(instructions));

public enum TokenKind
{
    Invalid,
    Number,
    Plus,
    Star,
    EndOfFile
}

public record Token(TokenKind Kind, string Text);

public record Lexer
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
                    tokens.Add(new Token(TokenKind.Invalid, source[start..current]));
                    break;
            }
        }
        
        tokens.Add(new Token(TokenKind.EndOfFile, ""));
        return tokens;
    }
}

public abstract record Node;
public record LiteralExpression(Token Value) : Node;
public record BinaryExpression(Node Left, Token Operator, Node Right) : Node;

public record Parser
{
    private List<Token> _tokens = [];
    private int _index;
    
    public List<Node> Parse(List<Token> tokens)
    {
        _tokens = tokens;
        
        var nodes = new List<Node>();
        
        while (!IsEndOfFile)
            nodes.Add(Factor());
        
        return nodes;
    }
    
    private Node Factor()
    {
        var expr = Term();

        return Match(TokenKind.Star) 
            ? new BinaryExpression(expr, PreviousToken, Term()) 
            : expr;
    }

    private Node Term()
    {
        var expr = Primary();   
        
        return Match(TokenKind.Plus) 
            ? new BinaryExpression(expr,  PreviousToken, Primary()) 
            : expr;
    }
    
    private Node Primary() =>
        Match(TokenKind.Number)
            ? new LiteralExpression(PreviousToken)
            : throw new InvalidOperationException("Unexpected token");

    private bool Match(TokenKind kind)
    {
        if (Token.Kind == kind)
        {
            _index++;
            return true;
        }

        return false;
    }
    
    private bool IsEndOfFile => Token.Kind == TokenKind.EndOfFile;

    private Token Token => _tokens[_index];
    private Token PreviousToken => _tokens[_index - 1];
}

class Compiler
{
    private List<Instruction> _instructions = [];

    public List<Instruction> Compile(List<Node> nodes)
    {
        _instructions = [];
        
        foreach (var node in nodes)
            Compile(node);

        return _instructions;
    }

    private void Compile(Node node)
    {
        switch (node)
        {
            case BinaryExpression binaryExpression:
                Compile(binaryExpression.Left);
                Compile(binaryExpression.Right);
                
                switch (binaryExpression.Operator.Kind)
                {
                    case TokenKind.Plus:
                        _instructions.Add(new AddInstruction());
                        break;
                    case TokenKind.Star:
                        _instructions.Add(new MulInstruction());
                        break;
                    default:
                        throw new InvalidOperationException("Unexpected operator token");
                }
                break;
            case LiteralExpression numericLiteralExpression:
                switch (numericLiteralExpression.Value.Kind)
                {
                    case TokenKind.Number:
                        _instructions.Add(new LoadNumberInstruction(int.Parse(numericLiteralExpression.Value.Text)));
                        break;
                    default:
                        throw new InvalidOperationException("Unecxpected literal token");
                }
                break;
            default:
                throw new ArgumentOutOfRangeException(nameof(node));
        }
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
    private readonly Stack<int> _stack = new();

    public int Run(List<Instruction> instructions)
    {
        foreach (var instruction in instructions)
            instruction.Execute(_stack);

        return _stack.Pop();
    }
}