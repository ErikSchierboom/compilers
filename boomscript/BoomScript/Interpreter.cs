namespace BoomScript;

public record StackFrame(Dictionary<string, object?> Variables, StackFrame? Parent = null)
{
    public object? this[string name]
    {
        get => Variables.TryGetValue(name, out var value) ? value : Parent?[name];
        set => Variables[name] = value ?? throw new ArgumentNullException(nameof(value));
    }

    public StackFrame CreateChild() => new(new Dictionary<string, object?>(), this);
}

public class Interpreter : SyntaxVisitor<object?>
{
    private readonly Stack<StackFrame> _stackFrames = new([new StackFrame(new Dictionary<string, object?>())]);
    private StackFrame CurrentStackFrame => _stackFrames.Peek();
    
    public object? Run(string code)
    {
        var syntaxTree = SyntaxTree.Parse(code);
        return Visit(syntaxTree.Root);
    }

    public override object? VisitCompilationUnit(CompilationUnit node)
    {
        object? result = null;
    
        foreach (var statement in node.Statements)
            result = Visit(statement);
        
        return result;
    }

    public override object? VisitVariableDeclarationStatement(VariableDeclarationStatement node) =>
        CurrentStackFrame.Variables[node.Identifier.Text] = Visit(node.Initializer);

    public override object? VisitExpressionStatement(ExpressionStatement node) =>
        Visit(node.Expression);

    public override object? VisitBinaryExpression(BinaryExpression node)
    {
        // TODO: handle invalid types
        var left = (int)Visit(node.Left);
        var right = (int)Visit(node.Right);
        
        return node.OperatorToken.Kind switch
        {
            SyntaxKind.PlusToken => left + right,
            SyntaxKind.MinusToken => left - right,
            SyntaxKind.StarToken => left * right,
            SyntaxKind.SlashToken => left / right,
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    public override object? VisitLiteralExpression(LiteralExpression node) =>
        node.LiteralToken.Kind switch
        {
            SyntaxKind.NumberToken => node.Value,
            _ => throw new ArgumentOutOfRangeException()
        };


    public override object? VisitParenthesizedExpression(ParenthesizedExpression node) => Visit(node.Expression);

    public override object? VisitUnaryExpression(UnaryExpression node)
    {
        return node.OperatorToken.Kind switch
        {
            // TODO: handle invalid types
            SyntaxKind.PlusToken => (int)Visit(node.Operand),
            SyntaxKind.MinusToken => -(int)Visit(node.Operand),
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    public override object? VisitNameExpression(NameExpression node) =>
        CurrentStackFrame[node.IdentifierToken.Text];

    public override object? VisitCallExpression(CallExpression node) => throw new NotImplementedException();
}
