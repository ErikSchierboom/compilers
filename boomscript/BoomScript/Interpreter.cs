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

public record Function(string[] Parameters, BlockStatement Body);

public class Interpreter : SyntaxVisitor<object?>
{
    private readonly Stack<StackFrame> _stackFrames = new([new StackFrame(new Dictionary<string, object?>())]);
    private StackFrame CurrentStackFrame => _stackFrames.Peek();
    
    public object? Run(string code)
    {
        var syntaxTree = SyntaxTree.Parse(code);
        return Visit(syntaxTree.Root);
    }

    public override object? VisitCompilationUnitSyntax(CompilationUnitSyntax node)
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

    public override object? VisitCallExpression(CallExpression node)
    {
        // TODO: add tail-call recursion
        if (CurrentStackFrame[node.Name.Text] is not Function function)
            throw new InvalidOperationException($"Function '{node.Name.Text}' is not a function."); // TODO: proper error handling

        var functionFrame = CurrentStackFrame.CreateChild();
        // TODO: check number of arguments

        foreach (var (name, value) in function.Parameters.Zip(node.Arguments, (name, arg) => (name, arg)))
            functionFrame[name] = Visit(value);
        
        _stackFrames.Push(functionFrame);
        var visit = Visit(function.Body);
        _stackFrames.Pop();
        
        return visit;
    }

    public override object? VisitFunctionDeclarationStatement(FunctionDeclarationStatement node) => 
        CurrentStackFrame[node.Identifier.Text] = new Function(node.Parameters.Select(param => param.Name.Text).ToArray(), node.Body);

    public override object? VisitBlockStatement(BlockStatement node)
    {
        object? result = null;
    
        foreach (var statement in node.Statements)
            result = Visit(statement);
        
        return result;
    }
}
