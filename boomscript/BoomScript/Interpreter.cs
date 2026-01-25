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

public class Interpreter
{
    private readonly Stack<StackFrame> _stackFrames = new([new StackFrame(new Dictionary<string, object?>())]);
    private StackFrame CurrentStackFrame => _stackFrames.Peek();
    
    public object? Run(string code)
    {
        var syntaxTree = SyntaxTree.Parse(code);

        // TODO: create visitor
        return Evaluate(syntaxTree.Root);
    }

    private object? Evaluate(CompilationUnit compilationUnit)
    {
        object? result = null;

        foreach (var statement in compilationUnit.Statements)
            result = Evaluate(statement);
        
        return result;
    }

    private object? Evaluate(Statement expression) =>
        expression switch
        {
            ExpressionStatement expressionStatement => Evaluate(expressionStatement),
            VariableDeclarationStatement variableDeclarationStatement => Evaluate(variableDeclarationStatement),
            _ => throw new ArgumentOutOfRangeException(nameof(expression))
        };
    
    private object? Evaluate(VariableDeclarationStatement variableDeclarationStatement) =>
        CurrentStackFrame.Variables[variableDeclarationStatement.Identifier.Text] = Evaluate(variableDeclarationStatement.Initializer);

    private object? Evaluate(ExpressionStatement expressionStatement) =>
        Evaluate(expressionStatement.Expression);

    private object? Evaluate(Expression expression) =>
        expression switch
        {
            BinaryExpression binaryExpression => Evaluate(binaryExpression),
            LiteralExpression literalExpression => Evaluate(literalExpression),
            ParenthesizedExpression parenthesizedExpression => Evaluate(parenthesizedExpression),
            UnaryExpression unaryExpression => Evaluate(unaryExpression),
            NameExpression nameExpression => Evaluate(nameExpression),
            CallExpression callExpression => Evaluate(callExpression),
            _ => throw new ArgumentOutOfRangeException(nameof(expression))
        };

    private object? Evaluate(BinaryExpression expression)
    {
        // TODO: handle invalid types
        var left = (int)Evaluate(expression.Left);
        var right = (int)Evaluate(expression.Right);
        
        return expression.OperatorToken.Kind switch
        {
            SyntaxKind.PlusToken => left + right,
            SyntaxKind.MinusToken => left - right,
            SyntaxKind.StarToken => left * right,
            SyntaxKind.SlashToken => left / right,
            _ => throw new ArgumentOutOfRangeException()
        };
    }
    
    private object? Evaluate(LiteralExpression expression) =>
        expression.LiteralToken.Kind switch
        {
            SyntaxKind.NumberToken => expression.Value,
            _ => throw new ArgumentOutOfRangeException()
        };

    private object? Evaluate(ParenthesizedExpression expression) =>
        Evaluate(expression.Expression);
    
    private object? Evaluate(UnaryExpression expression)
    {
        return expression.OperatorToken.Kind switch
        {
            // TODO: handle invalid types
            SyntaxKind.PlusToken => (int)Evaluate(expression.Operand),
            SyntaxKind.MinusToken => -(int)Evaluate(expression.Operand),
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    private object? Evaluate(NameExpression expression) =>
        CurrentStackFrame[expression.IdentifierToken.Text];
    
    private object? Evaluate(CallExpression expression) =>
        throw new NotImplementedException();
}
