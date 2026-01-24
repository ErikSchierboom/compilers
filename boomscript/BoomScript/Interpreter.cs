namespace BoomScript;

public class Interpreter
{
    public object? Run(string code)
    {
        var sourceText = new SourceText(code);
        var syntaxTree = new SyntaxTree(sourceText);
        var parser = new Parser(syntaxTree);
        var compilationUnit = parser.ParseCompilationUnit();
        
        // TODO: create visitor
        return Evaluate(compilationUnit);
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
            ExpressionStatement expressionStatement => Evaluate(expressionStatement.Expression),
            _ => throw new ArgumentOutOfRangeException(nameof(expression))
        };

    private object? Evaluate(Expression expression) =>
        expression switch
        {
            BinaryExpression binaryExpression => Evaluate(binaryExpression),
            LiteralExpression literalExpression => Evaluate(literalExpression),
            ParenthesizedExpression parenthesizedExpression => Evaluate(parenthesizedExpression),
            UnaryExpression unaryExpression => Evaluate(unaryExpression),
            _ => throw new ArgumentOutOfRangeException(nameof(expression))
        };

    private object? Evaluate(BinaryExpression expression)
    {
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
            SyntaxKind.PlusToken => (int)Evaluate(expression.Operand),
            SyntaxKind.MinusToken => -(int)Evaluate(expression.Operand),
            _ => throw new ArgumentOutOfRangeException()
        };
    }
}
