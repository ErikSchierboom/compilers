namespace Gleamy;

internal sealed class Binder(SyntaxTree tree)
{
    private BoundScope _scope = new();
    
    public BoundProgram Bind()
    {
        List<BoundStatement> boundStatements = new();
        Dictionary<FunctionSymbol, BoundBlockStatement> boundFunctions = new();

        foreach (var statement in tree.Statements)
        {
            switch (BindStatement(statement))
            {
                case BoundFunctionDeclarationStatement boundFunctionDeclarationStatement:
                    boundFunctions.Add(boundFunctionDeclarationStatement.Binding, boundFunctionDeclarationStatement.Body);
                    break;
                case BoundBindingDeclarationStatement boundBindingDeclarationStatement:
                    boundStatements.Add(boundBindingDeclarationStatement);
                    break;
                case BoundBlockStatement boundBlockStatement:
                    boundStatements.AddRange(boundBlockStatement.Statements);
                    break;
                case BoundExpressionStatement boundExpressionStatement:
                    boundStatements.Add(boundExpressionStatement);
                    break;
                default:
                    throw new InvalidOperationException("Cannot bind statement");
            }
        }

        return new BoundProgram(boundFunctions, [..boundStatements]);
    }
    
    private BoundStatement BindStatement(Statement statement)
    {
        switch (statement)
        {
            case ExpressionStatement expressionStatement:
                return BindExpressionStatement(expressionStatement);
            case BlockStatement blockStatement:
                return BindBlockStatement(blockStatement);
            case BindingDeclarationStatement bindingDeclarationStatement:
                return BindDeclarationStatement(bindingDeclarationStatement);
            case FunctionDeclarationStatement functionDeclarationStatement:
                return BindFunctionDeclarationStatement(functionDeclarationStatement);
            default:
                throw new ArgumentOutOfRangeException(nameof(statement));
        }
    }

    private BoundBindingDeclarationStatement BindDeclarationStatement(BindingDeclarationStatement bindingDeclarationStatement)
    {
        var boundExpression = BindExpression(bindingDeclarationStatement.Value);
        var bindingSymbol = new BindingSymbol(bindingDeclarationStatement.Identifier.Text, boundExpression.Type);
        
        if (!_scope.TryDeclareBinding(bindingSymbol))
            throw new InvalidOperationException($"Could not find symbol {bindingSymbol.Name}");
        
        return new BoundBindingDeclarationStatement(bindingSymbol, boundExpression);
    }

    private BoundFunctionDeclarationStatement BindFunctionDeclarationStatement(FunctionDeclarationStatement functionDeclarationStatement)
    {
        var boundReturnType = BindType(functionDeclarationStatement.ReturnValue);
        var boundParameters = new List<ParameterSymbol>(capacity: functionDeclarationStatement.Parameters.Length);
        
        _scope = new BoundScope(_scope);
                
        foreach (var parameter in functionDeclarationStatement.Parameters)
        {
            var parameterSymbol = BindParameter(parameter);
            boundParameters.Add(parameterSymbol);
            if (!_scope.TryDeclareBinding(new BindingSymbol(parameterSymbol.Name, parameterSymbol.Type)))
                throw new InvalidOperationException($"Could not find symbol {parameterSymbol.Name}");
        }

        var boundStatementBlock = BindBlockStatement(functionDeclarationStatement.Body);

        _scope = _scope.Parent!;

        var functionSymbol = new FunctionSymbol(functionDeclarationStatement.Identifier.Text, [..boundParameters], boundReturnType);
        if (!_scope.TryDeclareFunction(functionSymbol))
            throw new InvalidOperationException($"Could not find symbol {functionSymbol.Name}");
        
        return new BoundFunctionDeclarationStatement(functionSymbol, boundStatementBlock);
    }

    private BoundExpressionStatement BindExpressionStatement(ExpressionStatement expressionStatement)
    {
        var boundExpression = BindExpression(expressionStatement.Value);
        return new BoundExpressionStatement(boundExpression);
    }

    private BoundBlockStatement BindBlockStatement(BlockStatement blockStatement)
    {
        _scope = new BoundScope(_scope);
        BoundStatement[] boundStatements = [..blockStatement.Statements.Select(BindStatement)];
        _scope = _scope.Parent!;
        return new BoundBlockStatement(boundStatements);
    }

    private ParameterSymbol BindParameter(Parameter parameter)
    {
        var boundType = BindType(parameter.IdentifierType);
        return new ParameterSymbol(parameter.Identifier.Text, boundType);
    }

    private TypeSymbol BindType(IdentifierType identifier)
    {
        switch (identifier.Identifier.Type)
        {
            case TokenType.IntKeyword:
                return TypeSymbol.Int;
            default:
                throw new InvalidOperationException("Cannot bind identifier");
        }
    }

    private BoundExpression BindExpression(Expression expression)
    {
        switch (expression)
        {
            case BinaryExpression binaryExpression:
                var left = BindExpression(binaryExpression.Left);
                var right = BindExpression(binaryExpression.Right);
                var op = BindOperator(left, binaryExpression.Operator, right);
                return new  BoundBinaryExpression(left, op, right);
            case NameExpression nameExpression:
                if (!_scope.TryGetSymbol(nameExpression.Identifier.Text, out var nameSymbol))
                    throw new InvalidOperationException($"Could not find symbol {nameExpression.Identifier.Text}");

                if (nameSymbol is not BindingSymbol bindingSymbol)
                    throw new InvalidOperationException($"Invalid symbol {nameExpression.Identifier.Text}");
                
                return new BoundBindingExpression(bindingSymbol);
            case CallExpression callExpression:
                if (!_scope.TryGetSymbol(callExpression.Identifier.Text, out var symbol))
                    throw new InvalidOperationException($"Could not find symbol {callExpression.Identifier.Text}");

                if (symbol is not FunctionSymbol functionSymbol)
                    throw new InvalidOperationException($"Invalid symbol {callExpression.Identifier.Text}");
                
                BoundExpression[] boundArguments = [..callExpression.Arguments.Select(BindExpression)];
                return new BoundCallExpression(functionSymbol, boundArguments);
            case LiteralExpression literalExpression:
                switch (literalExpression.Value.Type)
                {
                    case TokenType.Number:
                        return new BoundLiteralExpression(TypeSymbol.Int, int.Parse(literalExpression.Value.Text)); 
                    default:
                        throw new InvalidOperationException("Cannot bind literal expression");    
                }
            default:
                throw new ArgumentOutOfRangeException(nameof(expression));
        }
    }

    private BoundBinaryOperator BindOperator(BoundExpression left, Token op, BoundExpression right)
    {
        if (left.Type == TypeSymbol.Int && left.Type == TypeSymbol.Int)
        {
            switch (op.Type)
            {
                case TokenType.Plus:
                    return new BoundBinaryOperator(left.Type, BoundBinaryOperatorKind.Add, right.Type, TypeSymbol.Int);
                case TokenType.Minus:
                    return new BoundBinaryOperator(left.Type, BoundBinaryOperatorKind.Subtract, right.Type, TypeSymbol.Int);
                case TokenType.Star:
                    return new BoundBinaryOperator(left.Type, BoundBinaryOperatorKind.Multiply, right.Type, TypeSymbol.Int);
                case TokenType.Slash:
                    return new BoundBinaryOperator(left.Type, BoundBinaryOperatorKind.Divide, right.Type, TypeSymbol.Int);
            }
        }
        
        throw new InvalidOperationException("Cannot bind binary operator");
    }
}

internal sealed class BoundScope(BoundScope? parent = null)
{
    private readonly Dictionary<string, Symbol> _symbols = new ();
    
    public BoundScope? Parent => parent;

    public bool TryDeclareFunction(FunctionSymbol function) => _symbols.TryAdd(function.Name, function);
    public bool TryDeclareBinding(BindingSymbol binding) => _symbols.TryAdd(binding.Name, binding);

    public bool TryGetSymbol(string name, out Symbol? symbol)
    {
        if (_symbols.TryGetValue(name, out symbol))
            return true;
        
        return parent?.TryGetSymbol(name, out symbol) ?? false;
    }

    public FunctionSymbol[] GetDeclaredFunctions() => [.._symbols.Values.OfType<FunctionSymbol>()];
    public BindingSymbol[] GetDeclaredBindings() => [.._symbols.Values.OfType<BindingSymbol>()];
}

internal abstract record Symbol;

internal sealed record TypeSymbol : Symbol
{
    public static readonly TypeSymbol Int = new();
}

internal sealed record ParameterSymbol(string Name, TypeSymbol Type) : Symbol;
internal sealed record BindingSymbol(string Name, TypeSymbol Type) : Symbol;
internal sealed record FunctionSymbol(string Name, ParameterSymbol[] Parameters, TypeSymbol ReturnType) : Symbol;

internal abstract record BoundStatement;
internal sealed record BoundExpressionStatement(BoundExpression Expression) : BoundStatement;
internal sealed record BoundFunctionDeclarationStatement(FunctionSymbol Binding, BoundBlockStatement Body) : BoundStatement;
internal sealed record BoundBindingDeclarationStatement(BindingSymbol Binding, BoundExpression Value) : BoundStatement;
internal sealed record BoundBlockStatement(BoundStatement[] Statements) : BoundStatement;

internal sealed record BoundProgram(Dictionary<FunctionSymbol, BoundBlockStatement> Functions, BoundStatement[] Statements);

internal abstract record BoundExpression(TypeSymbol Type); 
internal sealed record BoundLiteralExpression(TypeSymbol Type, object Value) : BoundExpression(Type);
internal sealed record BoundBindingExpression(BindingSymbol Binding) : BoundExpression(Binding.Type);
internal sealed record BoundCallExpression(FunctionSymbol Function, BoundExpression[] Arguments) : BoundExpression(Function.ReturnType);
internal sealed record BoundBinaryExpression(BoundExpression Left, BoundBinaryOperator Operator, BoundExpression Right) : BoundExpression(Operator.ResultType);

internal sealed record BoundBinaryOperator(TypeSymbol LeftType, BoundBinaryOperatorKind Kind, TypeSymbol RightType, TypeSymbol ResultType);
    
internal enum BoundBinaryOperatorKind
{
    Add,
    Subtract,
    Multiply,
    Divide,
}