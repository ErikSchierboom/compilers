namespace Gleamy;

internal sealed class Binder(SyntaxTree tree)
{
    private BoundScope _scope = new();
    
    public BoundProgram Bind()
    {
        throw new NotImplementedException();
    }
    
    private BoundStatement BindStatement(Statement statement)
    {
        throw new NotImplementedException();
    }
    
    private BoundExpression BindExpression(Expression expression)
    {
        switch (expression)
        {
            case BinaryExpression binaryExpression:
                var left = BindExpression(binaryExpression.Left);
                var right = BindExpression(binaryExpression.Right);
                
                // TODO: bind operator
                break;
            case NameExpression nameExpression:
                if (!_scope.TryGetSymbol(nameExpression.Identifier.Text, out var symbol))
                    throw new InvalidOperationException($"Could not find symbol {nameExpression.Identifier.Text}");
                    
                // return new BoundBindingExpression()
            case CallExpression callExpression:
                break;
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
internal sealed record BoundBindingDeclarationStatement(BindingSymbol Binding, BoundExpression Value) : BoundStatement;
internal sealed record BoundBlockStatement(BoundStatement[] Statements) : BoundStatement;

internal sealed record BoundProgram(Dictionary<FunctionSymbol, BoundBlockStatement> Functions, BoundStatement[] Statements);

internal abstract record BoundExpression; 
internal sealed record BoundLiteralExpression(TypeSymbol Type, object Value) : BoundExpression;
internal sealed record BoundBindingExpression(BindingSymbol Binding) : BoundExpression;
internal sealed record BoundCallExpression(FunctionSymbol Function, BoundExpression[] Arguments) : BoundExpression;
internal sealed record BoundBinaryExpression(BoundExpression Left, BoundBinaryOperator Operator, BoundExpression Right) : BoundExpression;

internal sealed record BoundBinaryOperator(TypeSymbol LeftType, BoundBinaryOperatorKind Kind, TypeSymbol RightType, TypeSymbol ResultType);
    
internal enum  BoundBinaryOperatorKind
{
    Add,
    Subtract,
}