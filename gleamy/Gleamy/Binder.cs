namespace Gleamy;

internal class Binder
{
    public static BoundProgram Bind(SyntaxTree tree)
    {
        throw new NotImplementedException();
    }
}

internal abstract record Symbol(string Name);
internal sealed record FunctionSymbol(string Name, TypeSymbol Type, ParameterSymbol[] Parameters, FunctionDeclarationStatement Declaration) : Symbol(Name);
internal sealed record BindingSymbol(string Name, TypeSymbol Type) : Symbol(Name);
internal sealed record ParameterSymbol(string Name, TypeSymbol Type) : Symbol(Name);

internal sealed record TypeSymbol(string Name) : Symbol(Name)
{
    public static readonly TypeSymbol Any = new("Any");
    public static readonly TypeSymbol Void = new("Void");
    public static readonly TypeSymbol Bool = new("Bool");
    public static readonly TypeSymbol Int = new("Int");
}

internal class BoundScope(BoundScope? parent = null)
{
    private readonly Dictionary<string, Symbol> _locals = new();

    public BoundScope CreateChild() => new(this);
        
    public Symbol this[string key]
    {
        get
        {
            if (_locals.TryGetValue(key, out var result))
                return result;
            
            return parent?[key] ?? throw new KeyNotFoundException();
        }
        set
        {
            if (!_locals.TryAdd(key, value))
                throw new InvalidOperationException("Cannot redeclare local");;
        }
    }
}



internal sealed record BoundProgram(BoundStatement[] Statements)
{
    public TypeSymbol Type => Statements.LastOrDefault()?.Type ?? TypeSymbol.Void;
}

internal abstract record BoundStatement
{
    public abstract TypeSymbol Type { get; }
}

internal sealed record BoundFunctionDeclarationStatement(FunctionSymbol Function, BoundBlockStatement Body) : BoundStatement
{
    public override TypeSymbol Type => Function.Type;
}

internal sealed record BoundExpressionStatement(BoundExpression Expression) : BoundStatement
{
    public override TypeSymbol Type => Expression.Type;
}

internal sealed record BoundBindingDeclarationStatement(Token Identifier, BoundExpression Value) : BoundStatement
{
    public override TypeSymbol Type => Value.Type;
}

internal sealed record BoundBlockStatement(BoundStatement[] Statements) : BoundStatement
{
    public override TypeSymbol Type => Statements.LastOrDefault()?.Type ?? TypeSymbol.Void;
}

internal abstract record BoundExpression
{
    public abstract TypeSymbol Type { get; }
}

internal sealed record BoundConstant(object Value)
{
    public TypeSymbol Type { get; } = Value switch
    {
        int i => TypeSymbol.Int,
        bool b => TypeSymbol.Bool,
        _ => throw new NotImplementedException()
    };
}

internal sealed record BoundLiteralExpression(Token Value) : BoundExpression
{
    public BoundConstant Constant { get; } = new(Value.Literal!);

    public override TypeSymbol Type => Constant.Type;
}

internal sealed record BoundNameExpression(BindingSymbol Symbol) : BoundExpression
{
    public override TypeSymbol Type => Symbol.Type;
}

internal sealed record BoundCallExpression(FunctionSymbol Function, BoundExpression[] Arguments) : BoundExpression
{
    public override TypeSymbol Type => Function.Type;
}

internal sealed record BoundUnaryExpression(Token Operator, BoundExpression Value) : BoundExpression
{
    public override TypeSymbol Type => Value.Type;
}
internal sealed record BoundBinaryExpression(BoundExpression Left, Token Operator, BoundExpression Right) : BoundExpression;

internal sealed record BoundParenthesizedExpression(BoundExpression Expression) : BoundExpression
{
    public override TypeSymbol Type => Expression.Type;
}

internal sealed record BoundLogicalAndExpression(BoundExpression Left, BoundExpression Right) : BoundExpression
{
    public override TypeSymbol Type => TypeSymbol.Bool;
}

internal sealed record BoundLogicalOrExpression(BoundExpression Left, BoundExpression Right) : BoundExpression
{
    public override TypeSymbol Type => TypeSymbol.Bool;
}

internal sealed record BoundValueMatchExpression(BoundExpression Input, BoundValueMatchCase[] Cases) : BoundExpression
{
    // We will verify that all expressions in the cases have the same type
    public override TypeSymbol Type => Cases[0].ReturnValue.Type;
}

internal sealed record BoundValueMatchCase(BoundValueMatchPattern Pattern, BoundExpression ReturnValue)
{
    public TypeSymbol Type => ReturnValue.Type;
}
internal abstract record BoundValueMatchPattern;
internal sealed record BoundConstantValueMatchPattern(BoundConstant Value) : BoundValueMatchPattern;
internal sealed record BoundNegationValueMatchPattern(BoundConstant Value) : BoundValueMatchPattern;

internal sealed record BoundBindingValueMatchPattern(Token Identifier) : BoundValueMatchPattern
{
    // A binding can match any type
    public TypeSymbol Type => TypeSymbol.Any;
}

internal sealed record BoundComparisonValueMatchPattern(Token Operator, BoundConstant CompareValue) : BoundValueMatchPattern;

internal sealed record BoundDiscardValueMatchPattern : BoundValueMatchPattern
{
    // A discard can match any type
    public TypeSymbol Type => TypeSymbol.Any;
}

internal sealed record BoundExpressionMatchExpression(BoundExpressionMatchCase[] Cases) : BoundExpression
{
    // We will verify that all expressions in the cases have the same type
    public override TypeSymbol Type => Cases[0].ReturnValue.Type;
}

internal sealed record BoundExpressionMatchCase(BoundExpressionMatchPattern Pattern, BoundExpression ReturnValue)
{
    public TypeSymbol Type => ReturnValue.Type;
}

internal abstract record BoundExpressionMatchPattern;

internal sealed record BoundExpressionExpressionMatchPattern(BoundExpression Expression) : BoundExpressionMatchPattern
{
    public TypeSymbol Type => Expression.Type;
}

internal sealed record BoundDiscardExpressionMatchPattern : BoundExpressionMatchPattern
{
    // A discard can match any type
    public TypeSymbol Type => TypeSymbol.Any;
}

