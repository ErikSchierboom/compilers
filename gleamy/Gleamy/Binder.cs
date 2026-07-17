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

internal enum BoundUnaryOperatorKind
{
    Negation,
    Plus,
    Minus,
    Complement
}

internal sealed record BoundUnaryOperator(BoundUnaryOperatorKind Kind, TypeSymbol Operand, TypeSymbol Result)
{
    public TypeSymbol Type => Result;

    public static BoundUnaryOperator Bind(Token @operator, TypeSymbol operand) =>
        @operator.Type switch
        {
            TokenType.Bang when operand == TypeSymbol.Bool => new BoundUnaryOperator(BoundUnaryOperatorKind.Negation, operand, TypeSymbol.Bool),
            TokenType.Plus when operand == TypeSymbol.Int => new BoundUnaryOperator(BoundUnaryOperatorKind.Plus, operand, TypeSymbol.Int),
            TokenType.Minus when operand == TypeSymbol.Int => new BoundUnaryOperator(BoundUnaryOperatorKind.Minus, operand, TypeSymbol.Int),
            TokenType.Tilde when operand == TypeSymbol.Int => new BoundUnaryOperator(BoundUnaryOperatorKind.Complement, operand, TypeSymbol.Int),
            _ => throw new InvalidOperationException($"Unary operator '{@operator.Type}' is not defined for type '{operand}'.")
        };
}

internal sealed record BoundUnaryExpression(Token Operator, BoundExpression Value) : BoundExpression
{
    public override TypeSymbol Type => Value.Type;
}

internal enum BoundBinaryOperatorKind
{
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulus,
    Equality,
    Inequality,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual
}

internal sealed record BoundBinaryOperator(BoundBinaryOperatorKind Kind, TypeSymbol LeftOperand, TypeSymbol RightOperand, TypeSymbol Result)
{
    public TypeSymbol Type => Result;

    public static BoundBinaryOperator Bind(Token @operator, TypeSymbol left, TypeSymbol right) =>
        @operator.Type switch
        {
            TokenType.EqualEqual when left == TypeSymbol.Bool && right == TypeSymbol.Bool => new BoundBinaryOperator(BoundBinaryOperatorKind.Equality, left, right, TypeSymbol.Bool),
            TokenType.EqualEqual when left == TypeSymbol.Int && right == TypeSymbol.Int => new BoundBinaryOperator(BoundBinaryOperatorKind.Equality, left, right, TypeSymbol.Bool),
            TokenType.BangEqual when left == TypeSymbol.Bool && right == TypeSymbol.Bool => new BoundBinaryOperator(BoundBinaryOperatorKind.Inequality, left, right, TypeSymbol.Bool),
            TokenType.BangEqual when left == TypeSymbol.Int && right == TypeSymbol.Int => new BoundBinaryOperator(BoundBinaryOperatorKind.Inequality, left, right, TypeSymbol.Bool),
            TokenType.Plus when left == TypeSymbol.Int && right == TypeSymbol.Int => new BoundBinaryOperator(BoundBinaryOperatorKind.Addition, left, right, TypeSymbol.Int),
            TokenType.Minus when left == TypeSymbol.Int && right == TypeSymbol.Int => new BoundBinaryOperator(BoundBinaryOperatorKind.Subtraction, left, right, TypeSymbol.Int),
            TokenType.Star when left == TypeSymbol.Int && right == TypeSymbol.Int => new BoundBinaryOperator(BoundBinaryOperatorKind.Multiplication, left, right, TypeSymbol.Int),
            TokenType.Slash when left == TypeSymbol.Int && right == TypeSymbol.Int => new BoundBinaryOperator(BoundBinaryOperatorKind.Division, left, right, TypeSymbol.Int),
            TokenType.Percent when left == TypeSymbol.Int && right == TypeSymbol.Int => new BoundBinaryOperator(BoundBinaryOperatorKind.Modulus, left, right, TypeSymbol.Int),
            TokenType.Greater when left == TypeSymbol.Int && right == TypeSymbol.Int => new BoundBinaryOperator(BoundBinaryOperatorKind.GreaterThan, left, right, TypeSymbol.Bool),
            TokenType.GreaterEqual when left == TypeSymbol.Int && right == TypeSymbol.Int => new BoundBinaryOperator(BoundBinaryOperatorKind.GreaterThanOrEqual, left, right, TypeSymbol.Bool),
            TokenType.Less when left == TypeSymbol.Int && right == TypeSymbol.Int => new BoundBinaryOperator(BoundBinaryOperatorKind.LessThan, left, right, TypeSymbol.Bool),
            TokenType.LessEqual when left == TypeSymbol.Int && right == TypeSymbol.Int => new BoundBinaryOperator(BoundBinaryOperatorKind.LessThanOrEqual, left, right, TypeSymbol.Bool),
            _ => throw new InvalidOperationException($"Binary operator '{@operator.Type}' is not defined for types '{left}' and '{right}'.")
        };
}

internal sealed record BoundBinaryExpression(BoundExpression Left, BoundBinaryOperator Operator, BoundExpression Right) : BoundExpression
{
    public override TypeSymbol Type => Operator.Type;
}

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

