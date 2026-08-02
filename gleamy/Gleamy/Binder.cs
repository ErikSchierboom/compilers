namespace Gleamy;

internal class Binder
{
    private readonly SyntaxTree _tree;

    private Binder(SyntaxTree tree) => _tree = tree;

    public static BoundProgram Bind(SyntaxTree tree)
    {
        return new Binder(tree).Bind();
    }

    public BoundProgram Bind()
    {
        var boundStatements = new List<BoundStatement>();
        var boundScope = new BoundScope
        {
            [TypeSymbol.Bool.Name] = TypeSymbol.Bool,
            [TypeSymbol.Int.Name] = TypeSymbol.Int,
            [TypeSymbol.IntArray.Name] = TypeSymbol.IntArray,
            [TypeSymbol.IntMatrix.Name] = TypeSymbol.IntMatrix,
            [TypeSymbol.BoolArray.Name] = TypeSymbol.BoolArray,
            [TypeSymbol.BoolMatrix.Name] = TypeSymbol.BoolMatrix
        };

        foreach (var statement in _tree.Statements)
            boundStatements.Add(Bind(statement, boundScope));

        return new BoundProgram(boundStatements);
    }

    private BoundStatement Bind(Statement statement, BoundScope scope)
    {
        switch (statement)
        {
            case BindingDeclarationStatement bindingDeclarationStatement:
                return Bind(bindingDeclarationStatement, scope);
            case BlockStatement blockStatement:
                return Bind(blockStatement, scope);
            case ExpressionStatement expressionStatement:
                return Bind(expressionStatement, scope);
            case FunctionDeclarationStatement functionDeclarationStatement:
                return Bind(functionDeclarationStatement, scope);
            default:
                throw new ArgumentOutOfRangeException(nameof(statement));
        }
    }
    
    private BoundExpressionStatement Bind(ExpressionStatement expressionStatement, BoundScope scope)
    {
        var boundExpression = Bind(expressionStatement.Expression, scope);
        return new BoundExpressionStatement(boundExpression);
    }

    private BoundBindingDeclarationStatement Bind(BindingDeclarationStatement bindingDeclarationStatement, BoundScope scope)
    {
        var boundValue = Bind(bindingDeclarationStatement.Value, scope);
        var bindingSymbol = new BindingSymbol(bindingDeclarationStatement.Identifier.Text, boundValue.Type);
        scope[bindingDeclarationStatement.Identifier.Text] = bindingSymbol;
        return new BoundBindingDeclarationStatement(bindingSymbol, boundValue);
    }
    
    private BoundBlockStatement Bind(BlockStatement blockStatement, BoundScope scope)
    {
        var boundStatements = new List<BoundStatement>();
        
        foreach (var statement in blockStatement.Statements)
            boundStatements.Add(Bind(statement, scope));

        return new BoundBlockStatement(boundStatements);
    }

    private BoundFunctionDeclarationStatement Bind(FunctionDeclarationStatement functionDeclarationStatement, BoundScope scope)
    {
        var functionScope = scope.CreateChild();
        var returnType = Bind(functionDeclarationStatement.ReturnValue, functionScope);
        var parameters = new List<ParameterSymbol>();
        var functionSymbol = new FunctionSymbol(functionDeclarationStatement.Identifier.Text, returnType, parameters, functionDeclarationStatement);
        
        // We need to forward declare the function within the function body's scope to allow
        // for recursive functions
        functionScope[functionDeclarationStatement.Identifier.Text] = functionSymbol;
        
        foreach (var parameter in functionDeclarationStatement.Parameters)
        {
            var parameterSymbol = new ParameterSymbol(parameter.Identifier.Text, Bind(parameter.IdentifierType, scope));
            parameters.Add(parameterSymbol);
            functionScope[parameter.Identifier.Text] = parameterSymbol;
        }

        var boundBlockStatement = Bind(functionDeclarationStatement.Body, functionScope);
        
        scope[functionDeclarationStatement.Identifier.Text] = functionSymbol;
        
        return new BoundFunctionDeclarationStatement(functionSymbol, boundBlockStatement);
    }

    private BoundExpression Bind(Expression expression, BoundScope scope)
    {
        switch (expression)
        {
            case BinaryExpression binaryExpression:
                return Bind(binaryExpression, scope);
            case CallExpression callExpression:
                return Bind(callExpression, scope);
            case ExpressionMatchExpression expressionMatchExpression:
                return Bind(expressionMatchExpression, scope);
            case LiteralExpression literalExpression:
                return Bind(literalExpression, scope);
            case ArrayLiteralExpression arrayLiteralExpression:
                return Bind(arrayLiteralExpression, scope);
            case LogicalAndExpression logicalAndExpression:
                return Bind(logicalAndExpression, scope);
            case LogicalOrExpression logicalOrExpression:
                return Bind(logicalOrExpression, scope);
            case NameExpression nameExpression:
                return Bind(nameExpression, scope);
            case ParenthesizedExpression parenthesizedExpression:
                return Bind(parenthesizedExpression, scope);
            case UnaryExpression unaryExpression:
                return Bind(unaryExpression, scope);
            case ValueMatchExpression valueMatchExpression:
                return Bind(valueMatchExpression, scope);
            default:
                throw new ArgumentOutOfRangeException(nameof(expression));
        }
    }

    private BoundBinaryExpression Bind(BinaryExpression binaryExpression, BoundScope scope)
    {
        var boundLeftExpression = Bind(binaryExpression.Left, scope);
        var boundRightExpression = Bind(binaryExpression.Right, scope);
        var boundBinaryOperator = BoundBinaryOperator.Bind(binaryExpression.Operator, boundLeftExpression.Type, boundRightExpression.Type);
        return new BoundBinaryExpression(boundLeftExpression, boundBinaryOperator, boundRightExpression);
    }

    private BoundCallExpression Bind(CallExpression callExpression, BoundScope scope)
    {
        var boundFunction = Bind(callExpression.Function, scope);
        if (boundFunction is not BoundNameExpression boundNameExpression)
            throw new InvalidOperationException($"Unexpected function {callExpression.Function}");
        
        if (boundNameExpression.Symbol is not FunctionSymbol functionSymbol)
            throw new InvalidOperationException($"Unexpected function {callExpression.Function}");

        if (functionSymbol.Parameters.Count != callExpression.Arguments.Length)
            throw new  InvalidOperationException($"Unexpected function {callExpression.Function}");

        var childScope = scope.CreateChild();
        
        var boundArguments = new List<BoundExpression>();
        foreach (var argument in callExpression.Arguments)
            boundArguments.Add(Bind(argument, childScope));

        return new BoundCallExpression(functionSymbol, boundArguments);
    }

    private BoundExpressionMatchExpression Bind(ExpressionMatchExpression expressionMatchExpression, BoundScope scope)
    {
        var cases = new List<BoundExpressionMatchCase>();
        
        foreach (var caseExpression in expressionMatchExpression.Cases)
            cases.Add(Bind(caseExpression, scope));
        
        return new BoundExpressionMatchExpression(cases);
    }

    private BoundExpressionMatchCase Bind(ExpressionMatchCase expressionMatchCase, BoundScope scope)
    {
        var boundPattern = Bind(expressionMatchCase.Pattern, scope);
        var boundReturnValue = Bind(expressionMatchCase.ReturnValue, scope);
        return new BoundExpressionMatchCase(boundPattern, boundReturnValue);
    }

    private BoundExpressionMatchPattern Bind(ExpressionMatchPattern expressionMatchPattern, BoundScope scope)
    {
        switch (expressionMatchPattern)
        {
            case DiscardExpressionMatchPattern:
                return new BoundDiscardExpressionMatchPattern();
            case ExpressionExpressionMatchPattern expressionExpressionMatchPattern:
                var boundExpression = Bind(expressionExpressionMatchPattern.Expression, scope);
                return new BoundExpressionExpressionMatchPattern(boundExpression);
            default:
                throw new ArgumentOutOfRangeException(nameof(expressionMatchPattern));
        }
    }

    private BoundLiteralExpression Bind(LiteralExpression literalExpression, BoundScope scope)
    {
        var boundConstant = BindConstant(literalExpression.Value);
        return new BoundLiteralExpression(boundConstant);
    }

    private BoundArrayLiteralExpression Bind(ArrayLiteralExpression arrayLiteralExpression, BoundScope scope)
    {
        var boundElements = new List<BoundExpression>();
        TypeSymbol? arrayElementType = null;

        foreach (var element in arrayLiteralExpression.Elements)
        {
            var boundExpression = Bind(element, scope);
            arrayElementType ??= boundExpression.Type;
            
            if (boundExpression.Type != arrayElementType)
                throw new InvalidOperationException("All elements in an array literal must have the same type.");

            boundElements.Add(boundExpression);
        }

        if (arrayElementType is null)
            throw new InvalidOperationException("An empty array must have an element type specified");

        return new BoundArrayLiteralExpression(boundElements, arrayElementType);
    }

    private BoundLogicalAndExpression Bind(LogicalAndExpression logicalAndExpression, BoundScope scope)
    {
        var boundLeftExpression = Bind(logicalAndExpression.Left, scope);
        var boundRightExpression = Bind(logicalAndExpression.Right, scope);
        if (boundLeftExpression.Type != TypeSymbol.Bool || boundRightExpression.Type  != TypeSymbol.Bool)
            throw new InvalidOperationException("AND comparison operands must both be booleans");
        
        return new BoundLogicalAndExpression(boundLeftExpression, boundRightExpression);
    }

    private BoundLogicalOrExpression Bind(LogicalOrExpression logicalOrExpression, BoundScope scope)
    {
        var boundLeftExpression = Bind(logicalOrExpression.Left, scope);
        var boundRightExpression = Bind(logicalOrExpression.Right, scope);
        if (boundLeftExpression.Type != TypeSymbol.Bool || boundRightExpression.Type  != TypeSymbol.Bool)
            throw new InvalidOperationException("OR comparison operands must both be booleans");
        
        return new BoundLogicalOrExpression(boundLeftExpression, boundRightExpression);
    }

    private BoundExpression Bind(NameExpression nameExpression, BoundScope scope)
    {
        var boundSymbol = scope[nameExpression.Identifier.Text];

        if (boundSymbol is TypeSymbol typeSymbol)
        {
            switch (typeSymbol.Kind)
            {
                case TypeKind.IntArray: return new BoundArrayLiteralExpression([], TypeSymbol.Int);
                case TypeKind.IntMatrix: return new BoundArrayLiteralExpression([], TypeSymbol.IntArray);
                case TypeKind.BoolArray: return new BoundArrayLiteralExpression([], TypeSymbol.Bool);
                case TypeKind.BoolMatrix: return new BoundArrayLiteralExpression([], TypeSymbol.BoolArray);
                default: throw new InvalidOperationException($"Unexpected type symbol: {typeSymbol}");
            }
        }

        return new BoundNameExpression(boundSymbol);
    }

    private BoundParenthesizedExpression Bind(ParenthesizedExpression expression, BoundScope scope)
    {
        var boundExpression = Bind(expression.Expression, scope);
        return new BoundParenthesizedExpression(boundExpression);
    }

    private BoundUnaryExpression Bind(UnaryExpression unaryExpression, BoundScope scope)
    {
        var boundExpression = Bind(unaryExpression.Value, scope);
        var boundUnaryOperator = BoundUnaryOperator.Bind(unaryExpression.Operator, boundExpression.Type);
        return new BoundUnaryExpression(boundUnaryOperator, boundExpression);
    }

    private BoundValueMatchExpression Bind(ValueMatchExpression valueMatchExpression, BoundScope scope)
    {
        var boundInput = Bind(valueMatchExpression.Input, scope);
        
        var boundCases = new List<BoundValueMatchCase>();

        foreach (var valueMatchCase in valueMatchExpression.Cases)
            boundCases.Add(Bind(valueMatchCase, boundInput.Type, scope));

        return new BoundValueMatchExpression(boundInput, boundCases);
    }

    private BoundValueMatchCase Bind(ValueMatchCase valueMatchCase, TypeSymbol boundInputType, BoundScope scope)
    {
        var bindingScope = scope.CreateChild();
        
        var boundPattern = Bind(valueMatchCase.Pattern, boundInputType, bindingScope);
        var boundReturnValue = Bind(valueMatchCase.ReturnValue, bindingScope);
        
        return new BoundValueMatchCase(boundPattern, boundReturnValue);
    }

    private BoundValueMatchPattern Bind(ValueMatchPattern valueMatchPattern, TypeSymbol boundInputType, BoundScope scope)
    {
        switch (valueMatchPattern)
        {
            case BindingValueMatchPattern bindingValueMatchPattern:
                var symbol = new BindingSymbol(bindingValueMatchPattern.Identifier.Text, boundInputType);
                scope[bindingValueMatchPattern.Identifier.Text] = symbol;
                return Bind(bindingValueMatchPattern, scope);
            case ComparisonValueMatchPattern comparisonValueMatchPattern:
                return Bind(comparisonValueMatchPattern, scope);
            case ConstantValueMatchPattern constantValueMatchPattern:
                return Bind(constantValueMatchPattern, scope);
            case DiscardValueMatchPattern discardValueMatchPattern:
                return Bind(discardValueMatchPattern, scope);
            case NegationValueMatchPattern negationValueMatchPattern:
                return Bind(negationValueMatchPattern, scope);
            default:
                throw new ArgumentOutOfRangeException(nameof(valueMatchPattern));
        }
    }

    private BoundBindingValueMatchPattern Bind(BindingValueMatchPattern bindingValueMatchPattern, BoundScope scope)
    {
        return new BoundBindingValueMatchPattern(bindingValueMatchPattern.Identifier);
    }
    
    private BoundValueMatchPattern Bind(ComparisonValueMatchPattern comparisonValueMatchPattern, BoundScope scope)
    {
        var boundConstant = BindConstant(comparisonValueMatchPattern.CompareValue);
        return new BoundComparisonValueMatchPattern(comparisonValueMatchPattern.Operator, boundConstant);
    }
    
    private BoundConstantValueMatchPattern Bind(ConstantValueMatchPattern constantValueMatchPattern, BoundScope scope)
    {
        var boundConstant = BindConstant(constantValueMatchPattern.Value);
        return new BoundConstantValueMatchPattern(boundConstant);
    }
    
    private BoundDiscardValueMatchPattern Bind(DiscardValueMatchPattern discardValueMatchPattern, BoundScope scope)
    {
        return new BoundDiscardValueMatchPattern();
    }
    
    private BoundNegationValueMatchPattern Bind(NegationValueMatchPattern negationValueMatchPattern, BoundScope scope)
    {
        var boundConstant = BindConstant(negationValueMatchPattern.Value);
        return new BoundNegationValueMatchPattern(boundConstant);
    }
    
    private BoundConstant BindConstant(Token token)
    {
        var literal = token.Literal ?? throw new ArgumentNullException(nameof(token));
        return new BoundConstant(literal);
    }

    private TypeSymbol Bind(IdentifierType identifierType, BoundScope scope)
    {
        return (TypeSymbol)scope[identifierType.Identifier.Text];
    }
}

internal abstract record Symbol(string Name);

internal sealed record FunctionSymbol(string Name, TypeSymbol Type, List<ParameterSymbol> Parameters, FunctionDeclarationStatement? Declaration) : Symbol(Name);
internal sealed record BindingSymbol(string Name, TypeSymbol Type) : Symbol(Name);
internal sealed record ParameterSymbol(string Name, TypeSymbol Type) : Symbol(Name);

internal enum TypeKind
{
    Unit,
    Any,
    Bool,
    BoolArray,
    BoolMatrix,
    Char,
    CharArray,
    CharMatrix,
    Int,
    IntArray,
    IntMatrix,
}

internal sealed record TypeSymbol(TypeKind Kind, string Name) : Symbol(Name)
{
    public static readonly TypeSymbol Any        = new(TypeKind.Any, "Any");
    public static readonly TypeSymbol Unit       = new(TypeKind.Unit, "Unit");
    public static readonly TypeSymbol Bool       = new(TypeKind.Bool, "Bool");
    public static readonly TypeSymbol BoolArray  = new(TypeKind.BoolArray, "Bool[]");
    public static readonly TypeSymbol BoolMatrix = new(TypeKind.BoolMatrix, "Bool[][]");
    public static readonly TypeSymbol Char       = new(TypeKind.Char, "Char");
    public static readonly TypeSymbol CharArray  = new(TypeKind.CharArray, "Char[]");
    public static readonly TypeSymbol CharMatrix = new(TypeKind.CharMatrix, "Char[][]");
    public static readonly TypeSymbol Int        = new(TypeKind.Int, "Int");
    public static readonly TypeSymbol IntArray   = new(TypeKind.IntArray, "Int[]");
    public static readonly TypeSymbol IntMatrix  = new(TypeKind.IntMatrix, "Int[][]");

    public TypeSymbol AddDimension() => _addDimensionMap[this];
    
    private static readonly Dictionary<TypeSymbol, TypeSymbol> _addDimensionMap = new()
    {
        [Bool] = BoolArray,
        [BoolArray] = BoolMatrix,
        [Char] = CharArray,
        [CharArray] = CharMatrix,
        [Int] = IntArray,
        [IntArray] = IntMatrix,
    };
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

internal sealed record BoundProgram(List<BoundStatement> Statements)
{
    public TypeSymbol Type => Statements.LastOrDefault()?.Type ?? TypeSymbol.Unit;
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

internal sealed record BoundBindingDeclarationStatement(BindingSymbol Binding, BoundExpression Value) : BoundStatement
{
    public override TypeSymbol Type => Value.Type;
}

internal sealed record BoundBlockStatement(List<BoundStatement> Statements) : BoundStatement
{
    public override TypeSymbol Type => Statements.LastOrDefault()?.Type ?? TypeSymbol.Unit;
}

internal abstract record BoundExpression
{
    public abstract TypeSymbol Type { get; }
}

internal sealed record BoundConstant(object Value)
{
    public TypeSymbol Type { get; } = Value switch
    {
        int => TypeSymbol.Int,
        bool => TypeSymbol.Bool,
        char => TypeSymbol.Char,
        _ => throw new NotImplementedException()
    };
}

internal sealed record BoundLiteralExpression(BoundConstant Value) : BoundExpression
{
    public override TypeSymbol Type => Value.Type;
}

internal sealed record BoundArrayLiteralExpression(List<BoundExpression> Elements, TypeSymbol ElementType) : BoundExpression
{
    public override TypeSymbol Type => ElementType.AddDimension();
}

internal sealed record BoundNameExpression(Symbol Symbol) : BoundExpression
{
    public override TypeSymbol Type => Symbol switch
    {
        BindingSymbol bindingSymbol => bindingSymbol.Type,
        FunctionSymbol functionSymbol => functionSymbol.Type,
        ParameterSymbol parameterSymbol => parameterSymbol.Type,
        TypeSymbol typeSymbol => typeSymbol,
        _ => throw new ArgumentOutOfRangeException(nameof(Symbol))
    };
}

internal sealed record BoundCallExpression(FunctionSymbol Function, List<BoundExpression> Arguments) : BoundExpression
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
            TokenType.Bang when operand == TypeSymbol.BoolArray => new BoundUnaryOperator(BoundUnaryOperatorKind.Negation, operand, TypeSymbol.BoolArray),
            TokenType.Bang when operand == TypeSymbol.BoolMatrix => new BoundUnaryOperator(BoundUnaryOperatorKind.Negation, operand, TypeSymbol.BoolMatrix),

            TokenType.Plus when operand == TypeSymbol.Int => new BoundUnaryOperator(BoundUnaryOperatorKind.Plus, operand, TypeSymbol.Int),
            TokenType.Plus when operand == TypeSymbol.IntArray => new BoundUnaryOperator(BoundUnaryOperatorKind.Plus, operand, TypeSymbol.IntArray),
            TokenType.Plus when operand == TypeSymbol.IntMatrix => new BoundUnaryOperator(BoundUnaryOperatorKind.Plus, operand, TypeSymbol.IntMatrix),

            TokenType.Minus when operand == TypeSymbol.Int => new BoundUnaryOperator(BoundUnaryOperatorKind.Minus, operand, TypeSymbol.Int),
            TokenType.Minus when operand == TypeSymbol.IntArray => new BoundUnaryOperator(BoundUnaryOperatorKind.Minus, operand, TypeSymbol.IntArray),
            TokenType.Minus when operand == TypeSymbol.IntMatrix => new BoundUnaryOperator(BoundUnaryOperatorKind.Minus, operand, TypeSymbol.IntMatrix),

            TokenType.Tilde when operand == TypeSymbol.Int => new BoundUnaryOperator(BoundUnaryOperatorKind.Complement, operand, TypeSymbol.Int),
            TokenType.Tilde when operand == TypeSymbol.IntArray => new BoundUnaryOperator(BoundUnaryOperatorKind.Complement, operand, TypeSymbol.IntArray),
            TokenType.Tilde when operand == TypeSymbol.IntMatrix => new BoundUnaryOperator(BoundUnaryOperatorKind.Complement, operand, TypeSymbol.IntMatrix),

            _ => throw new InvalidOperationException($"Unary operator '{@operator.Type}' is not defined for type '{operand}'.")
        };
}

internal sealed record BoundUnaryExpression(BoundUnaryOperator Operator, BoundExpression Value) : BoundExpression
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
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    LeftShift,
    RightShift,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor
}

internal sealed record BoundBinaryOperator(BoundBinaryOperatorKind Kind, TypeSymbol LeftOperand, TypeSymbol RightOperand, TypeSymbol Result)
{
    public TypeSymbol Type => Result;

    public static BoundBinaryOperator Bind(Token @operator, TypeSymbol left, TypeSymbol right) =>
        (@operator.Type, left.Kind, right.Kind) switch
        {
            (TokenType.Plus, TypeKind.Int, TypeKind.Int) => new BoundBinaryOperator(BoundBinaryOperatorKind.Addition, left, right, TypeSymbol.Int),
            (TokenType.Plus, TypeKind.Int or TypeKind.IntArray, TypeKind.Int or TypeKind.IntArray) => new BoundBinaryOperator(BoundBinaryOperatorKind.Addition, left, right, TypeSymbol.IntArray),
            (TokenType.Plus, TypeKind.Int or TypeKind.IntMatrix, TypeKind.Int or TypeKind.IntMatrix) => new BoundBinaryOperator(BoundBinaryOperatorKind.Addition, left, right, TypeSymbol.IntMatrix),
            
            (TokenType.Minus, TypeKind.Int, TypeKind.Int) => new BoundBinaryOperator(BoundBinaryOperatorKind.Subtraction, left, right, TypeSymbol.Int),
            (TokenType.Minus, TypeKind.Int or TypeKind.IntArray, TypeKind.Int or TypeKind.IntArray) => new BoundBinaryOperator(BoundBinaryOperatorKind.Subtraction, left, right, TypeSymbol.IntArray),
            (TokenType.Minus, TypeKind.Int or TypeKind.IntMatrix, TypeKind.Int or TypeKind.IntMatrix) => new BoundBinaryOperator(BoundBinaryOperatorKind.Subtraction, left, right, TypeSymbol.IntMatrix),
            
            (TokenType.Star, TypeKind.Int, TypeKind.Int) => new BoundBinaryOperator(BoundBinaryOperatorKind.Multiplication, left, right, TypeSymbol.Int),
            (TokenType.Star, TypeKind.Int or TypeKind.IntArray, TypeKind.Int or TypeKind.IntArray) => new BoundBinaryOperator(BoundBinaryOperatorKind.Multiplication, left, right, TypeSymbol.IntArray),
            (TokenType.Star, TypeKind.Int or TypeKind.IntMatrix, TypeKind.Int or TypeKind.IntMatrix) => new BoundBinaryOperator(BoundBinaryOperatorKind.Multiplication, left, right, TypeSymbol.IntMatrix),

            (TokenType.Slash, TypeKind.Int, TypeKind.Int) => new BoundBinaryOperator(BoundBinaryOperatorKind.Division, left, right, TypeSymbol.Int),
            (TokenType.Slash, TypeKind.Int or TypeKind.IntArray, TypeKind.Int or TypeKind.IntArray) => new BoundBinaryOperator(BoundBinaryOperatorKind.Division, left, right, TypeSymbol.IntArray),
            (TokenType.Slash, TypeKind.Int or TypeKind.IntMatrix, TypeKind.Int or TypeKind.IntMatrix) => new BoundBinaryOperator(BoundBinaryOperatorKind.Division, left, right, TypeSymbol.IntMatrix),
            
            (TokenType.Percent, TypeKind.Int, TypeKind.Int) => new BoundBinaryOperator(BoundBinaryOperatorKind.Modulus, left, right, TypeSymbol.Int),
            (TokenType.Percent, TypeKind.Int or TypeKind.IntArray, TypeKind.Int or TypeKind.IntArray) => new BoundBinaryOperator(BoundBinaryOperatorKind.Modulus, left, right, TypeSymbol.IntArray),
            (TokenType.Percent, TypeKind.Int or TypeKind.IntMatrix, TypeKind.Int or TypeKind.IntMatrix) => new BoundBinaryOperator(BoundBinaryOperatorKind.Modulus, left, right, TypeSymbol.IntMatrix),

            (TokenType.Ampersand, TypeKind.Int, TypeKind.Int) => new BoundBinaryOperator(BoundBinaryOperatorKind.BitwiseAnd, left, right, TypeSymbol.Int),
            (TokenType.Ampersand, TypeKind.Int or TypeKind.IntArray, TypeKind.Int or TypeKind.IntArray) => new BoundBinaryOperator(BoundBinaryOperatorKind.BitwiseAnd, left, right, TypeSymbol.IntArray),
            (TokenType.Ampersand, TypeKind.Int or TypeKind.IntMatrix, TypeKind.Int or TypeKind.IntMatrix) => new BoundBinaryOperator(BoundBinaryOperatorKind.BitwiseAnd, left, right, TypeSymbol.IntMatrix),

            (TokenType.Pipe, TypeKind.Int, TypeKind.Int) => new BoundBinaryOperator(BoundBinaryOperatorKind.BitwiseOr, left, right, TypeSymbol.Int),
            (TokenType.Pipe, TypeKind.Int or TypeKind.IntArray, TypeKind.Int or TypeKind.IntArray) => new BoundBinaryOperator(BoundBinaryOperatorKind.BitwiseOr, left, right, TypeSymbol.IntArray),
            (TokenType.Pipe, TypeKind.Int or TypeKind.IntMatrix, TypeKind.Int or TypeKind.IntMatrix) => new BoundBinaryOperator(BoundBinaryOperatorKind.BitwiseOr, left, right, TypeSymbol.IntMatrix),

            (TokenType.Caret, TypeKind.Int, TypeKind.Int) => new BoundBinaryOperator(BoundBinaryOperatorKind.BitwiseXor, left, right, TypeSymbol.Int),
            (TokenType.Caret, TypeKind.Int or TypeKind.IntArray, TypeKind.Int or TypeKind.IntArray) => new BoundBinaryOperator(BoundBinaryOperatorKind.BitwiseXor, left, right, TypeSymbol.IntArray),
            (TokenType.Caret, TypeKind.Int or TypeKind.IntMatrix, TypeKind.Int or TypeKind.IntMatrix) => new BoundBinaryOperator(BoundBinaryOperatorKind.BitwiseXor, left, right, TypeSymbol.IntMatrix),

            (TokenType.LessLess, TypeKind.Int, TypeKind.Int) => new BoundBinaryOperator(BoundBinaryOperatorKind.LeftShift, left, right, TypeSymbol.Int),
            (TokenType.LessLess, TypeKind.Int or TypeKind.IntArray, TypeKind.Int or TypeKind.IntArray) => new BoundBinaryOperator(BoundBinaryOperatorKind.LeftShift, left, right, TypeSymbol.IntArray),
            (TokenType.LessLess, TypeKind.Int or TypeKind.IntMatrix, TypeKind.Int or TypeKind.IntMatrix) => new BoundBinaryOperator(BoundBinaryOperatorKind.LeftShift, left, right, TypeSymbol.IntMatrix),

            (TokenType.GreaterGreater, TypeKind.Int, TypeKind.Int) => new BoundBinaryOperator(BoundBinaryOperatorKind.RightShift, left, right, TypeSymbol.Int),
            (TokenType.GreaterGreater, TypeKind.Int or TypeKind.IntArray, TypeKind.Int or TypeKind.IntArray) => new BoundBinaryOperator(BoundBinaryOperatorKind.RightShift, left, right, TypeSymbol.IntArray),
            (TokenType.GreaterGreater, TypeKind.Int or TypeKind.IntMatrix, TypeKind.Int or TypeKind.IntMatrix) => new BoundBinaryOperator(BoundBinaryOperatorKind.RightShift, left, right, TypeSymbol.IntMatrix),

            (TokenType.Greater, TypeKind.Int, TypeKind.Int) => new BoundBinaryOperator(BoundBinaryOperatorKind.Greater, left, right, TypeSymbol.Bool),
            (TokenType.Greater, TypeKind.Int or TypeKind.IntArray, TypeKind.Int or TypeKind.IntArray) => new BoundBinaryOperator(BoundBinaryOperatorKind.Greater, left, right, TypeSymbol.BoolArray),
            (TokenType.Greater, TypeKind.Int or TypeKind.IntMatrix, TypeKind.Int or TypeKind.IntMatrix) => new BoundBinaryOperator(BoundBinaryOperatorKind.Greater, left, right, TypeSymbol.BoolMatrix),

            (TokenType.GreaterEqual, TypeKind.Int, TypeKind.Int) => new BoundBinaryOperator(BoundBinaryOperatorKind.GreaterEqual, left, right, TypeSymbol.Bool),
            (TokenType.GreaterEqual, TypeKind.Int or TypeKind.IntArray, TypeKind.Int or TypeKind.IntArray) => new BoundBinaryOperator(BoundBinaryOperatorKind.GreaterEqual, left, right, TypeSymbol.BoolArray),
            (TokenType.GreaterEqual, TypeKind.Int or TypeKind.IntMatrix, TypeKind.Int or TypeKind.IntMatrix) => new BoundBinaryOperator(BoundBinaryOperatorKind.GreaterEqual, left, right, TypeSymbol.BoolMatrix),

            (TokenType.Less, TypeKind.Int, TypeKind.Int) => new BoundBinaryOperator(BoundBinaryOperatorKind.Less, left, right, TypeSymbol.Bool),
            (TokenType.Less, TypeKind.Int or TypeKind.IntArray, TypeKind.Int or TypeKind.IntArray) => new BoundBinaryOperator(BoundBinaryOperatorKind.Less, left, right, TypeSymbol.BoolArray),
            (TokenType.Less, TypeKind.Int or TypeKind.IntMatrix, TypeKind.Int or TypeKind.IntMatrix) => new BoundBinaryOperator(BoundBinaryOperatorKind.Less, left, right, TypeSymbol.BoolMatrix),

            (TokenType.LessEqual, TypeKind.Int, TypeKind.Int) => new BoundBinaryOperator(BoundBinaryOperatorKind.LessEqual, left, right, TypeSymbol.Bool),
            (TokenType.LessEqual, TypeKind.Int or TypeKind.IntArray, TypeKind.Int or TypeKind.IntArray) => new BoundBinaryOperator(BoundBinaryOperatorKind.LessEqual, left, right, TypeSymbol.BoolArray),
            (TokenType.LessEqual, TypeKind.Int or TypeKind.IntMatrix, TypeKind.Int or TypeKind.IntMatrix) => new BoundBinaryOperator(BoundBinaryOperatorKind.LessEqual, left, right, TypeSymbol.BoolMatrix),

            (TokenType.EqualEqual, TypeKind.Int, TypeKind.Int) => new BoundBinaryOperator(BoundBinaryOperatorKind.Equality, left, right, TypeSymbol.Bool),
            (TokenType.EqualEqual, TypeKind.Int or TypeKind.IntArray, TypeKind.Int or TypeKind.IntArray) => new BoundBinaryOperator(BoundBinaryOperatorKind.Equality, left, right, TypeSymbol.BoolArray),
            (TokenType.EqualEqual, TypeKind.Int or TypeKind.IntMatrix, TypeKind.Int or TypeKind.IntMatrix) => new BoundBinaryOperator(BoundBinaryOperatorKind.Equality, left, right, TypeSymbol.BoolMatrix),
            
            (TokenType.EqualEqual, TypeKind.Bool, TypeKind.Bool) => new BoundBinaryOperator(BoundBinaryOperatorKind.Equality, left, right, TypeSymbol.Bool),
            (TokenType.EqualEqual, TypeKind.Bool or TypeKind.BoolArray, TypeKind.Bool or TypeKind.BoolArray) => new BoundBinaryOperator(BoundBinaryOperatorKind.Equality, left, right, TypeSymbol.BoolArray),
            (TokenType.EqualEqual, TypeKind.Bool or TypeKind.BoolMatrix, TypeKind.Bool or TypeKind.BoolMatrix) => new BoundBinaryOperator(BoundBinaryOperatorKind.Equality, left, right, TypeSymbol.BoolMatrix),

            (TokenType.BangEqual, TypeKind.Int, TypeKind.Int) => new BoundBinaryOperator(BoundBinaryOperatorKind.Inequality, left, right, TypeSymbol.Bool),
            (TokenType.BangEqual, TypeKind.Int or TypeKind.IntArray, TypeKind.Int or TypeKind.IntArray) => new BoundBinaryOperator(BoundBinaryOperatorKind.Inequality, left, right, TypeSymbol.BoolArray),
            (TokenType.BangEqual, TypeKind.Int or TypeKind.IntMatrix, TypeKind.Int or TypeKind.IntMatrix) => new BoundBinaryOperator(BoundBinaryOperatorKind.Inequality, left, right, TypeSymbol.BoolMatrix),

            (TokenType.BangEqual, TypeKind.Bool, TypeKind.Bool) => new BoundBinaryOperator(BoundBinaryOperatorKind.Inequality, left, right, TypeSymbol.Bool),
            (TokenType.BangEqual, TypeKind.Bool or TypeKind.BoolArray, TypeKind.Bool or TypeKind.BoolArray) => new BoundBinaryOperator(BoundBinaryOperatorKind.Inequality, left, right, TypeSymbol.BoolArray),
            (TokenType.BangEqual, TypeKind.Bool or TypeKind.BoolMatrix, TypeKind.Bool or TypeKind.BoolMatrix) => new BoundBinaryOperator(BoundBinaryOperatorKind.Inequality, left, right, TypeSymbol.BoolMatrix),

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

internal sealed record BoundValueMatchExpression(BoundExpression Input, List<BoundValueMatchCase> Cases) : BoundExpression
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

internal sealed record BoundExpressionMatchExpression(List<BoundExpressionMatchCase> Cases) : BoundExpression
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
