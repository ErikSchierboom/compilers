namespace Arya;

public sealed record Scope
{
    private readonly Scope? _parent;
    private Dictionary<string, Value?> Values => field ??= new Dictionary<string, Value?>();
    
    public Scope(Scope? parent = null) => _parent = parent;

    public Scope CreateChild() => new(this);

    public Value? this[string key]
    {
        get
        {
            if (Values.TryGetValue(key, out var result))
                return result;
            
            return _parent?[key];
        }
        set
        {
            if (!Values.TryAdd(key, value))
                throw new InvalidOperationException("Cannot redeclare local");;
        }
    }
}

public class Interpreter
{
    private readonly List<Expression> _expressions;

    private Interpreter(List<Expression> expressions) => _expressions = expressions;

    public static Value? Evaluate(string code)
    {
        var expressions = Parser.Parse(code);
        return new Interpreter(expressions).Evaluate();
    }

    private Value? Evaluate()
    {
        Value? result = null;
        
        var defaultScope = CreateDefaultScope();

        foreach (var expression in _expressions)
            result = Evaluate(expression, defaultScope);

        return result;
    }

    private Value Evaluate(Expression expression, Scope scope) =>
        expression switch
        {
            ArrayLiteralExpression arrayLiteral => Evaluate(arrayLiteral, scope),
            UnaryExpression unary => Evaluate(unary, scope),
            BinaryExpression binary => Evaluate(binary, scope),
            LiteralExpression literal => Evaluate(literal, scope),
            CallExpression call => Evaluate(call, scope),
            ParenthesizedExpression parenthesized => Evaluate(parenthesized.Expression, scope),
            _ => throw new ArgumentOutOfRangeException(nameof(expression))
        };

    private Value Evaluate(UnaryExpression unary, Scope scope)
    {
        var operand = Evaluate(unary.Operand, scope);
        
        return (unary.Operator.Type, operand) switch
        {
            (TokenType.Plus, IntArray arr) => arr,
            (TokenType.Minus, IntArray arr) => arr.UnaryOp(i => -i),
            _ => throw new InvalidOperationException("Invalid unary expression")
        };
    }

    private Value Evaluate(BinaryExpression binary, Scope scope)
    {
        var left = Evaluate(binary.Left, scope);
        var right = Evaluate(binary.Right, scope);

        switch (binary.Operator.Type, left, right)
        {
            case (TokenType.Plus, IntArray, EmptyArray):
            case (TokenType.Plus, EmptyArray, IntArray):
                return EmptyArray.Instance;
            case (TokenType.Plus, IntArray l, IntArray r):
                return l.BinaryOp(r, (a, b) => a + b);
            default:
                throw new InvalidOperationException("Invalid binary expression");
        }
    }

    private static Value Evaluate(LiteralExpression literal, Scope scope) =>
        literal.Value.Type switch
        {
            TokenType.String => CharArray.Vector((string)literal.Value.Literal!),
            TokenType.Number => IntArray.Scalar((int)literal.Value.Literal!),
            TokenType.Char => CharArray.Scalar((char)literal.Value.Literal!),
            _ => throw new ArgumentOutOfRangeException(nameof(literal.Value.Type))
        };

    private Value Evaluate(CallExpression call, Scope scope)
    {
        if (scope[call.FunctionName.Text] is not Function function)
            throw new InvalidOperationException("Can only call functions");
                
        if (call.Arguments.Length != function.Arity)
            throw new InvalidOperationException("Invalid number of arguments");

        var arguments = call.Arguments.Select(arg => Evaluate(arg, scope)).ToArray();
        return function.Invoke(arguments);
    }

    private Value Evaluate(ArrayLiteralExpression arrayLiteral, Scope scope)
    {
        if (arrayLiteral.Elements.Length == 0)
            return EmptyArray.Instance;
                
        Type? elementType = null;
        Shape? shape = null;
        bool identicalShapes = true;
                
        Value[] newElements = new Value[arrayLiteral.Elements.Length];

        for (var index = 0; index < arrayLiteral.Elements.Length; index++)
        {
            var evaluatedElement = Evaluate(arrayLiteral.Elements[index], scope);

            if (elementType is null)
                elementType = evaluatedElement.GetType();
            else if (elementType != evaluatedElement.GetType())
                throw new InvalidOperationException("Array elements must be of the same type.");

            if (shape is null)
                shape = evaluatedElement.Shape;
            else if (shape != evaluatedElement.Shape)
                identicalShapes = false;

            newElements[index] = evaluatedElement;
        }

        var newShape = shape!.Prepend(newElements.Length);
        
        if (!identicalShapes)
            return BoxArray.Vector([..newElements.Select(element => element.Box())]);

        if (elementType == typeof(IntArray))
            return new IntArray(newShape, [..newElements.Cast<IntArray>().SelectMany(array => array.Elements)]);
                
        if (elementType == typeof(CharArray))
            return new CharArray(newShape, [..newElements.Cast<CharArray>().SelectMany(array => array.Elements)]);
                
        if (elementType == typeof(EmptyArray))
            return EmptyArray.Instance;
                
        throw new InvalidOperationException("Invalid array element type");
    }

    private static Scope CreateDefaultScope()
    {
        var scope = new Scope();
        
        // foreach (var builtinFunction in BuiltinFunctions.All)
        //     scope[builtinFunction.Name] = builtinFunction;

        return scope;
    }
}
