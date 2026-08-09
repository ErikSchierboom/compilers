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
            ArrayExpression array => Evaluate(array, scope),
            UnaryExpression unary => Evaluate(unary, scope),
            BinaryExpression binary => Evaluate(binary, scope),
            LiteralExpression literal => Evaluate(literal, scope),
            CallExpression call => Evaluate(call, scope),
            IndexerExpression indexer => Evaluate(indexer, scope),
            ParenthesizedExpression parenthesized => Evaluate(parenthesized.Expression, scope),
            _ => throw new ArgumentOutOfRangeException(nameof(expression))
        };

    private Value Evaluate(UnaryExpression unary, Scope scope) =>
        UnaryOp(unary.Operator.Type, Evaluate(unary.Operand, scope));

    private static Value UnaryOp(TokenType op, Value operand) =>
        (op, operand) switch
        {
            (TokenType.Plus, IntArray arr) => arr,
            (TokenType.Minus, IntArray arr) => arr.Map(i => -i),
            (_, BoxArray arr) => arr.Pervade(element => UnaryOp(op, element)),
            _ => throw new InvalidOperationException("Invalid unary expression")
        };

    private Value Evaluate(BinaryExpression binary, Scope scope) =>
        BinaryOp(binary.Operator.Type, Evaluate(binary.Left, scope), Evaluate(binary.Right, scope));

    private static Value BinaryOp(TokenType op, Value left, Value right) =>
        (op, left, right) switch
        {
            (TokenType.PlusPlus, IntArray l, IntArray r) => l.Append(r),
            (TokenType.PlusPlus, IntArray l, EmptyArray r) => l.Append(r),
            (TokenType.PlusPlus, EmptyArray l, IntArray r) => r.Append(l),
            (TokenType.PlusPlus, CharArray l, CharArray r) => l.Append(r),
            (TokenType.PlusPlus, CharArray l, EmptyArray r) => l.Append(r),
            (TokenType.PlusPlus, EmptyArray l, CharArray r) => r.Append(l),
            (TokenType.PlusPlus, EmptyArray, EmptyArray) => EmptyArray.Instance,
            
            (_, _, EmptyArray) or
            (_, EmptyArray, _) => EmptyArray.Instance,

            (TokenType.Plus or TokenType.Minus or TokenType.Star or TokenType.Slash or TokenType.Percent, BoxArray l, var r) => l.Pervade(r, (a, b) => BinaryOp(op, a, b)),
            (TokenType.Plus or TokenType.Minus or TokenType.Star or TokenType.Slash or TokenType.Percent, var l, BoxArray r) => r.Pervade(l, (a, b) => BinaryOp(op, b, a)),

            (TokenType.Plus, IntArray l, IntArray r) => l.Zip(r, (a, b) => a + b),
            (TokenType.Plus, CharArray l, IntArray r) => l.Zip(r, (a, b) => (char)(a + b)),
            (TokenType.Plus, IntArray l, CharArray r) => r.Zip(l, (a, b) => (char)(a + b)),

            (TokenType.Minus, IntArray l, IntArray r) => l.Zip(r, (a, b) => a - b),
            (TokenType.Minus, CharArray l, IntArray r) => l.Zip(r, (a, b) => (char)(a - b)),
            (TokenType.Minus, IntArray l, CharArray r) => r.Zip(l, (a, b) => (char)(b - a)),

            (TokenType.Star, IntArray l, IntArray r) => l.Zip(r, (a, b) => a * b),

            (TokenType.Slash, IntArray l, IntArray r) => l.Zip(r, (a, b) => a / b),

            (TokenType.Percent, IntArray l, IntArray r) => l.Zip(r, (a, b) => a % b),
            
            _ => throw new InvalidOperationException("Invalid binary expression")
        };

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

    private Value Evaluate(ArrayExpression array, Scope scope)
    {
        if (array.Elements.Length == 0)
            return EmptyArray.Instance;
                
        Type? elementType = null;
        Shape? shape = null;
        bool identicalShapes = true;
                
        Value[] newElements = new Value[array.Elements.Length];

        for (var index = 0; index < array.Elements.Length; index++)
        {
            var evaluatedElement = Evaluate(array.Elements[index], scope);

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
    
    private Value Evaluate(IndexerExpression indexer, Scope scope)
    {
        var target = Evaluate(indexer.Target, scope);
        var index = Evaluate(indexer.Index, scope);

        if (target is not IntArray targetArray)
            throw new InvalidOperationException("Can only index into arrays");

        if (index is not IntArray indexArray)
            throw new InvalidOperationException("Can only index with arrays");
        
        if (indexArray.Shape.IsMatrix)
            throw new InvalidOperationException("Can only index with scalars or vectors");

        var newElements = indexArray.Elements
            .SelectMany(targetIndex =>
            {   
                var from = (targetIndex - 1) * targetArray.Shape.RowCount;
                var until = targetIndex * targetArray.Shape.RowCount;
                return targetArray.Elements[from..until];
            });
        var newShape = indexArray.Shape.IsScalar
            ? targetArray.Shape.RemoveFirst()
            : targetArray.Shape.Replace(0, indexArray.Elements.Length);
        return new IntArray(newShape,[..newElements]);
    }

    private static Scope CreateDefaultScope()
    {
        var scope = new Scope();
        
        foreach (var builtinFunction in BuiltinFunctions.All)
            scope[builtinFunction.Name] = builtinFunction;

        return scope;
    }
}
