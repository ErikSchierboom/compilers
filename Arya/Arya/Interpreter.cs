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
                throw new InvalidOperationException("Cannot redeclare local"); ;
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
            BoxExpression box => Evaluate(box, scope),
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
            (TokenType.Plus, Array<int> arr) => arr,
            (TokenType.Minus, Array<int> arr) => arr.Unary(i => -i),
            (_, Array<Box> arr) => arr.Unary(box => box.Unary(element => UnaryOp(op, element))),
            _ => throw new InvalidOperationException("Invalid unary expression")
        };

    private Value Evaluate(BinaryExpression binary, Scope scope) =>
        BinaryOp(binary.Operator.Type, Evaluate(binary.Left, scope), Evaluate(binary.Right, scope));

    private static Value BinaryOp(TokenType op, Value left, Value right) =>
        (op, left, right) switch
        {
            (TokenType.PlusPlus, Array<int> l, Array<int> r) => l.Append(r),
            (TokenType.PlusPlus, Array<int> l, Array<Any>) => l.Append(Array<int>.Empty),
            (TokenType.PlusPlus, Array<Any>, Array<int> r) => r.Append(Array<int>.Empty),
            (TokenType.PlusPlus, Array<char> l, Array<char> r) => l.Append(r),
            (TokenType.PlusPlus, Array<char> l, Array<Any>) => l.Append(Array<char>.Empty),
            (TokenType.PlusPlus, Array<Any>, Array<char> r) => r.Append(Array<char>.Empty),
            (TokenType.PlusPlus, Array<Any>, Array<Any>) => Array<Any>.Empty,
            (TokenType.PlusPlus, Array<Box> l, Array<Box> r) => l.Zip(r, (a, b) => BinaryOp(op, a.Value, b.Value).Box()),

            (_, _, Array<Any>) or
            (_, Array<Any>, _) => Array<Any>.Empty,

            (TokenType.Plus or TokenType.Minus or TokenType.Star or TokenType.Slash or TokenType.Percent, Array<Box> l, var r) => l.Zip(r.Boxes(), (a, b) => BinaryOp(op, a.Value, b.Value).Box()),
            (TokenType.Plus or TokenType.Minus or TokenType.Star or TokenType.Slash or TokenType.Percent, var l, Array<Box> r) => r.Zip(l.Boxes(), (a, b) => BinaryOp(op, b.Value, a.Value).Box()),

            (TokenType.Plus, Array<int> l, Array<int> r) => l.Zip(r, (a, b) => a + b),
            (TokenType.Plus, Array<char> l, Array<int> r) => l.Zip(r, (a, b) => (char)(a + b)),
            (TokenType.Plus, Array<int> l, Array<char> r) => r.Zip(l, (a, b) => (char)(a + b)),

            (TokenType.Minus, Array<int> l, Array<int> r) => l.Zip(r, (a, b) => a - b),
            (TokenType.Minus, Array<char> l, Array<int> r) => l.Zip(r, (a, b) => (char)(a - b)),
            (TokenType.Minus, Array<int> l, Array<char> r) => r.Zip(l, (a, b) => (char)(b - a)),

            (TokenType.Star, Array<int> l, Array<int> r) => l.Zip(r, (a, b) => a * b),

            (TokenType.Slash, Array<int> l, Array<int> r) => l.Zip(r, (a, b) => a / b),

            (TokenType.Percent, Array<int> l, Array<int> r) => l.Zip(r, (a, b) => a % b),

            _ => throw new InvalidOperationException("Invalid binary expression")
        };

    private static Value Evaluate(LiteralExpression literal, Scope scope) =>
        literal.Value.Type switch
        {
            TokenType.String => Array<char>.Vector(((string)literal.Value.Literal!).ToCharArray()),
            TokenType.Number => Array<int>.Scalar((int)literal.Value.Literal!),
            TokenType.Char => Array<char>.Scalar((char)literal.Value.Literal!),
            _ => throw new ArgumentOutOfRangeException(nameof(literal.Value.Type))
        };
    
    private Value Evaluate(BoxExpression box, Scope scope)
    {
        var value = Evaluate(box.Expression, scope);
        return Array<Box>.Scalar(value.Box());
    }

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
            return Array<Any>.Empty;

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
            throw new InvalidOperationException("Array elements must have identical shapes.");

        if (elementType == typeof(Array<int>))
            return new Array<int>(newShape, [.. newElements.Cast<Array<int>>().SelectMany(intArray => intArray.Elements)]);

        if (elementType == typeof(Array<char>))
            return new Array<char>(newShape, [.. newElements.Cast<Array<char>>().SelectMany(charArray => charArray.Elements)]);

        if (elementType == typeof(Array<Box>))
            return new Array<Box>(newShape, [.. newElements.Cast<Array<Box>>().SelectMany(boxArray => boxArray.Elements)]);

        if (elementType == typeof(Array<Any>))
            return Array<Any>.Empty;
        
        throw new InvalidOperationException("Invalid array element type");
    }

    private Value Evaluate(IndexerExpression indexer, Scope scope)
    {
        var target = Evaluate(indexer.Target, scope);
        var index = Evaluate(indexer.Index, scope);

        // TODO: support all arrays by moving functionality to array
        if (target is not Array<int> targetArray)
            throw new InvalidOperationException("Can only index into arrays");

        if (index is not Array<int> indexArray)
            throw new InvalidOperationException("Can only index with arrays");

        if (indexArray.Shape.IsMatrix)
            throw new InvalidOperationException("Can only index with scalars or vectors");

        var newElements = indexArray.Elements
            .Select(oneBasedIndex => oneBasedIndex > 0 ? oneBasedIndex - 1 : targetArray.Shape.RowCount + oneBasedIndex)
            .SelectMany(zeroBasedIndex => targetArray.Elements.Skip(zeroBasedIndex * targetArray.Shape.RowLength).Take(targetArray.Shape.RowLength));
        var newShape = indexArray.Shape.IsScalar 
            ? targetArray.Shape.RemoveFirst() 
            : targetArray.Shape.SetFirst(indexArray.Elements.Length);
        
        return targetArray with { Shape = newShape, Elements = [..newElements] };
    }

    private static Scope CreateDefaultScope()
    {
        var scope = new Scope();

        foreach (var builtinFunction in BuiltinFunctions.All)
            scope[builtinFunction.Name] = builtinFunction;

        return scope;
    }
}
