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
            (TokenType.Exclamation, Array<int> arr) => arr.Unary(i => ~i),
            (TokenType.Exclamation, Array<bool> arr) => arr.Unary(b => !b),
            (_, Array<Box> arr) => arr.Unary(box => UnaryOp(op, box.Value).Box()),
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

            (TokenType.Plus or TokenType.Minus or TokenType.Star or TokenType.Slash or 
             TokenType.Percent or TokenType.Ampersand or TokenType.Pipe or
             TokenType.GreaterGreater or TokenType.LessLess, Array<Box> l, var r) => l.Zip(r.Boxes(), (a, b) => BinaryOp(op, a.Value, b.Value).Box()),
            (TokenType.Plus or TokenType.Minus or TokenType.Star or TokenType.Slash or 
             TokenType.Percent or TokenType.Ampersand or TokenType.Pipe or
             TokenType.GreaterGreater or TokenType.LessLess, var l, Array<Box> r) => r.Zip(l.Boxes(), (a, b) => BinaryOp(op, b.Value, a.Value).Box()),

            (TokenType.Plus, Array<int> l, Array<int> r) => l.Zip(r, (a, b) => a + b),
            (TokenType.Plus, Array<char> l, Array<int> r) => l.Zip(r, (a, b) => (char)(a + b)),
            (TokenType.Plus, Array<int> l, Array<char> r) => r.Zip(l, (a, b) => (char)(a + b)),

            (TokenType.Minus, Array<int> l, Array<int> r) => l.Zip(r, (a, b) => a - b),
            (TokenType.Minus, Array<char> l, Array<int> r) => l.Zip(r, (a, b) => (char)(a - b)),
            (TokenType.Minus, Array<int> l, Array<char> r) => r.Zip(l, (a, b) => (char)(b - a)),

            (TokenType.Star, Array<int> l, Array<int> r) => l.Zip(r, (a, b) => a * b),
            (TokenType.Slash, Array<int> l, Array<int> r) => l.Zip(r, (a, b) => a / b),
            (TokenType.Percent, Array<int> l, Array<int> r) => l.Zip(r, (a, b) => a % b),
            (TokenType.Ampersand, Array<int> l, Array<int> r) => l.Zip(r, (a, b) => a & b),
            (TokenType.Pipe, Array<int> l, Array<int> r) => l.Zip(r, (a, b) => a | b),
            (TokenType.GreaterGreater, Array<int> l, Array<int> r) => l.Zip(r, (a, b) => a >> b),
            (TokenType.LessLess, Array<int> l, Array<int> r) => l.Zip(r, (a, b) => a << b),

            _ => throw new InvalidOperationException("Invalid binary expression")
        };

    private static Value Evaluate(LiteralExpression literal, Scope scope) =>
        literal.Value.Type switch
        {
            TokenType.String => Array<char>.Vector(((string)literal.Value.Literal!).ToCharArray()),
            TokenType.Number => Array<int>.Scalar((int)literal.Value.Literal!),
            TokenType.Char => Array<char>.Scalar((char)literal.Value.Literal!),
            TokenType.Boolean => Array<bool>.Scalar((bool)literal.Value.Literal!),
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

        var elements = array.Elements.Select(element => Evaluate(element, scope)).ToArray();

        if (elements.Select(element => element.GetType()).Distinct().Count() > 1)
            throw new InvalidOperationException("Array elements must be of the same type.");

        if (elements.Select(element => element.Shape).Distinct().Count() > 1)
            throw new InvalidOperationException("Array elements must have identical shapes.");

        var newShape = elements[0].Shape.Prepend(elements.Length);

        return elements[0] switch
        {
            Array<int> => new Array<int>(newShape, [.. elements.Cast<Array<int>>().SelectMany(intArray => intArray.Elements)]),
            Array<char> => new Array<char>(newShape, [.. elements.Cast<Array<char>>().SelectMany(charArray => charArray.Elements)]),
            Array<bool> => new Array<bool>(newShape, [.. elements.Cast<Array<bool>>().SelectMany(boolArray => boolArray.Elements)]),
            Array<Box> => new Array<Box>(newShape, [.. elements.Cast<Array<Box>>().SelectMany(boxArray => boxArray.Elements)]),
            Array<Any> => Array<Any>.Empty,
            _ => throw new InvalidOperationException("Invalid array element type")
        };
    }

    private Value Evaluate(IndexerExpression indexer, Scope scope)
    {
        var target = Evaluate(indexer.Target, scope);
        var index = Evaluate(indexer.Index, scope);

        if (index is not Array<int> indexArray)
            throw new InvalidOperationException("Can only index with arrays");

        if (indexArray.Shape.IsMatrix)
            throw new InvalidOperationException("Can only index with scalars or vectors");

        return target switch
        {
            Array<int> intArray => intArray.Index(indexArray),
            Array<char> charArray => charArray.Index(indexArray),
            Array<bool> boolArray => boolArray.Index(indexArray),
            Array<Box> boxArray => boxArray.Index(indexArray),
            _ => throw new InvalidOperationException("Can only index into arrays")
        };
    }

    private static Scope CreateDefaultScope()
    {
        var scope = new Scope();

        foreach (var builtinFunction in BuiltinFunctions.All)
            scope[builtinFunction.Name] = builtinFunction;

        return scope;
    }
}
