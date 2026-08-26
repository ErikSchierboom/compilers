namespace Arya;

public class Interpreter
{
    private readonly Expression _expression;

    private Interpreter(Expression expression) => _expression = expression;

    public static Value Evaluate(string code)
    {
        var expression = Parser.Parse(code);
        var loweredExpression = Lowerer.Lower(expression);
        return new Interpreter(loweredExpression).Evaluate();
    }

    private Value Evaluate() => Evaluate(_expression, CreateDefaultScope());

    public Value Evaluate(Expression expression, Scope scope) =>
        expression switch
        {
            ArrayExpression array => Evaluate(array, scope),
            UnaryExpression unary => Evaluate(unary, scope),
            BinaryExpression binary => Evaluate(binary, scope),
            LiteralExpression literal => Evaluate(literal, scope),
            BoxExpression box => Evaluate(box, scope),
            CallExpression call => Evaluate(call, scope),
            ParenthesizedExpression parenthesized => Evaluate(parenthesized.Expression, scope),
            AssignmentExpression assignment => Evaluate(assignment, scope),
            NameExpression name => Evaluate(name, scope),
            PlaceholderExpression name => Evaluate(name, scope),
            BlockExpression block => Evaluate(block, scope),
            LambdaExpression block => Evaluate(block, scope),
            _ => throw new ArgumentOutOfRangeException(nameof(expression))
        };

    private Value Evaluate(BlockExpression block, Scope scope)
    {
        Value? result = null;

        foreach (var expression in block.Expressions)
            result = Evaluate(expression, scope);

        return result ?? throw new InvalidOperationException("Empty block expression");
    }

    private Value Evaluate(UnaryExpression unary, Scope scope) =>
        throw new InvalidOperationException("Unary expressions should have been rewritten");

    private Value Evaluate(BinaryExpression binary, Scope scope) =>
        throw new InvalidOperationException("Binary expressions should have been rewritten");

    private Value Evaluate(LiteralExpression literal, Scope scope) =>
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
        Function? function = null;

        switch (call.Target)
        {
            case NameExpression nameExpression:
                function = scope[nameExpression.Identifier.Text] as Function;
                break;
            case LambdaExpression lambdaExpression:
                function = Evaluate(lambdaExpression, scope) as LambdaFunction;
                break;
        }

        if (function is null)
            throw new InvalidOperationException("Can only call functions");

        if (call.Arguments.Length != function.Arity)
            throw new InvalidOperationException("Invalid number of arguments");

        var arguments = call.Arguments.Select(arg => Evaluate(arg, scope)).ToArray();
        return function.Invoke(arguments, this, scope);
    }

    private Value Evaluate(LambdaExpression lambda, Scope scope)
    {
        var parameterNames = lambda.Parameters.Select(parameter => parameter.Identifier.Text);
        return new LambdaFunction([..parameterNames], lambda.Body);
    }

    private Value Evaluate(NameExpression call, Scope scope) =>
        scope[call.Identifier.Text] ?? throw new InvalidOperationException("Variable not found");

    private Value Evaluate(PlaceholderExpression call, Scope scope) =>
        scope[call.Identifier.Text] ?? throw new InvalidOperationException("Placeholder not found");

    private Value Evaluate(AssignmentExpression call, Scope scope)
    {
        var value = Evaluate(call.Value, scope);
        scope[call.Identifier.Identifier.Text] = value;
        return value;
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

    private static Scope CreateDefaultScope()
    {
        var scope = new Scope();

        foreach (var builtinFunction in BuiltinFunction.All)
            scope[builtinFunction.Name] = builtinFunction;

        return scope;
    }
}

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
        set => Values[key] = value;
    }
}
