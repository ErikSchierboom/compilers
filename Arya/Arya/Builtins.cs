namespace Arya;

public static class BuiltinFunctions
{
    public static readonly Function[] All =
    [
        new AbsFunction(),
        new LowercaseFunction(),
        new UppercaseFunction(),
        new TrimFunction()
    ];
}

public abstract record UnaryIntegerFunction(string Name, Func<int, int> Operation) : Function(Name)
{
    public override int Arity => 1;
    
    public override Value Invoke(params Value[] arguments) =>
        arguments[0] switch
        {
            Array array => Array.UnaryOp(array, Operation),
            Integer integer => new Integer(Operation(integer.Value)),
            _ => throw new InvalidOperationException("Invalid argument type")
        };
}

public abstract record UnaryStringFunction(string Name, Func<string, string> Operation) : Function(Name)
{
    public override int Arity => 1;
    
    public override Value Invoke(params Value[] arguments) =>
        arguments[0] switch
        {
            Array array => Array.UnaryOp(array, Operation),
            String str => new String(Operation(str.Value)),
            _ => throw new InvalidOperationException("Invalid argument type")
        };
}

public sealed record AbsFunction() : UnaryIntegerFunction("abs", Math.Abs);
public sealed record LowercaseFunction() : UnaryStringFunction("lowercase", str => str.ToLowerInvariant());
public sealed record UppercaseFunction() : UnaryStringFunction("uppercase", str => str.ToUpperInvariant());
public sealed record TrimFunction() : UnaryStringFunction("trim", str => str.Trim());
