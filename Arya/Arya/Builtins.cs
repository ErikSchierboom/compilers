namespace Arya;

public abstract record UnaryFunction : Function
{
    public override int Arity => 1;
}

public abstract record UnaryIntegerFunction(Func<int, int> Operation) : UnaryFunction
{
    public override Value Invoke(params Value[] arguments) =>
        arguments[0] switch
        {
            Array array => Array.UnaryOp(array, Operation),
            Integer integer => new Integer(Operation(integer.Value)),
            _ => throw new InvalidOperationException("Invalid argument type")
        };
}

public abstract record UnaryStringFunction(Func<string, string> Operation) : UnaryFunction
{
    public override Value Invoke(params Value[] arguments) =>
        arguments[0] switch
        {
            Array array => Array.UnaryOp(array, Operation),
            String str => new String(Operation(str.Value)),
            _ => throw new InvalidOperationException("Invalid argument type")
        };
}

public sealed record AbsFunction() : UnaryIntegerFunction(Math.Abs);
public sealed record LowercaseFunction() : UnaryStringFunction(str => str.ToLower());
