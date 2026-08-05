namespace Arya;

public sealed record AbsFunction : Function
{
    public override int Arity => 1;

    public override Value Invoke(params Value[] arguments) =>
        arguments[0] switch
        {
            Array array => Array.UnaryOp(array, Math.Abs),
            Integer integer => new Integer(Math.Abs(integer.Value)),
            _ => throw new InvalidOperationException("Invalid argument type")
        };
}