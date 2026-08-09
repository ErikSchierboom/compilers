namespace Arya;

internal static class BuiltinFunctions
{
    public static readonly Function[] All =
    [
        // Unary functions
        new AbsFunction(),
        // new LowercaseFunction(),
        // new UppercaseFunction(),
        // new TrimFunction()
    ];

    private abstract record UnaryIntegerFunction(string Name, Func<int, int> Operation) : Function(Name)
    {
        public override int Arity => 1;
    
        public override Value Invoke(params Value[] arguments) =>
            arguments[0] switch
            {
                EmptyArray => EmptyArray.Instance,
                Array<int> intArray => intArray.Map(Operation),
                Array<Box> boxArray => boxArray.Pervade(element => Invoke(element)),
                _ => throw new InvalidOperationException("Invalid argument type")
            };
    }
    
    private sealed record AbsFunction() : UnaryIntegerFunction("abs", Math.Abs);

    // private abstract record UnaryStringFunction(string Name, Func<string, string> Operation) : Function(Name)
    // {
    //     public override int Arity => 1;
    //
    //     public override Value Invoke(params Value[] arguments) =>
    //         arguments[0] switch
    //         {
    //             Array array => Array.UnaryOp(array, Operation),
    //             String str => new String(Operation(str.Value)),
    //             _ => throw new InvalidOperationException("Invalid argument type")
    //         };
    // }
    //
    // private sealed record LowercaseFunction() : UnaryStringFunction("lowercase", str => str.ToLowerInvariant());
    // private sealed record UppercaseFunction() : UnaryStringFunction("uppercase", str => str.ToUpperInvariant());
    // private sealed record TrimFunction() : UnaryStringFunction("trim", str => str.Trim());
}
