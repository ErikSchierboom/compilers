namespace Arya;

internal static class BuiltinFunctions
{
    public static readonly Function[] All =
    [
        // Unary functions - integers
        new AbsFunction(),
        
        // Unary functions - characters
        new LowercaseFunction(),
        new UppercaseFunction(),
        new TrimFunction(),
    ];

    private abstract record UnaryIntegerFunction(string Name, Func<int, int> Operation) : Function(Name)
    {
        public override int Arity => 1;

        public override Value Invoke(params Value[] arguments) =>
            arguments[0] switch
            {
                Array<int> intArray => intArray.Map(Operation),
                Array<Box> boxArray => boxArray.Map(box => box.Map(element => Invoke(element))),
                Array<Any> => Array<Any>.Empty,
                _ => throw new InvalidOperationException("Invalid argument type")
            };
    }

    private sealed record AbsFunction() : UnaryIntegerFunction("abs", Math.Abs);

    private abstract record UnaryCharFunction(string Name, Func<char, char> Operation) : Function(Name)
    {
        public override int Arity => 1;

        public override Value Invoke(params Value[] arguments) =>
            arguments[0] switch
            {
                Array<char> charArray => charArray.Map(Operation),
                Array<Box> boxArray => boxArray.Map(box => box.Map(element => Invoke(element))),
                Array<Any> => Array<Any>.Empty,
                _ => throw new InvalidOperationException("Invalid argument type")
            };
    }

    private sealed record LowercaseFunction() : UnaryCharFunction("lowercase", char.ToLowerInvariant);
    private sealed record UppercaseFunction() : UnaryCharFunction("uppercase", char.ToUpperInvariant);

    private abstract record UnaryCharSequenceFunction(string Name, Func<char[], char[]> Operation) : Function(Name)
    {
        protected UnaryCharSequenceFunction(string name, Func<ReadOnlyMemory<char>, ReadOnlyMemory<char>> operation) : 
            this(name, chars => operation(chars.AsMemory()).ToArray())
        {
        }

        public override int Arity => 1;

        public override Value Invoke(params Value[] arguments)
        {
            switch (arguments[0])
            {
                case Array<char> charArray:
                    return charArray.Map(Operation);
                case Array<Box> boxArray:
                    return boxArray.Map(box => box.Map(element => Invoke(element)));
                case Array<Any>:
                    return Array<Any>.Empty;
                default:
                    throw new InvalidOperationException("Invalid argument type");
            }
        }
    }

    private sealed record TrimFunction() : UnaryCharSequenceFunction("trim", str => str.Trim());
}
