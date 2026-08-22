namespace Arya;

internal static class BuiltinFunctions
{
    public static readonly Function[] All =
    [
        new Unary.CountFunction(),
        new Unary.LengthFunction(),
        new Unary.TransposeFunction(),
        new Unary.RangeFunction(),
        new Unary.AbsFunction(),
        new Unary.LowercaseFunction(),
        new Unary.UppercaseFunction(),
        new Unary.TrimFunction(),
        new Unary.ReverseFunction(),
        new Unary.IndicesFunction(),

        new Binary.ReshapeFunction(),
        new Binary.ReplicateFunction(),
    ];

    private static class Unary
    {
        internal abstract record UnaryFunction(string Name) : Function(Name)
        {
            public override int Arity => 1;
        }

        internal abstract record UnaryIntegerFunction(string Name, Func<int, int> Operation) : Function(Name)
        {
            public override int Arity => 1;

            public override Value Invoke(params Value[] arguments) =>
                arguments[0] switch
                {
                    Array<int> intArray => intArray.Unary(Operation),
                    Array<Box> boxArray => boxArray.Unary(box => box.Unary(element => Invoke(element))),
                    Array<Any> => Array<Any>.Empty,
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }

        internal sealed record AbsFunction() : UnaryIntegerFunction("abs", Math.Abs);

        internal abstract record UnaryCharFunction(string Name, Func<char, char> Operation) : UnaryFunction(Name)
        {
            public override Value Invoke(params Value[] arguments) =>
                arguments[0] switch
                {
                    Array<char> charArray => charArray.Unary(Operation),
                    Array<Box> boxArray => boxArray.Unary(box => box.Unary(element => Invoke(element))),
                    Array<Any> => Array<Any>.Empty,
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }

        internal sealed record LowercaseFunction() : UnaryCharFunction("lowercase", char.ToLowerInvariant);
        internal sealed record UppercaseFunction() : UnaryCharFunction("uppercase", char.ToUpperInvariant);

        internal abstract record UnaryCharSequenceFunction(string Name, Func<char[], char[]> Operation) : Function(Name)
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
                        return charArray.Binary(Operation);
                    case Array<Box> boxArray:
                        return boxArray.Unary(box => box.Unary(element => Invoke(element)));
                    case Array<Any>:
                        return Array<Any>.Empty;
                    default:
                        throw new InvalidOperationException("Invalid argument type");
                }
            }
        }

        internal sealed record TrimFunction() : UnaryCharSequenceFunction("trim", str => str.Trim());

        internal sealed record CountFunction() : UnaryFunction("count")
        {
            public override Value Invoke(params Value[] arguments) =>
                arguments[0] switch
                {
                    Array<int> intArray   => Array<int>.Scalar(intArray.Elements.Length),
                    Array<char> charArray => Array<int>.Scalar(charArray.Elements.Length),
                    Array<Box> boxArray   => Array<int>.Scalar(boxArray.Elements.Length),
                    Array<Any>            => Array<int>.Scalar(0),
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }
        
        internal sealed record LengthFunction() : UnaryFunction("length")
        {
            public override Value Invoke(params Value[] arguments) =>
                arguments[0] switch
                {
                    Array<int> intArray   => Array<int>.Scalar(intArray.Shape.RowCount),
                    Array<char> charArray => Array<int>.Scalar(charArray.Shape.RowCount),
                    Array<Box> boxArray   => Array<int>.Scalar(boxArray.Shape.RowCount),
                    Array<Any>            => Array<int>.Scalar(0),
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }

        internal sealed record TransposeFunction() : UnaryFunction("transpose")
        {
            public override Value Invoke(params Value[] arguments) =>
                arguments[0] switch
                {
                    Array<int> intArray   => intArray.Transpose(),
                    Array<char> charArray => charArray.Transpose(),
                    Array<Box> boxArray   => boxArray.Transpose(),
                    Array<Any> anyArray   => anyArray,
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }

        internal sealed record RangeFunction() : UnaryFunction("range")
        {
            public override Value Invoke(params Value[] arguments)
            {
                if (arguments[0] is not Array<int> intArray)
                    throw new InvalidOperationException("Invalid argument type");

                if (!intArray.Shape.IsScalar)
                    throw new InvalidOperationException("Invalid argument type");

                var numberOfElements = intArray.Elements[0];
                if (numberOfElements < 0)
                    throw new InvalidOperationException("Invalid argument type");

                if (numberOfElements == 0)
                    return Array<Any>.Empty;
                
                return Array<int>.Vector([.. Enumerable.Range(0, numberOfElements)]);
            }
        }

        internal sealed record ReverseFunction() : UnaryFunction("reverse")
        {
            public override Value Invoke(params Value[] arguments) =>
                arguments[0] switch
                {
                    Array<int> intArray   => intArray.Reverse(),
                    Array<bool> boolArray => boolArray.Reverse(),
                    Array<char> charArray => charArray.Reverse(),
                    Array<Box> boxArray   => boxArray.Reverse(),
                    Array<Any> anyArray   => anyArray,
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }

        internal sealed record IndicesFunction() : UnaryFunction("indices")
        {
            public override Value Invoke(params Value[] arguments) =>
                arguments[0] switch
                {
                    Array<int> intArray   => intArray.Indices(),
                    Array<bool> boolArray => boolArray.Indices(),
                    Array<char> charArray => charArray.Indices(),
                    Array<Box> boxArray   => boxArray.Indices(),
                    Array<Any> anyArray   => anyArray.Indices(),
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }
    }

    private static class Binary
    {
        internal abstract record BinaryFunction(string Name) : Function(Name)
        {
            public override int Arity => 2;
        }
        
        internal sealed record ReshapeFunction() : BinaryFunction("reshape")
        {
            public override Value Invoke(params Value[] arguments) =>
                (arguments[0], arguments[1]) switch
                {
                    (Array<int> intArray, Array<int> newDimensions) => intArray.Reshape(newDimensions),
                    (Array<bool> boolArray, Array<int> newDimensions) => boolArray.Reshape(newDimensions),
                    (Array<char> charArray, Array<int> newDimensions) => charArray.Reshape(newDimensions),
                    (Array<Box> boxArray, Array<int> newDimensions) => boxArray.Reshape(newDimensions),
                    (Array<Any> anyArray, Array<int>) => anyArray,
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }

        internal sealed record ReplicateFunction() : BinaryFunction("replicate")
        {
            public override Value Invoke(params Value[] arguments) =>
                (arguments[0], arguments[1]) switch
                {
                    (Array<Any> _, _) or (_, Array<Any>) => Array<Any>.Empty,
                    (Array<int> intArray, Array<int> replications) => intArray.Replicate(replications),
                    (Array<bool> boolArray, Array<int> replications) => boolArray.Replicate(replications),
                    (Array<char> charArray, Array<int> replications) => charArray.Replicate(replications),
                    (Array<Box> boxArray, Array<int> replications) => boxArray.Replicate(replications),
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }
    }
}
