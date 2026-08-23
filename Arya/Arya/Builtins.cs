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
        new Binary.MaxFunction(),
        new Binary.MinFunction(),
        new Binary.ReduceFunction(),
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

            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                arguments[0] switch
                {
                    Array<int> intArray => intArray.Unary(Operation),
                    Array<Box> boxArray => boxArray.Unary(box => Invoke([box.Value], interpreter, scope).Box()),
                    Array<Any> => Array<Any>.Empty,
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }

        internal sealed record AbsFunction() : UnaryIntegerFunction("abs", Math.Abs);

        internal abstract record UnaryCharFunction(string Name, Func<char, char> Operation) : UnaryFunction(Name)
        {
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                arguments[0] switch
                {
                    Array<char> charArray => charArray.Unary(Operation),
                    Array<Box> boxArray => boxArray.Unary(box => Invoke([box.Value], interpreter, scope).Box()),
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

            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope)
            {
                switch (arguments[0])
                {
                    case Array<char> charArray:
                        return charArray.Binary(Operation);
                    case Array<Box> boxArray:
                        return boxArray.Unary(box => Invoke([box.Value], interpreter, scope).Box());
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
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                arguments[0] switch
                {
                    Array<int> intArray   => Array<int>.Scalar(intArray.Elements.Length),
                    Array<bool> boolArray => Array<int>.Scalar(boolArray.Elements.Length),
                    Array<char> charArray => Array<int>.Scalar(charArray.Elements.Length),
                    Array<Box> boxArray   => Array<int>.Scalar(boxArray.Elements.Length),
                    Array<Any>            => Array<int>.Scalar(0),
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }
        
        internal sealed record LengthFunction() : UnaryFunction("length")
        {
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                arguments[0] switch
                {
                    Array<int> intArray   => Array<int>.Scalar(intArray.Shape.RowCount),
                    Array<bool> boolArray => Array<int>.Scalar(boolArray.Shape.RowCount),
                    Array<char> charArray => Array<int>.Scalar(charArray.Shape.RowCount),
                    Array<Box> boxArray   => Array<int>.Scalar(boxArray.Shape.RowCount),
                    Array<Any>            => Array<int>.Scalar(0),
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }

        internal sealed record TransposeFunction() : UnaryFunction("transpose")
        {
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                arguments[0] switch
                {
                    Array<int> intArray   => Transpose(intArray),
                    Array<bool> boolArray => Transpose(boolArray),
                    Array<char> charArray => Transpose(charArray),
                    Array<Box> boxArray   => Transpose(boxArray),
                    Array<Any> anyArray   => anyArray,
                    _ => throw new InvalidOperationException("Invalid argument type")
                };

            private static Array<T> Transpose<T>(Array<T> array)
            {
                if (array.Shape.IsScalar || array.Shape.IsVector)
                    return array;

                var newShape = new Shape([array.Shape.Dimensions[^1], ..array.Shape.Dimensions[..^1]]);
                var newElements = new T[array.Elements.Length];

                for (var y = 0; y < array.Shape.Dimensions[^1]; y++)
                for (var x = 0; x < array.Shape.Dimensions[0]; x++)
                    newElements[y * array.Shape.Dimensions[0] + x] = array.Elements[x * array.Shape.Dimensions[^1] + y];

                return array with { Shape = newShape, Elements = [..newElements] };
            }
        }

        internal sealed record RangeFunction() : UnaryFunction("range")
        {
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope)
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
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                arguments[0] switch
                {
                    Array<int> intArray   => Reverse(intArray),
                    Array<bool> boolArray => Reverse(boolArray),
                    Array<char> charArray => Reverse(charArray),
                    Array<Box> boxArray   => Reverse(boxArray),
                    Array<Any> anyArray   => anyArray,
                    _ => throw new InvalidOperationException("Invalid argument type")
                };

            private static Array<T> Reverse<T>(Array<T> array)
            {
                if (array.Shape.IsScalar)
                    return array;

                if (array.Shape.IsVector)
                    return Array<T>.Vector([.. array.Elements.Reverse()]);

                var newElements = array.Rows().Reverse().SelectMany(element => element);
                return array with { Elements = [..newElements] };
            }
        }

        internal sealed record IndicesFunction() : UnaryFunction("indices")
        {
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
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
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                (arguments[0], arguments[1]) switch
                {
                    (Array<int> intArray, Array<int> newDimensions) => Reshape(intArray, newDimensions),
                    (Array<bool> boolArray, Array<int> newDimensions) => Reshape(boolArray, newDimensions),
                    (Array<char> charArray, Array<int> newDimensions) => Reshape(charArray, newDimensions),
                    (Array<Box> boxArray, Array<int> newDimensions) => Reshape(boxArray, newDimensions),
                    (Array<Any> anyArray, Array<int>) => anyArray,
                    _ => throw new InvalidOperationException("Invalid argument type")
                };

            private static Array<T> Reshape<T>(Array<T> array, Array<int> newDimensions)
            {
                var newShape = new Shape(newDimensions.Elements);
                if (newShape.Count != array.Shape.Count)
                    throw new InvalidOperationException("Invalid reshape dimensions");

                return array with { Shape = newShape };
            }
        }

        internal sealed record ReplicateFunction() : BinaryFunction("replicate")
        {
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                (arguments[0], arguments[1]) switch
                {
                    (Array<Any> _, _) or (_, Array<Any>) => Array<Any>.Empty,
                    (Array<int> intArray, Array<int> replications) => Replicate(intArray, replications),
                    (Array<bool> boolArray, Array<int> replications) => Replicate(boolArray, replications),
                    (Array<char> charArray, Array<int> replications) => Replicate(charArray, replications),
                    (Array<Box> boxArray, Array<int> replications) => Replicate(boxArray, replications),
                    _ => throw new InvalidOperationException("Invalid argument type")
                };

            private static Array<T> Replicate<T>(Array<T> array, Array<int> replications)
            {
                if (replications.Elements.Any(replication => replication < 0))
                    throw new InvalidOperationException("Replication amount must be >= 0");

                if (replications.Shape.Rank > 1)
                    throw new InvalidOperationException("Invalid replication dimensions");

                var newRows = array.Rows()
                    .Zip(replications.Elements.Repeat(), Enumerable.Repeat)
                    .SelectMany(newRow => newRow)
                    .ToArray();
                var newElements = newRows.SelectMany(newRow => newRow).ToArray();
                var newShape = array.Shape.SetFirst(newRows.Length);

                return array with { Shape = newShape, Elements = newElements };
            }
        }

        internal sealed record MaxFunction() : BinaryFunction("max")
        {
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                (arguments[0], arguments[1]) switch
                {
                    (Array<Any> _, _) or (_, Array<Any>) => Array<Any>.Empty,
                    (Array<int> intArray, Array<int> otherIntArray) => intArray.Zip(otherIntArray, Math.Max),
                    (Array<char> charArray, Array<char> otherCharArray) => charArray.Zip(otherCharArray, (a, b) => a >= b ? a : b),
                    (Array<Box> boxArray, var right) => boxArray.Zip(right.Boxes(), (a, b) => Invoke([a.Value, b.Value], interpreter, scope).Box()),
                    (var left, Array<Box> boxArray) => boxArray.Zip(left.Boxes(), (a, b) => Invoke([b.Value, a.Value], interpreter, scope).Box()),
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }

        internal sealed record MinFunction() : BinaryFunction("min")
        {
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                (arguments[0], arguments[1]) switch
                {
                    (Array<Any> _, _) or (_, Array<Any>) => Array<Any>.Empty,
                    (Array<int> intArray, Array<int> otherIntArray) => intArray.Zip(otherIntArray, Math.Min),
                    (Array<char> charArray, Array<char> otherCharArray) => charArray.Zip(otherCharArray, (a, b) => a <= b ? a : b),
                    (Array<Box> boxArray, var right) => boxArray.Zip(right.Boxes(), (a, b) => Invoke([a.Value, b.Value], interpreter, scope).Box()),
                    (var left, Array<Box> boxArray) => boxArray.Zip(left.Boxes(), (a, b) => Invoke([b.Value, a.Value], interpreter, scope).Box()),
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }

        internal sealed record ReduceFunction() : BinaryFunction("reduce")
        {
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                (arguments[0], arguments[1]) switch
                {
                    (Array<Any> _, _) or (_, Array<Any>) => Array<Any>.Empty,
                    (Array<int> intArray, Function reducer) => Reduce(intArray, reducer, interpreter, scope),
                    (Array<char> charArray, Function reducer) => Reduce(charArray, reducer, interpreter, scope),
                    (Array<Box> boxArray, Function reducer) => Reduce(boxArray, reducer, interpreter, scope),
                    _ => throw new InvalidOperationException("Invalid argument type")
                };

            private Value Reduce<T>(Array<T> array, Function reducer, Interpreter interpreter, Scope scope)
            {
                if (array.Shape.IsScalar)
                    return array;

                var newElements = array
                    .Rows()
                    .Select(Array<T>.Vector)
                    .Aggregate((a, b) => (Array<T>)reducer.Invoke([a, b], interpreter, scope));
                var newShape = newElements.Shape.RemoveFirst();
                return newElements with { Shape = newShape };
            }
        }
    }
}
