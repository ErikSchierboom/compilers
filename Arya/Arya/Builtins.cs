namespace Arya;

public abstract record BuiltinFunction(string Name) : Function
{
    public override string ToString() => Name;

    public static readonly BuiltinFunction[] All =
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
        new Unary.FlattenFunction(),
        new Unary.IndicesFunction(),
        new Unary.PlusFunction(),
        new Unary.MinusFunction(),
        new Unary.NotFunction(),
        new Unary.StringFunction(),

        new Binary.AppendFunction(),
        new Binary.ReshapeFunction(),
        new Binary.ReplicateFunction(),
        new Binary.MaxFunction(),
        new Binary.MinFunction(),
        new Binary.ReduceFunction(),
        new Binary.AddFunction(),
        new Binary.SubtractFunction(),
        new Binary.MultiplyFunction(),
        new Binary.DivideFunction(),
        new Binary.ModuloFunction(),
        new Binary.BitwiseAndFunction(),
        new Binary.BitwiseOrFunction(),
        new Binary.BitwiseShiftLeftFunction(),
        new Binary.BitwiseShiftRightFunction(),
        new Binary.LessFunction(),
        new Binary.LessEqualFunction(),
        new Binary.GreaterFunction(),
        new Binary.GreaterEqualFunction(),
        new Binary.EqualFunction(),
        new Binary.NotEqualFunction(),
    ];

    private static class Unary
    {
        public abstract record UnaryFunction(string Name) : BuiltinFunction(Name)
        {
            public override int Arity => 1;
        }

        public sealed record CountFunction() : UnaryFunction("count")
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

        public sealed record LengthFunction() : UnaryFunction("length")
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

        public sealed record TransposeFunction() : UnaryFunction("transpose")
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

        public sealed record RangeFunction() : UnaryFunction("range")
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

        public sealed record ReverseFunction() : UnaryFunction("reverse")
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

        public sealed record FlattenFunction() : UnaryFunction("flatten")
        {
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                arguments[0] switch
                {
                    Array<int> intArray   => Flatten(intArray),
                    Array<bool> boolArray => Flatten(boolArray),
                    Array<char> charArray => Flatten(charArray),
                    Array<Box> boxArray   => Flatten(boxArray),
                    Array<Any> anyArray   => anyArray,
                    _ => throw new InvalidOperationException("Invalid argument type")
                };

            private static Array<T> Flatten<T>(Array<T> array) => Array<T>.Vector([.. array.Elements]);
        }

        public sealed record IndicesFunction() : UnaryFunction("indices")
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

        public sealed record NotFunction() : UnaryFunction("not")
        {
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                arguments[0] switch
                {
                    Array<int> intArray   => intArray.Unary(i => ~i),
                    Array<bool> boolArray => boolArray.Unary(b => !b),
                    Array<Box> boxArray   => boxArray.Unary(box => Invoke([box.Value], interpreter, scope).Box()),
                    Array<Any> => Array<Any>.Empty,
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }

        public sealed record StringFunction() : UnaryFunction("string")
        {
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                Array<char>.Vector([..arguments[0].ToString()]);
        }

        public abstract record UnaryIntegerFunction(string Name, Func<int, int> Operation) : BuiltinFunction(Name)
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

        public sealed record AbsFunction() : UnaryIntegerFunction("abs", Math.Abs);
        public sealed record PlusFunction() : UnaryIntegerFunction("plus", x => x);
        public sealed record MinusFunction() : UnaryIntegerFunction("minus", x => -x);

        public abstract record UnaryCharFunction(string Name, Func<char, char> Operation) : UnaryFunction(Name)
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

        public sealed record LowercaseFunction() : UnaryCharFunction("lowercase", char.ToLowerInvariant);
        public sealed record UppercaseFunction() : UnaryCharFunction("uppercase", char.ToUpperInvariant);

        public sealed record TrimFunction() : UnaryFunction("trim")
        {
            public override int Arity => 1;

            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                arguments[0] switch
                {
                    Array<char> charArray => Trim(charArray) ,
                    Array<Box> boxArray => boxArray.Unary(box => Invoke([box.Value], interpreter, scope).Box()),
                    Array<Any> => Array<Any>.Empty,
                    _ => throw new InvalidOperationException("Invalid argument type")
                };

            private static Value Trim(Array<char> charArray)
            {
                if (charArray.Shape.IsScalar && char.IsWhiteSpace(charArray.Elements[0]))
                    return Array<char>.Empty;

                return charArray.Binary(str => str.Trim().ToArray());
            }
        }
    }

    private static class Binary
    {
        public abstract record BinaryFunction(string Name) : BuiltinFunction(Name)
        {
            public override int Arity => 2;
        }

        public sealed record AppendFunction() : BinaryFunction("append")
        {
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                (arguments[0], arguments[1]) switch
                {
                    (Array<int> l, Array<int> r) => Append(l, r),
                    (Array<int> l, Array<Any>) => Append(l, Array<int>.Empty),
                    (Array<Any>, Array<int> r) => Append(Array<int>.Empty, r),
                    (Array<char> l, Array<char> r) => Append(l, r),
                    (Array<char> l, Array<Any>) => Append(l, Array<char>.Empty),
                    (Array<Any>, Array<char> r) => Append(Array<char>.Empty, r),
                    (Array<bool> l, Array<bool> r) => Append(l, r),
                    (Array<bool> l, Array<Any>) => Append(l, Array<bool>.Empty),
                    (Array<Any>, Array<bool> r) => Append(Array<bool>.Empty, r),
                    (Array<Any>, Array<Any>) => Array<Any>.Empty,
                    (Array<Box> l, Array<Box> r) => l.Zip(r, (a, b) => Invoke([a.Value, b.Value], interpreter, scope).Box()),
                    _ => throw new InvalidOperationException("Invalid argument type")
                };

            private static Array<T> Append<T>(Array<T> array, Array<T> other)
            {
                if ((array.Shape.IsScalar || array.Shape.IsVector) && (other.Shape.IsScalar || other.Shape.IsVector))
                    return Array<T>.Vector([.. array.Elements, .. other.Elements]);

                if (array.Shape != other.Shape)
                    throw new InvalidOperationException("Cannot perform append on arrays with different shapes");

                var newElements = array.Rows()
                    .Zip(other.Rows(), (row, otherRow) => row.Concat(otherRow))
                    .SelectMany(elements => elements);
                return new Array<T>(array.Shape.Replace(1, array.Shape.Dimensions[1] + other.Shape.Dimensions[1]), [.. newElements]);
            }
        }

        public sealed record ReshapeFunction() : BinaryFunction("reshape")
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

        public sealed record ReplicateFunction() : BinaryFunction("replicate")
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

        public sealed record MaxFunction() : BinaryFunction("max")
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

        public sealed record MinFunction() : BinaryFunction("min")
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

        public sealed record ReduceFunction() : BinaryFunction("reduce")
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

                var reducedArray = array
                    .Rows()
                    .Select(Array<T>.Vector)
                    .Aggregate((a, b) => (Array<T>)reducer.Invoke([a, b], interpreter, scope));

                if (array.Shape.Rank == reducedArray.Shape.Rank && reducedArray.Elements.Length == 1)
                    return reducedArray with { Shape = reducedArray.Shape.RemoveFirst() };

                return reducedArray;
            }
        }

        public abstract record BinaryIntegerAndCharFunction(string Name, Func<int, int, int> Operation) : BinaryFunction(Name)
        {
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                (arguments[0], arguments[1]) switch
                {
                    (Array<Any>, _) or (_, Array<Any>) => Array<Any>.Empty,
                    (Array<int> l, Array<int> r) => l.Zip(r, Operation),
                    (Array<char> l, Array<int> r) => l.Zip(r, (a, b) => (char)(Operation(a, b))),
                    (Array<int> l, Array<char> r) => r.Zip(l, (a, b) => (char)(Operation(b, a))),
                    (Array<Box> boxArray, var right) => boxArray.Zip(right.Boxes(), (a, b) => Invoke([a.Value, b.Value], interpreter, scope).Box()),
                    (var left, Array<Box> boxArray) => boxArray.Zip(left.Boxes(), (a, b) => Invoke([b.Value, a.Value], interpreter, scope).Box()),
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }

        public sealed record AddFunction() : BinaryIntegerAndCharFunction("add", (a, b) => a + b);
        public sealed record SubtractFunction() : BinaryIntegerAndCharFunction("subtract", (a, b) => a - b);

        public abstract record BinaryIntegerFunction(string Name, Func<int, int, int> Operation) : BinaryFunction(Name)
        {
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                (arguments[0], arguments[1]) switch
                {
                    (Array<Any>, _) or (_, Array<Any>) => Array<Any>.Empty,
                    (Array<int> l, Array<int> r) => l.Zip(r, Operation),
                    (Array<Box> boxArray, var right) => boxArray.Zip(right.Boxes(), (a, b) => Invoke([a.Value, b.Value], interpreter, scope).Box()),
                    (var left, Array<Box> boxArray) => boxArray.Zip(left.Boxes(), (a, b) => Invoke([b.Value, a.Value], interpreter, scope).Box()),
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }

        public sealed record MultiplyFunction() : BinaryIntegerFunction("multiply", (a, b) => a * b);
        public sealed record DivideFunction() : BinaryIntegerFunction("divide", (a, b) => a / b);
        public sealed record ModuloFunction() : BinaryIntegerFunction("modulo", (a, b) => a % b);
        public sealed record BitwiseAndFunction() : BinaryIntegerFunction("and", (a, b) => a & b);
        public sealed record BitwiseOrFunction() : BinaryIntegerFunction("or", (a, b) => a | b);
        public sealed record BitwiseShiftLeftFunction() : BinaryIntegerFunction("shiftLeft", (a, b) => a << b);
        public sealed record BitwiseShiftRightFunction() : BinaryIntegerFunction("shiftRight", (a, b) => a >> b);

        public sealed record EqualFunction() : BinaryFunction("equal")
        {
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                (arguments[0], arguments[1]) switch
                {
                    (Array<Any> _, _) or (_, Array<Any>) => Array<Any>.Empty,
                    (Array<int> l, Array<int> r) => l.Zip(r, (a, b) => a == b),
                    (Array<char> l, Array<char> r) => l.Zip(r, (a, b) => a == b),
                    (Array<bool> l, Array<bool> r) => l.Zip(r, (a, b) => a == b),
                    (Array<Box> l, Array<Box> r) => l.Zip(r, (a, b) => Invoke([a.Value, b.Value], interpreter, scope).Box()),
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }

        public sealed record NotEqualFunction() : BinaryFunction("notEqual")
        {
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                (arguments[0], arguments[1]) switch
                {
                    (Array<Any> _, _) or (_, Array<Any>) => Array<Any>.Empty,
                    (Array<int> l, Array<int> r) => l.Zip(r, (a, b) => a != b),
                    (Array<char> l, Array<char> r) => l.Zip(r, (a, b) => a != b),
                    (Array<bool> l, Array<bool> r) => l.Zip(r, (a, b) => a != b),
                    (Array<Box> l, Array<Box> r) => l.Zip(r, (a, b) => Invoke([a.Value, b.Value], interpreter, scope).Box()),
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }

        public sealed record LessFunction() : BinaryFunction("less")
        {
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                (arguments[0], arguments[1]) switch
                {
                    (Array<Any> _, _) or (_, Array<Any>) => Array<Any>.Empty,
                    (Array<int> l, Array<int> r) => l.Zip(r, (a, b) => a < b),
                    (Array<char> l, Array<char> r) => l.Zip(r, (a, b) => a < b),
                    (Array<Box> l, Array<Box> r) => l.Zip(r, (a, b) => Invoke([a.Value, b.Value], interpreter, scope).Box()),
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }

        public sealed record LessEqualFunction() : BinaryFunction("lessEqual")
        {
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                (arguments[0], arguments[1]) switch
                {
                    (Array<Any> _, _) or (_, Array<Any>) => Array<Any>.Empty,
                    (Array<int> l, Array<int> r) => l.Zip(r, (a, b) => a <= b),
                    (Array<char> l, Array<char> r) => l.Zip(r, (a, b) => a <= b),
                    (Array<Box> l, Array<Box> r) => l.Zip(r, (a, b) => Invoke([a.Value, b.Value], interpreter, scope).Box()),
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }

        public sealed record GreaterFunction() : BinaryFunction("greater")
        {
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                (arguments[0], arguments[1]) switch
                {
                    (Array<Any> _, _) or (_, Array<Any>) => Array<Any>.Empty,
                    (Array<int> l, Array<int> r) => l.Zip(r, (a, b) => a > b),
                    (Array<char> l, Array<char> r) => l.Zip(r, (a, b) => a > b),
                    (Array<Box> l, Array<Box> r) => l.Zip(r, (a, b) => Invoke([a.Value, b.Value], interpreter, scope).Box()),
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }
        public sealed record GreaterEqualFunction() : BinaryFunction("greaterEqual")
        {
            public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope) =>
                (arguments[0], arguments[1]) switch
                {
                    (Array<Any> _, _) or (_, Array<Any>) => Array<Any>.Empty,
                    (Array<int> l, Array<int> r) => l.Zip(r, (a, b) => a >= b),
                    (Array<char> l, Array<char> r) => l.Zip(r, (a, b) => a >= b),
                    (Array<Box> l, Array<Box> r) => l.Zip(r, (a, b) => Invoke([a.Value, b.Value], interpreter, scope).Box()),
                    _ => throw new InvalidOperationException("Invalid argument type")
                };
        }
    }
}
