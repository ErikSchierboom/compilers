using System.Collections;
using System.Text;

namespace Arya;

public abstract record Value
{
    public abstract Shape Shape { get; init; }

    public Box Box() => new(this);

    /// <summary>
    /// Views this value as an array of boxes. A box array already is one; anything else becomes
    /// a scalar array holding a single box so that it broadcasts like any other scalar.
    /// </summary>
    public Array<Box> Boxes() => this as Array<Box> ?? new Array<Box>(Shape.Scalar, Box());
}

public sealed record Box(Value Value) : Value
{
    public override Shape Shape { get; init; } = Shape.Scalar;

    public override string ToString() => ValueRenderer.Render(this);
}

public sealed record Shape(params int[] Dimensions)
{
    public static readonly Shape Scalar = new();
    public static readonly Shape Empty = new(0);

    public bool IsScalar => Dimensions.Length == 0;
    public bool IsVector => Dimensions.Length == 1;
    public bool IsMatrix => Dimensions.Length == 2;

    public int Rank => Dimensions.Length;

    public int Count => Dimensions.Aggregate(1, (count, dimension) => count * dimension);
    public int RowCount => Dimensions.FirstOrDefault(1);
    public int RowLength => Dimensions.Skip(1).Aggregate(1, (count, dimension) => count * dimension);

    /// <summary>
    /// The shape that results from combining this shape with <paramref name="other"/>,
    /// where a scalar operand is stretched to match the other operand.
    /// </summary>
    public Shape Broadcast(Shape other) =>
        IsScalar ? other :
        other.IsScalar ? this :
        this == other ? this :
        throw new InvalidOperationException("Cannot perform binary operations on arrays with different shapes");

    public Shape Prepend(int dimension) => new([dimension, .. Dimensions]);
    public Shape Replace(int dimension, int size) => new([.. Dimensions[..dimension], size, .. Dimensions[(dimension + 1)..]]);
    public Shape RemoveFirst() => IsScalar ? this : new([.. Dimensions.Skip(1)]);
    public Shape SetFirst(int size) => new([size, .. Dimensions.Skip(1)]);

    public bool Equals(Shape? other) =>
        StructuralComparisons.StructuralEqualityComparer.Equals(Dimensions, other?.Dimensions);

    public override int GetHashCode() =>
        StructuralComparisons.StructuralEqualityComparer.GetHashCode(Dimensions);

    public override string ToString() => "<" + string.Join(" ", Dimensions.Select(d => d.ToString())) + ">";
}

public sealed record Array<T>(Shape Shape, params T[] Elements) : Value
{
    public static readonly Array<T> Empty = new(Shape.Empty);

    public static Array<T> Scalar(T element) => new(Shape.Scalar, element);

    public static Array<T> Vector(params T[] elements) => new(new Shape(elements.Length), elements);

    public static Array<T> Matrix(T[][] elements) => new(new Shape(elements.Length, elements[0].Length), [.. elements.SelectMany(row => row)]);

    public IEnumerable<T[]> Rows()
    {
        if (Shape.IsScalar)
            return [Elements];
        
        if (Shape.IsVector)
            return [..Elements.Chunk(1)];
        
        return Elements.Chunk(Shape.Dimensions[0]);
    }

    public Array<T> Unary(Func<T, T> operation) =>
        new(Shape, [.. Elements.Select(operation)]);
    
    public Array<TOut> Binary<TOut>(Func<T[], TOut[]> operation)
    {
        if (Shape.IsScalar)
            return Array<TOut>.Scalar(operation(Elements)[0]);
        
        if (Shape.IsVector)
            return Array<TOut>.Vector(operation(Elements));

        var newRows = Rows().Select(operation).ToArray();
        var newElements = newRows.SelectMany(element => element).ToArray();
        var newShape = Shape.Replace(0, newRows.Length);
        return new Array<TOut>(newShape, newElements);
    }

    /// <summary>
    /// Applies <paramref name="operation"/> to each pair of elements, stretching whichever
    /// operand is a scalar. Repeating both sides makes the stretch fall out of taking exactly
    /// as many elements as the resulting shape holds.
    /// </summary>
    public Array<TOut> Zip<TOther, TOut>(Array<TOther> other, Func<T, TOther, TOut> operation)
    {
        var shape = Shape.Broadcast(other.Shape);
        return new Array<TOut>(shape, [.. Elements.Repeat().Zip(other.Elements.Repeat(), operation).Take(shape.Count)]);
    }

    public bool Equals(Array<T>? other) =>
        StructuralComparisons.StructuralEqualityComparer.Equals(Elements, other?.Elements) &&
        Shape.Equals(other?.Shape);

    public override int GetHashCode() =>
        HashCode.Combine(
            StructuralComparisons.StructuralEqualityComparer.GetHashCode(Elements),
            Shape.GetHashCode());

    public override string ToString() => ValueRenderer.Render(this);

    public Array<int> Indices() => Array<int>.Vector([..Enumerable.Range(1, Shape.RowCount)]);
}

public abstract record Function : Value
{
    public override Shape Shape { get; init; } = Shape.Scalar;

    public abstract int Arity { get; }

    public abstract Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope);
}

public sealed record LambdaFunction(string[] Parameters, Expression Body) : Function
{
    public override int Arity => Parameters.Length;

    public override Value Invoke(Value[] arguments, Interpreter interpreter, Scope scope)
    {
        var functionScope = scope.CreateChild();

        foreach (var (parameter, argument) in Parameters.Zip(arguments))
            functionScope[parameter] = argument;

        return interpreter.Evaluate(Body, functionScope);
    }
}

/// <summary>
/// This type is used as an empty array's element type, as we can't know its type.
/// </summary>
public sealed record Any;

internal sealed class ValueRenderer
{
    public static string Render(Value value) =>
        value switch
        {
            Box box => $"@{Render(box.Value)}",
            BuiltinFunction builtinFunction => $"{builtinFunction.Name}/{builtinFunction.Arity}",
            LambdaFunction lambdaFunction => $"{lambdaFunction.GetHashCode()}/{lambdaFunction.Arity}",
            Array<int> intArray => Render(intArray),
            Array<char> charArray => Render(charArray),
            Array<bool> boolArray => Render(boolArray),
            Array<Box> boxArray => Render(boxArray),
            Array<Any> => "[]",
            _ => throw new ArgumentOutOfRangeException(nameof(value))
        };

    private static string Render(Array<char> array)
    {
        if (array.Shape.IsScalar)
            return Render(array.Elements[0]);

        var sb = new StringBuilder();

        if (array.Shape.IsVector)
        {
            sb.Append('"');
            sb.Append(array.Elements);
            sb.Append('"');
            return sb.ToString();
        }

        for (var index = 0; index < array.Shape.Dimensions.Length - 1; index++)
            sb.Append('[');

        sb.Append('"');

        var chunkSize = array.Shape.Dimensions[^1];
        var numberOfChunks = array.Elements.Length / chunkSize;

        for (var i = 0; i < numberOfChunks; i++)
        {
            sb.Append(array.Elements[(i * chunkSize)..((i + 1) * chunkSize)]);

            if (i < numberOfChunks - 1)
                sb.Append("\" \"");
        }

        sb.Append('"');

        for (var index = 0; index < array.Shape.Dimensions.Length - 1; index++)
            sb.Append(']');

        return sb.ToString();
    }

    private static string Render<T>(Array<T> array)
    {
        if (array.Shape.IsScalar)
            return Render(array.Elements[0]);

        var sb = new StringBuilder();

        for (var index = 0; index < array.Shape.Dimensions.Length; index++)
            sb.Append('[');

        var chunkSize = array.Shape.Dimensions[^1];
        var numberOfChunks = array.Elements.Length / chunkSize;

        for (var i = 0; i < numberOfChunks; i++)
        {
            for (var j = 0; j < chunkSize; j++)
            {
                sb.Append(Render(array.Elements[i * chunkSize + j]!));

                if (j < chunkSize - 1)
                    sb.Append(' ');
            }

            if (i < numberOfChunks - 1)
                sb.Append("] [");
        }

        for (var index = 0; index < array.Shape.Dimensions.Length; index++)
            sb.Append(']');

        return sb.ToString();
    }

    private static string Render<T>(T obj)
    {
        if (obj is bool b)
            return b ? "true" : "false";

        if (obj is char c)
            return $"'{c}'";

        return obj!.ToString()!;
    }
}
