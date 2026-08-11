using System.Collections;

namespace Arya;

public abstract record Value
{
    public abstract Shape Shape { get; init; }

    public Box Box() => new(this);

    /// <summary>
    /// Views this value as an array of boxes. A box array already is one; anything else becomes
    /// a scalar array holding a single box, so that it broadcasts like any other scalar.
    /// </summary>
    public Array<Box> Boxes() => this as Array<Box> ?? new Array<Box>(Shape.Scalar, Box());
}

public sealed record Box(Value Value) : Value
{
    public Box Map(Func<Value, Value> operation) => new(operation(Value));

    public override Shape Shape { get; init; } = Shape.Scalar;

    public override string ToString() => $"|{Value}|";
}

public sealed record Shape(params int[] Dimensions)
{
    public static readonly Shape Scalar = new();

    public bool IsScalar => Dimensions.Length == 0;
    public bool IsVector => Dimensions.Length == 1;
    public bool IsMatrix => Dimensions.Length == 2;

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

public sealed record EmptyArray : Value
{
    public static readonly EmptyArray Instance = new();

    public override Shape Shape { get; init; } = Shape.Scalar;

    public override string ToString() => $"[] <{Shape}>";
}

public sealed record Array<T>(Shape Shape, params T[] Elements) : Value
{
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

    public Array<T> Map(Func<T, T> operation) =>
        new(Shape, [.. Elements.Select(operation)]);
    
    public Array<T> Map(Func<T[], T[]> operation)
    {
        if (Shape.IsScalar)
            return this;
        
        if (Shape.IsVector)
            return Vector(operation(Elements));

        // TODO: auto promote to boxes if needed        
        // TODO: support matrix
        // TODO: change shape
        var newRows = Rows().Select(operation).ToArray();
        var newElements = newRows.SelectMany(element => element).ToArray();
        return this with { Shape = Shape.Replace(0, newRows.Length), Elements = [..newRows.SelectMany(element => element)] };
    }

    /// <summary>
    /// Applies <paramref name="operation"/> to each pair of elements, stretching whichever
    /// operand is a scalar. Repeating both sides makes the stretch fall out of taking exactly
    /// as many elements as the resulting shape holds.
    /// </summary>
    public Array<T> Zip<TOther>(Array<TOther> other, Func<T, TOther, T> operation)
    {
        var shape = Shape.Broadcast(other.Shape);
        return new Array<T>(shape, [.. Elements.Repeat().Zip(other.Elements.Repeat(), operation).Take(shape.Count)]);
    }

    public Array<T> Append(EmptyArray _) =>
        Shape.IsScalar ? Vector(Elements) : this;

    public Array<T> Append(Array<T> other)
    {
        if ((Shape.IsScalar || Shape.IsVector) && (other.Shape.IsScalar || other.Shape.IsVector))
            return Vector([.. Elements, .. other.Elements]);

        if (Shape != other.Shape)
            throw new InvalidOperationException("Cannot perform binary operations on arrays with different shapes");

        var newElements = Rows().Zip(other.Rows(), (row, otherRow) => row.Concat(otherRow))
            .SelectMany(elements => elements);
        int size = other.Shape.Dimensions[1];
        return new Array<T>(Shape.Replace(1, Shape.Dimensions[1] + size), [.. newElements]);
    }

    public bool Equals(Array<T>? other) =>
        StructuralComparisons.StructuralEqualityComparer.Equals(Elements, other?.Elements) &&
        Shape.Equals(other?.Shape);

    public override int GetHashCode() =>
        HashCode.Combine(
            StructuralComparisons.StructuralEqualityComparer.GetHashCode(Elements),
            Shape.GetHashCode());

    public override string ToString() =>
        Shape.Dimensions.Length switch
        {
            0 when typeof(T) == typeof(char) => '"' + string.Concat(Elements) + '"',
            0 => Elements[0]!.ToString()!,
            1 => "[" + string.Join(" ", Elements) + "] ",
            2 => "[[" + string.Join("] [ ", Elements) + "]]",
            _ => base.ToString()
        };
}

public abstract record Function(string Name) : Value
{
    public override Shape Shape { get; init; } = Shape.Scalar;

    public abstract int Arity { get; }

    public abstract Value Invoke(params Value[] arguments);

    public override string ToString() => Name;
}
