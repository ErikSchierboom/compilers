using System.Collections;

namespace Arya;

public abstract record Value
{
    public abstract Shape Shape { get; init; }

    public Box Box() => new(this);
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

    public static Shape Vector(int length) => new(length);
    public static Shape Vector<T>(T[] elements) => new(elements.Length);

    public static Shape Matrix(int rows, int columns) => new(rows, columns);
    public static Shape Matrix<T>(T[][] elements) => new(elements.Length, elements[0].Length);

    public bool IsScalar => Dimensions.Length == 0;
    public bool IsVector => Dimensions.Length == 1;
    public bool IsMatrix => Dimensions.Length == 2;

    public int Count => Dimensions.Aggregate(1, (count, dimension) => count * dimension);
    public int RowCount => Dimensions.Length >= 2 ? Dimensions[1] : 1;

    /// <summary>
    /// The shape that results from combining this shape with <paramref name="other"/>,
    /// where a scalar operand is stretched to match the other operand.
    /// </summary>
    public Shape Broadcast(Shape other) =>
        IsScalar       ? other :
        other.IsScalar ? this  :
        this == other  ? this  :
        throw new InvalidOperationException("Cannot perform binary operations on arrays with different shapes");

    public Shape Prepend(int dimension) => new([dimension, ..Dimensions]);
    public Shape Increment(int dimension, int size) => Replace(dimension, Dimensions[dimension] + size);
    public Shape Replace(int dimension, int size) => new([..Dimensions[..dimension], size, ..Dimensions[(dimension + 1)..]]);
    public Shape RemoveFirst() => new([.. Dimensions.Skip(1)]);

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

    public override string ToString() => "[]";
}

public abstract record Array<T>(Shape Shape, params T[] Elements) : Value
{
    public virtual bool Equals(Array<T>? other) =>
        StructuralComparisons.StructuralEqualityComparer.Equals(Elements, other?.Elements) &&
        Shape.Equals(other?.Shape);

    public override int GetHashCode() =>
        HashCode.Combine(
            StructuralComparisons.StructuralEqualityComparer.GetHashCode(Elements),
            Shape.GetHashCode());
}

/// <summary>
/// An array that knows its own concrete type, so that operations defined in terms of shapes
/// alone can be written once here and still return the element type they started with.
/// </summary>
public abstract record Array<TSelf, T>(Shape Shape, params T[] Elements) : Array<T>(Shape, Elements)
    where TSelf : Array<TSelf, T>
{
    protected abstract TSelf Create(Shape shape, T[] elements);

    public IEnumerable<T[]> Rows =>
        Shape.IsScalar || Shape.IsVector ? [Elements] : Elements.Chunk(Shape.Dimensions[0]);

    public TSelf Map(Func<T, T> operation) =>
        Create(Shape, [.. Elements.Select(operation)]);

    /// <summary>
    /// Applies <paramref name="operation"/> to each pair of elements, stretching whichever
    /// operand is a scalar. Repeating both sides makes the stretch fall out of taking exactly
    /// as many elements as the resulting shape holds.
    /// </summary>
    public TSelf Zip<TOther>(Array<TOther> other, Func<T, TOther, T> operation)
    {
        var shape = Shape.Broadcast(other.Shape);
        return Create(shape, [.. Elements.Repeat().Zip(other.Elements.Repeat(), operation).Take(shape.Count)]);
    }

    public TSelf Append(EmptyArray _) =>
        Shape.IsScalar ? Create(Shape.Vector(Elements), Elements) : (TSelf)this;

    public TSelf Append(TSelf other)
    {
        if ((Shape.IsScalar || Shape.IsVector) && (other.Shape.IsScalar || other.Shape.IsVector))
            return Create(Shape.Vector(Elements.Length + other.Elements.Length), [.. Elements, .. other.Elements]);

        if (Shape != other.Shape)
            throw new InvalidOperationException("Cannot perform binary operations on arrays with different shapes");

        var newElements = Rows.Zip(other.Rows, (row, otherRow) => row.Concat(otherRow))
            .SelectMany(elements => elements);
        return Create(Shape.Increment(1, other.Shape.Dimensions[1]), [.. newElements]);
    }
}

public sealed record IntArray(Shape Shape, params int[] Elements) : Array<IntArray, int>(Shape, Elements)
{
    public static IntArray Scalar(int element) => new(Shape.Scalar, element);

    public static IntArray Vector(params int[] elements) => new(Shape.Vector(elements), elements);

    public static IntArray Matrix(int[][] elements) => new(Shape.Matrix(elements), [.. elements.SelectMany(row => row)]);

    protected override IntArray Create(Shape shape, int[] elements) => new(shape, elements);

    public override string ToString() =>
        Shape.Dimensions.Length switch
        {
            0 => Elements[0].ToString(),
            1 => "[" + string.Join(" ", Elements.Select(e => e.ToString())) + "]",
            2 => "[[" + string.Join("] [ ", Elements.Select(e => e.ToString())) + "]]",
            _ => base.ToString()
        };
}

public sealed record CharArray(Shape Shape, params char[] Elements) : Array<CharArray, char>(Shape, Elements)
{
    public static CharArray Scalar(char element) => new(Shape.Scalar, element);

    public static CharArray Vector(params char[] elements) => new(Shape.Vector(elements), elements);
    public static CharArray Vector(string str) => new(Shape.Vector(str.Length), [..str]);

    public static CharArray Matrix(char[][] elements) => new(Shape.Matrix(elements.Length, elements[0].Length), [.. elements.SelectMany(row => row)]);
    public static CharArray Matrix(params string[] elements) => new(Shape.Matrix(elements.Length, elements[0].Length), [.. elements.SelectMany(row => row)]);

    protected override CharArray Create(Shape shape, char[] elements) => new(shape, elements);

    public override string ToString() =>
        Shape.Dimensions.Length switch
        {
            0 => '"' + new string(Elements) + '"',
            1 => "[" + string.Join(" ", Elements.Select(e => e.ToString())) + "]",
            2 => "[[" + string.Join("] [ ", Elements.Select(e => e.ToString())) + "]]",
            _ => base.ToString()
        };
}

public sealed record BoxArray(Shape Shape, params Box[] Elements) : Array<BoxArray, Box>(Shape, Elements)
{
    public static BoxArray Vector(params Box[] elements) => new(Shape.Vector(elements), elements);

    protected override BoxArray Create(Shape shape, Box[] elements) => new(shape, elements);

    public BoxArray Pervade(Func<Value, Value> operation) =>
        Map(element => element.Map(operation));

    public BoxArray Pervade(Value other, Func<Value, Value, Value> operation) =>
        Zip(Enclose(other), (element, otherElement) => operation(element.Value, otherElement.Value).Box());

    /// <summary>
    /// Views <paramref name="value"/> as a box array, so that a non-boxed operand broadcasts
    /// into every box the same way an ordinary scalar does.
    /// </summary>
    private static BoxArray Enclose(Value value) =>
        value as BoxArray ?? new BoxArray(Shape.Scalar, value.Box());

    public override string ToString() => "[" + string.Join(" ", Elements.Select(e => e.ToString())) + "]";
}

public abstract record Function(string Name) : Value
{
    public override Shape Shape { get; init; } = Shape.Scalar;

    public abstract int Arity { get; }

    public abstract Value Invoke(params Value[] arguments);

    public override string ToString() => Name;
}

// TODO: add BoolArray
// TODO: add FunctionArray
