using System.Collections;

namespace Arya;

public abstract record Value
{
    public abstract Shape Shape { get; init; }

    public Box Box() => new(this);
}

public sealed record Box(Value Value) : Value
{
    public override Shape Shape { get; init; } = Shape.Scalar;
}

public sealed record Shape(params int[] Dimensions)
{
    public static readonly Shape Scalar = new();
    public static Shape Vector<T>(T[] elements) => new(elements.Length);
    public static Shape Matrix<T>(T[][] elements) => new(elements.Length, elements[0].Length);
    
    public Shape Prepend(int dimension) => new([dimension, ..Dimensions]);
    
    public bool Equals(Shape? other) => 
        StructuralComparisons.StructuralEqualityComparer.Equals(Dimensions, other?.Dimensions);

    public override int GetHashCode() =>
        StructuralComparisons.StructuralEqualityComparer.GetHashCode(Dimensions);
    
    public override string ToString() => "<" + string.Join(" ", Dimensions.Select(d => d.ToString())) + ">";
}

public abstract record Array<T>(Shape Shape, params T[] Elements) : Value;

public sealed record EmptyArray() : Array<EmptyArray>(Shape.Scalar)
{
    public static readonly EmptyArray Instance = new();
}

public sealed record IntArray(Shape Shape, params int[] Elements) : Array<int>(Shape, Elements)
{
    public static IntArray Scalar(int element) => new(Shape.Scalar, element);
    public static IntArray Vector(params int[] elements) => new(Shape.Vector(elements), elements);
    public static IntArray Matrix(int[][] elements) => new(Shape.Matrix(elements), [.. elements.SelectMany(row => row)]);
    
    public IntArray UnaryOp(Func<int, int> operation) => new(Shape, [.. Elements.Select(operation)]);
    
    public bool Equals(IntArray? other) => 
        StructuralComparisons.StructuralEqualityComparer.Equals(Elements, other?.Elements) &&
        Shape.Equals(other?.Shape);

    public override int GetHashCode() =>
        HashCode.Combine(
            StructuralComparisons.StructuralEqualityComparer.GetHashCode(Elements),
            Shape.GetHashCode());

    public override string ToString() => "[" + string.Join(" ", Elements.Select(e => e.ToString())) + "]";
}

public sealed record CharArray(Shape Shape, params char[] Elements) : Array<char>(Shape, Elements)
{
    public static CharArray Scalar(char element) => new(Shape.Scalar, element);
    public static CharArray Vector(params char[] elements) => new(Shape.Vector(elements), elements);
    public static CharArray Matrix(char[][] elements) => new(Shape.Matrix(elements), [.. elements.SelectMany(row => row)]);

    public bool Equals(CharArray? other) => 
        StructuralComparisons.StructuralEqualityComparer.Equals(Elements, other?.Elements) &&
        Shape.Equals(other?.Shape);

    public override int GetHashCode() =>
        HashCode.Combine(
            StructuralComparisons.StructuralEqualityComparer.GetHashCode(Elements),
            Shape.GetHashCode());
}

public sealed record BoxArray(Shape Shape, params Box[] Elements) : Array<Box>(Shape, Elements)
{
    public static BoxArray Vector(params Box[] elements) => new(Shape.Vector(elements), elements);
    public static BoxArray Matrix(Box[][] elements) => new(Shape.Matrix(elements), [.. elements.SelectMany(row => row)]);
    
    public bool Equals(BoxArray? other) => 
        StructuralComparisons.StructuralEqualityComparer.Equals(Elements, other?.Elements) &&
        Shape.Equals(other?.Shape);

    public override int GetHashCode() =>
        HashCode.Combine(
            StructuralComparisons.StructuralEqualityComparer.GetHashCode(Elements),
            Shape.GetHashCode());
}

public abstract record Function(string Name) : Value
{
    public abstract int Arity { get; }
    
    public abstract Value Invoke(params Value[] arguments);
    
    public override string ToString() => Name;
}

// TODO: add BoolArray
// TODO: add FunctionArray

