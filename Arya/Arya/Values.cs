using System.Collections;

namespace Arya;

public abstract record Value
{
    public abstract Shape Shape { get; init; }
}

public sealed record Shape(params int[] Dimensions)
{
    public static readonly Shape Scalar = new();
    public static Shape Vector<T>(T[] elements) => new(elements.Length);
    public static Shape Matrix(int rows, int columns) => new(rows, columns);
    
    public Shape Prepend(int dimension) => new([dimension, ..Dimensions]);
    
    public bool Equals(Shape? other) => 
        StructuralComparisons.StructuralEqualityComparer.Equals(Dimensions, other?.Dimensions);

    public override int GetHashCode() =>
        StructuralComparisons.StructuralEqualityComparer.GetHashCode(Dimensions);
    
    public override string ToString() => "<" + string.Join(" ", Dimensions.Select(d => d.ToString())) + ">";
}

public abstract record Array<T>(Shape Shape, params T[] Elements) : Value;

public sealed record Empty() : Array<Empty>(Shape.Scalar)
{
    public static readonly Empty Instance = new();
}

public sealed record Integers(Shape Shape, params int[] Elements) : Array<int>(Shape, Elements)
{
    public static Integers Scalar(int element) => new(Shape.Scalar, element);
    public static Integers Vector(params int[] elements) => new(Shape.Vector(elements), elements);
    
    public bool Equals(Integers? other) => 
        StructuralComparisons.StructuralEqualityComparer.Equals(Elements, other?.Elements) &&
        Shape.Equals(other?.Shape);

    public override int GetHashCode() =>
        HashCode.Combine(
            StructuralComparisons.StructuralEqualityComparer.GetHashCode(Elements),
            Shape.GetHashCode());

    public override string ToString() => "[" + string.Join(" ", Elements.Select(e => e.ToString())) + "]";
}

public sealed record Chars(Shape Shape, params char[] Elements) : Array<char>(Shape, Elements)
{
    public bool Equals(Chars? other) => 
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

