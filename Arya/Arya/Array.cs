using System.Collections;

namespace Arya;

public record Array(int[] Elements, int[] Dimensions)
{
    public static readonly Array Empty = new([], []);

    public Array(int element) : this([element], [])
    {
    }

    public static Array Merge(Array[] arrays)
    {
        if (arrays.Length == 0)
            return Empty;
        
        var uniqueDimensionHashCodes = arrays.Select(array => array.Dimensions)
            .Select(StructuralComparisons.StructuralEqualityComparer.GetHashCode)
            .ToHashSet();
        if (uniqueDimensionHashCodes.Count > 1)
            throw new ArgumentException("Arrays don't have the same shape");
        
        var shape= arrays[0].Dimensions.Prepend(arrays.Length);
        var elements = arrays.SelectMany(array => array.Elements);
        return new Array([..elements], [..shape]);
    }

    public Array Add(Array right) => ApplyBinary(right, (l, r) => l + r);
    public Array Multiply(Array right) => ApplyBinary(right, (l, r) => l * r);

    public Array Append(Array right)
    {
        if (Elements.Length == 0)
            return right;
        
        if (right.Elements.Length == 0)
            return this;
        
        var newElements = Elements.Concat(right.Elements);
        
        if (Dimensions.Length == 0)
            return new Array([..newElements], [Elements.Length + right.Elements.Length]);

        if (right.Dimensions.Length == 0)
            return new Array([..newElements], [Elements.Length + right.Elements.Length]);
        
        if (!Dimensions.AsSpan()[1..].SequenceEqual(right.Dimensions.AsSpan()[1..]))
            throw new ArgumentException("Arrays don't have the same shape");
        
        var newDimensions = Dimensions[1..].Prepend(Dimensions[0] + right.Dimensions[0]);

        return new([.. newElements], [..newDimensions]);
    }
    
    private Array ApplyUnary(Func<int, int> operation) =>
        this with { Elements = [.. Elements.Select(operation)] };

    private Array ApplyBinary(Array right, Func<int, int, int> operation)
    {
        if (!Dimensions.SequenceEqual(right.Dimensions))
            throw new ArgumentException("Arrays don't have the same shape");
        
        return this with { Elements = [.. Elements.Zip(right.Elements, operation)] };
    }
    
    public virtual bool Equals(Array? other)
    {
        if (other is null) return false;
        if (ReferenceEquals(this, other)) return true;
        
        return StructuralComparisons.StructuralEqualityComparer.Equals(Elements, other.Elements) &&
               StructuralComparisons.StructuralEqualityComparer.Equals(Dimensions, other.Dimensions);
    }

    public override int GetHashCode() =>
        HashCode.Combine(
            StructuralComparisons.StructuralEqualityComparer.GetHashCode(Elements),
            StructuralComparisons.StructuralEqualityComparer.GetHashCode(Dimensions));

    public override string ToString() => ArrayPrinter.Print(this);
}

public static class ArrayPrinter
{
    public static string Print(Array array) => $"[{string.Join(" ", array.Elements.Select(Print))}]";
    private static string Print(int i) => i.ToString();
}
