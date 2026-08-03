using System.Collections;

namespace Arya;

public record Array(int[] Elements, int[] Dimensions)
{
    public static Array ApplyUnary(Array left, Func<int, int> operation)
    {
        return left with { Elements = [.. left.Elements.Select(operation)] };
    }
    
    public static Array ApplyBinary(Array left, Array right, Func<int, int, int> operation)
    {
        if (left.Dimensions.Length != right.Dimensions.Length)
            throw new ArgumentException("Arrays must have the same dimensions");

        return left with { Elements = [.. left.Elements.Zip(right.Elements, operation)] };
    }
    
    public static Array Merge(params Array[] arrays)
    {
        var elements = arrays.SelectMany(a => a.Elements);
        var dimensions = arrays.SelectMany(a => a.Dimensions);
        return new Array([.. elements], [.. dimensions.Prepend(arrays.Length)]);
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
}
