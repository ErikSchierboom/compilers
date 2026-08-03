using System.Collections;

namespace Arya;

public record Array(int[] Elements, int[] Dimensions)
{
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
