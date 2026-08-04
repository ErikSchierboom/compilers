using System.Collections;

namespace Arya;

public abstract record Value;

public sealed record Integer(int Value) : Value;

public sealed record Array(Value[] Elements) : Value
{
    public bool Equals(Array? other) => 
        StructuralComparisons.StructuralEqualityComparer.Equals(Elements, other?.Elements);

    public override int GetHashCode() => 
        StructuralComparisons.StructuralEqualityComparer.GetHashCode(Elements);
}
