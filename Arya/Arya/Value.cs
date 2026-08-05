using System.Collections;

namespace Arya;

public abstract record Value;

public sealed record Integer(int Value) : Value;
public sealed record String(string Value) : Value;

public sealed record Array(params Value[] Elements) : Value
{
    public static Array Append(Array left, Array right) =>
        new([.. left.Elements, .. right.Elements]);
    
    public static Array Append(Array left, Value right) =>
        new([.. left.Elements.Append(right)]);
    
    public static Array Append(Value left, Array right) =>
        new([.. right.Elements.Prepend(left)]);

    public static Array BinaryOp(Array left, Integer right, Func<int, int, int> operation) =>
        left.MapIntegers(li => new Integer(operation(li, right.Value)));
    
    public static Array BinaryOp(Integer left, Array right, Func<int, int, int> operation) =>
        right.MapIntegers(ri => new Integer(operation(left.Value, ri)));

    public static Array BinaryOp(Array left, Array right, Func<int, int, int> operation)
    {
        if (left.Elements.Length != right.Elements.Length)
            throw new ArgumentException("Arrays must have the same length", nameof(left));

        var mappedElements = left.Elements.Zip(right.Elements, 
            (l, r) => (l, r) switch
            {
                (Integer li, Integer ri) => new Integer(operation(li.Value, ri.Value)),
                (Array la, Array ra) => (Value)BinaryOp(la, ra, operation),
                _ => throw new ArgumentOutOfRangeException()
            });

        return new Array([.. mappedElements]);
    }

    private Array MapIntegers(Func<int, Value> map) =>
        Map(element => element switch
        {
            Array array => array.MapIntegers(map),
            Integer integer => map(integer.Value),
            _ => throw new ArgumentOutOfRangeException(nameof(element))
        });

    private Array Map(Func<Value, Value> map) => new([..Elements.Select(map)]);

    public bool Equals(Array? other) => 
        StructuralComparisons.StructuralEqualityComparer.Equals(Elements, other?.Elements);

    public override int GetHashCode() => 
        StructuralComparisons.StructuralEqualityComparer.GetHashCode(Elements);
}
