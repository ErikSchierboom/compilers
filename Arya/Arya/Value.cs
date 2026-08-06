using System.Collections;

namespace Arya;

public abstract record Value;

public sealed record Integer(int Value) : Value
{
    public override string ToString() => Value.ToString();
}

public sealed record String(string Value) : Value
{
    public String RotateChars(int amount) =>
        new(new string([..Value.Select(c => (char)(c + amount))]));
    
    public override string ToString() => Value;
}

public abstract record Function : Value
{
    public abstract int Arity { get; }
    
    public abstract Value Invoke(params Value[] arguments);
}

public sealed record Array(params Value[] Elements) : Value
{
    public static Array Append(Array left, Array right) =>
        new([.. left.Elements, .. right.Elements]);
    
    public static Array Append(Array left, Value right) =>
        new([.. left.Elements.Append(right)]);
    
    public static Array Append(Value left, Array right) =>
        new([.. right.Elements.Prepend(left)]);
    
    public static Array UnaryOp(Array operand, Func<string, string> operation) =>
        operand.MapStrings(li => new String(operation(li)));

    public static Array UnaryOp(Array operand, Func<int, int> operation) =>
        operand.MapIntegers(li => new Integer(operation(li)));

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

    private Array MapStrings(Func<string, Value> map) =>
        Map(element => element switch
        {
            Array array => array.MapStrings(map),
            String str => map(str.Value),
            _ => throw new ArgumentOutOfRangeException(nameof(element))
        });

    private Array Map(Func<Value, Value> map) => new([..Elements.Select(map)]);

    public bool Equals(Array? other) => 
        StructuralComparisons.StructuralEqualityComparer.Equals(Elements, other?.Elements);

    public override int GetHashCode() => 
        StructuralComparisons.StructuralEqualityComparer.GetHashCode(Elements);
    
    public override string ToString() => "[" + string.Join(" ", Elements.Select(e => e.ToString())) + "]";
}
