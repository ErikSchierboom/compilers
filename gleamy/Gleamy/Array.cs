namespace Gleamy;

public record Shape(List<int> Dimensions);

public abstract record Array<T>(T[] Elements, Shape Shape);
public sealed record IntArray(int[] Elements, Shape Shape) : Array<int>(Elements, Shape);
public sealed record CharArray(char[] Elements, Shape Shape) : Array<char>(Elements, Shape);
public sealed record BoolArray(bool[] Elements, Shape Shape) : Array<bool>(Elements, Shape);
