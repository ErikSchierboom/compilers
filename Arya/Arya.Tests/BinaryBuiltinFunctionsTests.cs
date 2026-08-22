namespace Arya.Tests;

public class BinaryBuiltinFunctionsTests
{
    public static readonly TheoryData<string, Value> ReshapeTestData =
        new()
        {
            { "reshape([], [1])", Array<Any>.Vector() },
            { "reshape([1 2 4 5 6 7], [2 3])", Array<int>.Matrix([[1, 2, 4], [5, 6, 7]]) },
            { "reshape([[-4 -5 -6 -7] [-6 -7 -8 -9]], [2 2 2])", new Array<int>(new Shape(2, 2, 2),-4, -5, -6, -7, -6, -7, -8, -9) },
            { "reshape([[11 13] [15 17]], [4])", Array<int>.Vector(11, 13, 15, 17) },
        };

    [Theory, MemberData(nameof(ReshapeTestData))]
    public void Reshape(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));

    public static readonly TheoryData<string, Value> ReplicateTestData =
        new()
        {
            { "replicate([], [])", Array<Any>.Empty },
            { "replicate(1, 1)", Array<int>.Vector(1) },
            { "replicate(2, 3)", Array<int>.Vector(2, 2, 2) },
            { "replicate([1 2 4], [1 0 1])", Array<int>.Vector(1, 4) },
            { "replicate([1 2 4], [2 1 3])", Array<int>.Vector(1, 1, 2, 4, 4, 4) },
            { "replicate([1 2 3 4], [1 0])", Array<int>.Vector(1, 3) },
            { "replicate([[-4 -5] [-6 -7]], [2 1])", Array<int>.Matrix([[-4, -5], [-4, -5], [-6, -7]]) },
            { "replicate('a', 1)", Array<char>.Vector('a') },
            { "replicate('e', 4)", Array<char>.Vector('e', 'e', 'e', 'e') },
            { "replicate(['a' 'c' 'e'], [1 0 1])", Array<char>.Vector('a', 'e') },
            { "replicate(['a' 'c' 'e'], [2 1 3])", Array<char>.Vector('a', 'a', 'c', 'e', 'e', 'e') },
            { "replicate(['a' 'b' 'c' 'd'], [1 0])", Array<char>.Vector('a', 'c') },
            { "replicate([['a' 'b'] ['c' 'd']], [2 1])", Array<char>.Matrix([['a', 'b'], ['a', 'b'], ['c', 'd']]) },
        };

    [Theory, MemberData(nameof(ReplicateTestData))]
    public void Replicate(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));

    public static readonly TheoryData<string, Value> MaxTestData =
        new()
        {
            { "max(1, 2)", Array<int>.Scalar(2) },
            { "max(3, [2 3 4])", Array<int>.Vector(3, 3, 4) },
            { "max([2 5], [4 3])", Array<int>.Vector(4, 5) },
            { "max([[5 4] [6 7]], 6)", Array<int>.Matrix([[6, 6], [6, 7]]) },
            { "max([[1 5] [3 2]], [[4 1] [2 2]]", Array<int>.Matrix([[4, 5], [3, 2]]) },
            { "max([], 2)", Array<Any>.Empty },
            { "max([|[1]| |[2 3]|], 10)", Array<Box>.Vector(Array<int>.Vector(11).Box(), Array<int>.Vector(12, 13).Box()) },
            { "max(10, [|[1]| |[2 3]|])", Array<Box>.Vector(Array<int>.Vector(11).Box(), Array<int>.Vector(12, 13).Box()) },
            { "max([|[1]| |[2 3]|], [|[10]| |[20 30]|])", Array<Box>.Vector(Array<int>.Vector(11).Box(), Array<int>.Vector(22, 33).Box()) },
            { "max([|[1]| |[2 3]|], [])", Array<Any>.Empty },
            { "max([], [|[1]| |[2 3]|])", Array<Any>.Empty },
            { "max('a', 'b')", Array<char>.Scalar('b') },
            { "max('c' + 'b')", Array<char>.Scalar('c') },
            { "max(['d' 'g' 'i'], 'h')", Array<char>.Vector('h', 'h', 'i') },
            { "max([['e'] ['k'] ['g']], 'f')", Array<char>.Matrix([['f'], ['k'], ['g']]) },
            { "max([|['a' 'b' 'c']| |['d' 'e']|], 'c')", Array<Box>.Vector(Array<char>.Vector('c', 'c', 'c').Box(), Array<char>.Vector('d', 'e').Box()) },
        };

    [Theory, MemberData(nameof(MaxTestData))]
    public void Max(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
}
