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
        };

    [Theory, MemberData(nameof(ReplicateTestData))]
    public void Replicate(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
}
