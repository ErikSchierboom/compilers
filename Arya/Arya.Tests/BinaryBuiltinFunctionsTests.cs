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
}
