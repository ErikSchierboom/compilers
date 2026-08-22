namespace Arya.Tests;

public class AssignmentsTests
{
    public static readonly TheoryData<string, Value> LiteralTestData =
        new()
        {
            { "a = 5", Array<int>.Scalar(5) },
            { "b = 'z'", Array<char>.Scalar('z') },
            { "c = true", Array<bool>.Scalar(true) },
            { """
              d = "abc"
              """, Array<char>.Vector([.."abc"]) },
            { "e = [1 2 3]", Array<int>.Vector(1, 2, 3) },
            { "f = [true false]", Array<bool>.Vector(true, false) },
            { "g = [[1 2] [3 4]]", Array<int>.Matrix([[1, 2], [3, 4]]) },
            { "h = [@[1 2] @[3]]", Array<Box>.Vector(Array<int>.Vector(1, 2).Box(), Array<int>.Vector(3).Box()) },
        };

    [Theory, MemberData(nameof(LiteralTestData))]
    public void Literal(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
}
