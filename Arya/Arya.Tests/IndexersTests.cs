namespace Arya.Tests;

public class IndexersTests
{
    public static readonly TheoryData<string, Value> IntegersTestData =
        new()
        {
            { "[4 5 6][2]", IntArray.Scalar(5) },
            { "[4 5 6][[1 3]]", IntArray.Vector(4, 6) },
            { "[4 5 6][[1 2 1 3]]", IntArray.Vector(4, 5, 4, 6) },
        };

    [Theory, MemberData(nameof(IntegersTestData))]
    public void Integers(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
}
