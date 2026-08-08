namespace Arya.Tests;

public class IndexersTests
{
    public static readonly TheoryData<string, Value> IntegersTestData =
        new()
        {
            { "[1 2 3][2]", IntArray.Scalar(2) },
        };

    [Theory, MemberData(nameof(IntegersTestData))]
    public void Integers(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
}
