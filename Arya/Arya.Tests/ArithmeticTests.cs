namespace Arya.Tests;

public class ArithmeticTests
{
    [Theory, MemberData(nameof(AdditionTestData))]
    public void Addition(string code, Array expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
    
    public static TheoryData<string, Array> AdditionTestData() =>
        new()
        {
            { "1 + 2", new Array([3], []) },
            { "88 + 123", new Array([211], []) }
        };
}