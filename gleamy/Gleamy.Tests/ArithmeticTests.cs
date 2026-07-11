namespace Gleamy.Tests;

public sealed class ArithmeticTests
{
    [Theory]
    [InlineData("1 + 1", 2)]
    [InlineData("2 * 3", 6)]
    [InlineData("4 - 2", 2)]
    [InlineData("10 / 2", 5)]
    public void Simple(string code, int expected)
    {
        var result = Interpreter.Evaluate(code);
        Assert.Equal(expected, result);
    }
}