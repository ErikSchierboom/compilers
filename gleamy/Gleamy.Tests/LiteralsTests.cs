namespace Gleamy.Tests;

public sealed class LiteralsTests
{
    [Theory]
    [InlineData("1", 1)]
    [InlineData("33", 33)]
    public void Numbers(string code, int expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));

    [Theory]
    [InlineData("true", true)]
    [InlineData("false", false)]
    public void Booleans(string code, bool expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
}