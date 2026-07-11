namespace Gleamy.Tests;

public sealed class ArithmeticTests
{
    [Theory]
    [InlineData("1 + 1", 2)]
    [InlineData("2 * 3", 6)]
    [InlineData("4 - 2", 2)]
    [InlineData("10 / 2", 5)]
    public void Simple(string code, int expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
    
    [Theory]
    [InlineData("2 * (3 + 1)", 8)]
    [InlineData("(1 * (5 - 2)) * (3 + 1)", 12)]
    public void Parenthesized(string code, int expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
    
    [Theory]
    [InlineData("1 + 2 * 3", 7)]
    [InlineData("2 * 3 + 1", 7)]
    [InlineData("4 - 2 * 2", 0)]
    [InlineData("10 / 2 + 3", 8)]
    public void OperatorPrecedence(string code, int expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
}