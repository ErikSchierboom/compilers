namespace Gleamy.Tests;

public sealed class LogicalOperationsTests
{
    [Theory]
    [InlineData("true && true", true)]
    [InlineData("true && false", false)]
    [InlineData("false && true", false)]
    [InlineData("false && false", false)]
    public void LogicalAnd(string code, bool expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));

    [Theory]
    [InlineData("true || true", true)]
    [InlineData("true || false", true)]
    [InlineData("false || true", true)]
    [InlineData("false || false", false)]
    public void LogicalOr(string code, bool expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));

    [Theory]
    [InlineData("!true", false)]
    [InlineData("!false", true)]
    public void LogicalNot(string code, bool expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
}