namespace Gleamy.Tests;

public sealed class BooleansTests
{
    [Theory]
    [InlineData("true && true", true)]
    [InlineData("true && false", false)]
    [InlineData("false && true", false)]
    [InlineData("false && false", false)]
    public void LogicalAnd(string code, bool expected)
    {
        var result = Interpreter.Evaluate(code);
        Assert.Equal(expected, result);
    }
    
    [Theory]
    [InlineData("true || true", true)]
    [InlineData("true || false", true)]
    [InlineData("false || true", true)]
    [InlineData("false || false", false)]
    public void LogicalOr(string code, bool expected)
    {
        var result = Interpreter.Evaluate(code);
        Assert.Equal(expected, result);
    }
    
    [Theory]
    [InlineData("!true", false)]
    [InlineData("!false", true)]
    public void LogicalNot(string code, bool expected)
    {
        var result = Interpreter.Evaluate(code);
        Assert.Equal(expected, result);
    }
}