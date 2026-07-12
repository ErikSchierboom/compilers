namespace Gleamy.Tests;

public sealed class ComparisonTests
{
    [Theory]
    [InlineData("1 == 1", true)]
    [InlineData("2 == 3", false)]
    [InlineData("true == false", false)]
    [InlineData("true == true", true)]
    public void Equality(string code, bool expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
    
    [Theory]
    [InlineData("1 != 1", false)]
    [InlineData("2 != 3", true)]
    [InlineData("true != false", true)]
    [InlineData("true != true", false)]
    public void Inequality(string code, bool expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
    
    [Theory]
    [InlineData("1 > 1", false)]
    [InlineData("2 > 3", false)]
    [InlineData("3 > 2", true)]
    public void Greater(string code, bool expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
    
    [Theory]
    [InlineData("1 >= 1", true)]
    [InlineData("2 >= 3", false)]
    [InlineData("3 >= 2", true)]
    public void GreaterOrEqual(string code, bool expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
    
    [Theory]
    [InlineData("1 < 1", false)]
    [InlineData("2 < 3", true)]
    [InlineData("3 < 2", false)]
    public void Less(string code, bool expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
    
    [Theory]
    [InlineData("1 <= 1", true)]
    [InlineData("2 <= 3", true)]
    [InlineData("3 <= 2", false)]
    public void LessOrEqual(string code, bool expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
}