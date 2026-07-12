namespace Gleamy.Tests;

public sealed class ArithmeticTests
{
    [Theory]
    [InlineData("-1", -1)]
    [InlineData("-8", -8)]
    [InlineData("--13", 13)]
    public void UnaryMinus(string code, int expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
    
    [Theory]
    [InlineData("+1", 1)]
    [InlineData("+8", 8)]
    [InlineData("++13", 13)]
    public void UnaryPlus(string code, int expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
    
    [Theory]
    [InlineData("1 + 1", 2)]
    [InlineData("2 + 3", 5)]
    [InlineData("88 + 0", 88)]
    public void Addition(string code, int expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
    
    [Theory]
    [InlineData("1 - 1", 0)]
    [InlineData("5 - 3", 2)]
    [InlineData("88 - 0", 88)]
    public void Subtraction(string code, int expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
    
    [Theory]
    [InlineData("2 * 3", 6)]
    [InlineData("4 * 0", 0)]
    [InlineData("11 * 1", 11)]
    public void Multiplication(string code, int expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
    
    [Theory]
    [InlineData("6 / 3", 2)]
    [InlineData("15 / 3", 5)]
    [InlineData("10 / 1", 10)]
    public void Division(string code, int expected) =>
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