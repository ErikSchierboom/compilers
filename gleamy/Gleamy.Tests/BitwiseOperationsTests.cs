namespace Gleamy.Tests;

public sealed class BitwiseOperationsTests
{
    [Theory]
    [InlineData("1 & 1", 1)]
    [InlineData("2 & 3", 2)]
    [InlineData("4 & 2", 0)]
    [InlineData("10 & 2", 2)]
    public void And(string code, int expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
    
    [Theory]
    [InlineData("1 | 1", 1)]
    [InlineData("2 | 3", 3)]
    [InlineData("4 | 2", 6)]
    [InlineData("10 | 2", 10)]
    public void Or(string code, int expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));

    [Theory]
    [InlineData("1 ^ 1", 0)]
    [InlineData("2 ^ 3", 1)]
    [InlineData("4 ^ 2", 6)]
    [InlineData("10 ^ 2", 8)]
    public void Xor(string code, int expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
    
    // TODO: implement bitwise shift right
    // TODO: implement bitwise shift left
    // TODO: implement bitwise complement
}