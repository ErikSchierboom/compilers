namespace Gleamy.Tests;

public class ArrayTests
{   
    [Fact]
    public void Empty()
    {
        const string source = "[]";
        Assert.Equal(Array.Empty<object>(), Interpreter.Evaluate(source));
    }

    [Theory]
    [InlineData("[1]", new object[] { 1 })]
    [InlineData("[true, false]", new object[] { true, false })]
    [InlineData("[13, 15, 17]", new object[] { 13, 15, 17 })]
    public void NonEmpty(string code, object[] expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));

    [Theory]
    [InlineData("[[1], [2, 3]]", new object[] { new object[] { 1 }, new object[] { 2, 3 } })]
    [InlineData("[[[true]], [[false]], [[true]]]", new object[] { new object[] { new object[] { true } }, new object[] { new object[] { false } }, new object[] { new object[] { true } } })]
    public void Nested(string code, object[] expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
}