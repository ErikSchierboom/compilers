namespace Gleamy.Tests;

public class LogicalOperationsTests
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

    public class LogicalNot
    {
        [Theory]
        [InlineData("!true", false)]
        [InlineData("!false", true)]
        public void Scalars(string code, bool expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("!Bool[]", new bool[0])]
        [InlineData("![true, false]", new[] { false, true })]
        public void Vectors(string code, bool[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Fact]
        public void Matrices()
        {
            Assert.Equal(Array.Empty<bool[]>(), Interpreter.Evaluate("!Bool[][]"));
            Assert.Equal(new bool[][] { [false], [true, true] }, Interpreter.Evaluate("![[true], [false, false]]"));
        }
    }
}