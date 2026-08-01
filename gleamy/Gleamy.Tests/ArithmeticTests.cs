namespace Gleamy.Tests;

public class ArithmeticTests
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
    
    public class Addition
    {
        [Theory]
        [InlineData("1 + 1", 2)]
        [InlineData("2 + 3", 5)]
        [InlineData("88 + 0", 88)]
        public void Scalars(string code, int expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
        
        [Theory]
        [InlineData("5 + Int[]", new int[0])]
        [InlineData("1 + [2, 3]", new[] { 3, 4 })]
        [InlineData("[3, 4] + 2", new[] { 5, 6 })]
        public void ScalarAndVector(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
        
        [Theory]
        [InlineData("Int[] + Int[]", new int[0])]
        [InlineData("[1, 2] + [3, 4]", new[] { 4, 6 })]
        public void Vectors(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
        
        [Fact]
        public void ScalarAndMatrix()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("5 + Int[][]"));
            Assert.Equal(new int[][] { [6], [7, 8] }, Interpreter.Evaluate("5 + [[1], [2, 3]]"));
        }
        
        [Fact]
        public void Matrices()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("Int[][] + Int[][]"));
            Assert.Equal(new int[][] { [6], [7, 9] }, Interpreter.Evaluate("[[1], [2, 3]] + [[5], [5, 6]]"));
        }
    }
    
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