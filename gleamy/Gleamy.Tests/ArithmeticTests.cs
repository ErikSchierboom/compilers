namespace Gleamy.Tests;

public class ArithmeticTests
{
    public class UnaryMinus
    {
        [Theory]
        [InlineData("-1", -1)]
        [InlineData("-8", -8)]
        [InlineData("--13", 13)]
        public void Scalars(string code, int expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("-Int[]", new int[0])]
        [InlineData("-[1, 2]", new[] { -1, -2 })]
        public void Vectors(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Fact]
        public void Matrices()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("-Int[][]"));
            Assert.Equal(new int[][] { [-1], [-2, -3] }, Interpreter.Evaluate("-[[1], [2, 3]]"));
        }
    }

    public class UnaryPlus
    {
        [Theory]
        [InlineData("+1", 1)]
        [InlineData("+8", 8)]
        [InlineData("++13", 13)]
        public void Scalars(string code, int expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("+Int[]", new int[0])]
        [InlineData("+[1, 2]", new[] { 1, 2 })]
        public void Vectors(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Fact]
        public void Matrices()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("+Int[][]"));
            Assert.Equal(new int[][] { [1], [2, 3] }, Interpreter.Evaluate("+[[1], [2, 3]]"));
        }
    }
    
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
    
        public class Subtraction
    {
        [Theory]
        [InlineData("1 - 1", 0)]
        [InlineData("5 - 3", 2)]
        [InlineData("88 - 0", 88)]
        public void Scalars(string code, int expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("5 - Int[]", new int[0])]
        [InlineData("1 - [2, 3]", new[] { -1, -2 })]
        [InlineData("[3, 4] - 2", new[] { 1, 2 })]
        public void ScalarAndVector(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("Int[] - Int[]", new int[0])]
        [InlineData("[1, 2] - [3, 4]", new[] { -2, -2 })]
        public void Vectors(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Fact]
        public void ScalarAndMatrix()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("5 - Int[][]"));
            Assert.Equal(new int[][] { [4], [3, 2] }, Interpreter.Evaluate("5 - [[1], [2, 3]]"));
        }

        [Fact]
        public void Matrices()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("Int[][] - Int[][]"));
            Assert.Equal(new int[][] { [-4], [-3, -3] }, Interpreter.Evaluate("[[1], [2, 3]] - [[5], [5, 6]]"));
        }
    }

    public class Multiplication
    {
        [Theory]
        [InlineData("2 * 3", 6)]
        [InlineData("4 * 0", 0)]
        [InlineData("11 * 1", 11)]
        public void Scalars(string code, int expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("5 * Int[]", new int[0])]
        [InlineData("2 * [2, 3]", new[] { 4, 6 })]
        [InlineData("[3, 4] * 2", new[] { 6, 8 })]
        public void ScalarAndVector(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("Int[] * Int[]", new int[0])]
        [InlineData("[1, 2] * [3, 4]", new[] { 3, 8 })]
        public void Vectors(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Fact]
        public void ScalarAndMatrix()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("5 * Int[][]"));
            Assert.Equal(new int[][] { [5], [10, 15] }, Interpreter.Evaluate("5 * [[1], [2, 3]]"));
        }

        [Fact]
        public void Matrices()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("Int[][] * Int[][]"));
            Assert.Equal(new int[][] { [5], [10, 18] }, Interpreter.Evaluate("[[1], [2, 3]] * [[5], [5, 6]]"));
        }
    }

    public class Division
    {
        [Theory]
        [InlineData("6 / 3", 2)]
        [InlineData("15 / 3", 5)]
        [InlineData("10 / 1", 10)]
        public void Scalars(string code, int expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("5 / Int[]", new int[0])]
        [InlineData("12 / [2, 3]", new[] { 6, 4 })]
        [InlineData("[12, 15] / 3", new[] { 4, 5 })]
        public void ScalarAndVector(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("Int[] / Int[]", new int[0])]
        [InlineData("[12, 12] / [3, 4]", new[] { 4, 3 })]
        public void Vectors(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Fact]
        public void ScalarAndMatrix()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("5 / Int[][]"));
            Assert.Equal(new int[][] { [6], [4, 3] }, Interpreter.Evaluate("12 / [[2], [3, 4]]"));
        }

        [Fact]
        public void Matrices()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("Int[][] / Int[][]"));
            Assert.Equal(new int[][] { [4], [3, 3] }, Interpreter.Evaluate("[[12], [15, 18]] / [[3], [5, 6]]"));
        }
    }

    public class Modulus
    {
        [Theory]
        [InlineData("7 % 3", 1)]
        [InlineData("10 % 2", 0)]
        [InlineData("8 % 5", 3)]
        public void Scalars(string code, int expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("5 % Int[]", new int[0])]
        [InlineData("10 % [3, 4]", new[] { 1, 2 })]
        [InlineData("[12, 15] % 5", new[] { 2, 0 })]
        public void ScalarAndVector(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("Int[] % Int[]", new int[0])]
        [InlineData("[12, 15] % [5, 4]", new[] { 2, 3 })]
        public void Vectors(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Fact]
        public void ScalarAndMatrix()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("5 % Int[][]"));
            Assert.Equal(new int[][] { [1], [2, 0] }, Interpreter.Evaluate("10 % [[3], [4, 5]]"));
        }

        [Fact]
        public void Matrices()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("Int[][] % Int[][]"));
            Assert.Equal(new int[][] { [2], [1, 2] }, Interpreter.Evaluate("[[12], [15, 18]] % [[5], [7, 8]]"));
        }
    }
    
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