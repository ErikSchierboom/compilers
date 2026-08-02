namespace Gleamy.Tests;

public class BitwiseOperationsTests
{
    public class Complement
    {
        [Theory]
        [InlineData("~1", -2)]
        [InlineData("~0", -1)]
        [InlineData("~4", -5)]
        [InlineData("~-1", 0)]
        public void Scalars(string code, int expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("~Int[]", new int[0])]
        [InlineData("~[1, 2]", new[] { -2, -3 })]
        public void Vectors(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Fact]
        public void Matrices()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("~Int[][]"));
            Assert.Equal(new int[][] { [-2], [-3, -4] }, Interpreter.Evaluate("~[[1], [2, 3]]"));
        }
    }

    public class And
    {
        [Theory]
        [InlineData("1 & 1", 1)]
        [InlineData("2 & 3", 2)]
        [InlineData("4 & 2", 0)]
        [InlineData("10 & 2", 2)]
        public void Scalars(string code, int expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("5 & Int[]", new int[0])]
        [InlineData("2 & [3, 2]", new[] { 2, 2 })]
        [InlineData("[2, 4] & 3", new[] { 2, 0 })]
        public void ScalarAndVector(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("Int[] & Int[]", new int[0])]
        [InlineData("[2, 4] & [3, 2]", new[] { 2, 0 })]
        public void Vectors(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Fact]
        public void ScalarAndMatrix()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("5 & Int[][]"));
            Assert.Equal(new int[][] { [0], [2, 2] }, Interpreter.Evaluate("2 & [[1], [2, 3]]"));
        }

        [Fact]
        public void Matrices()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("Int[][] & Int[][]"));
            Assert.Equal(new int[][] { [1], [0, 2] }, Interpreter.Evaluate("[[1], [2, 3]] & [[5], [5, 6]]"));
        }
    }

    public class Or
    {
        [Theory]
        [InlineData("1 | 1", 1)]
        [InlineData("2 | 3", 3)]
        [InlineData("4 | 2", 6)]
        [InlineData("10 | 2", 10)]
        public void Scalars(string code, int expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("5 | Int[]", new int[0])]
        [InlineData("1 | [2, 0]", new[] { 3, 1 })]
        [InlineData("[2, 4] | 3", new[] { 3, 7 })]
        public void ScalarAndVector(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("Int[] | Int[]", new int[0])]
        [InlineData("[2, 4] | [3, 2]", new[] { 3, 6 })]
        public void Vectors(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Fact]
        public void ScalarAndMatrix()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("5 | Int[][]"));
            Assert.Equal(new int[][] { [3], [2, 3] }, Interpreter.Evaluate("2 | [[1], [2, 3]]"));
        }

        [Fact]
        public void Matrices()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("Int[][] | Int[][]"));
            Assert.Equal(new int[][] { [5], [7, 7] }, Interpreter.Evaluate("[[1], [2, 3]] | [[5], [5, 6]]"));
        }
    }

    public class Xor
    {
        [Theory]
        [InlineData("1 ^ 1", 0)]
        [InlineData("2 ^ 3", 1)]
        [InlineData("4 ^ 2", 6)]
        [InlineData("10 ^ 2", 8)]
        public void Scalars(string code, int expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("5 ^ Int[]", new int[0])]
        [InlineData("1 ^ [2, 0]", new[] { 3, 1 })]
        [InlineData("[2, 4] ^ 3", new[] { 1, 7 })]
        public void ScalarAndVector(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("Int[] ^ Int[]", new int[0])]
        [InlineData("[2, 4] ^ [3, 2]", new[] { 1, 6 })]
        public void Vectors(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Fact]
        public void ScalarAndMatrix()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("5 ^ Int[][]"));
            Assert.Equal(new int[][] { [3], [0, 1] }, Interpreter.Evaluate("2 ^ [[1], [2, 3]]"));
        }

        [Fact]
        public void Matrices()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("Int[][] ^ Int[][]"));
            Assert.Equal(new int[][] { [4], [7, 5] }, Interpreter.Evaluate("[[1], [2, 3]] ^ [[5], [5, 6]]"));
        }
    }

    public class ShiftLeft
    {
        [Theory]
        [InlineData("1 << 0", 1)]
        [InlineData("1 << 1", 2)]
        [InlineData("4 << 2", 16)]
        [InlineData("10 << 2", 40)]
        public void Scalars(string code, int expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("5 << Int[]", new int[0])]
        [InlineData("1 << [1, 2]", new[] { 2, 4 })]
        [InlineData("[1, 2] << 2", new[] { 4, 8 })]
        public void ScalarAndVector(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("Int[] << Int[]", new int[0])]
        [InlineData("[1, 2] << [2, 3]", new[] { 4, 16 })]
        public void Vectors(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Fact]
        public void ScalarAndMatrix()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("5 << Int[][]"));
            Assert.Equal(new int[][] { [2], [4, 8] }, Interpreter.Evaluate("1 << [[1], [2, 3]]"));
        }

        [Fact]
        public void Matrices()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("Int[][] << Int[][]"));
            Assert.Equal(new int[][] { [32], [64, 192] }, Interpreter.Evaluate("[[1], [2, 3]] << [[5], [5, 6]]"));
        }
    }

    public class ShiftRight
    {
        [Theory]
        [InlineData("1 >> 0", 1)]
        [InlineData("4 >> 1", 2)]
        [InlineData("25 >> 3", 3)]
        [InlineData("88 >> 4", 5)]
        public void Scalars(string code, int expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("5 >> Int[]", new int[0])]
        [InlineData("16 >> [1, 2]", new[] { 8, 4 })]
        [InlineData("[4, 8] >> 2", new[] { 1, 2 })]
        public void ScalarAndVector(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("Int[] >> Int[]", new int[0])]
        [InlineData("[16, 32] >> [2, 3]", new[] { 4, 4 })]
        public void Vectors(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Fact]
        public void ScalarAndMatrix()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("5 >> Int[][]"));
            Assert.Equal(new int[][] { [8], [4, 2] }, Interpreter.Evaluate("16 >> [[1], [2, 3]]"));
        }

        [Fact]
        public void Matrices()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("Int[][] >> Int[][]"));
            Assert.Equal(new int[][] { [1], [0, 0] }, Interpreter.Evaluate("[[32], [64, 128]] >> [[5], [7, 8]]"));
        }
    }
}
