namespace Gleamy.Tests;

public class ComparisonTests
{
    public class Equality
    {
        [Theory]
        [InlineData("1 == 1", true)]
        [InlineData("2 == 3", false)]
        [InlineData("true == false", false)]
        [InlineData("true == true", true)]
        public void Scalars(string code, bool expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("5 == Int[]", new bool[0])]
        [InlineData("1 == [1, 2]", new[] { true, false })]
        [InlineData("[1, 2] == 2", new[] { false, true })]
        [InlineData("false == [false]", new[] { true })]
        [InlineData("true == [true, false]", new[] { true, false })]
        public void ScalarAndVector(string code, bool[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("Int[] == Int[]", new bool[0])]
        [InlineData("Bool[] == Bool[]", new bool[0])]
        [InlineData("[1, 2] == [1, 3]", new[] { true, false })]
        [InlineData("[true] == [false]", new[] { false })]
        public void Vectors(string code, bool[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Fact]
        public void ScalarAndMatrix()
        {
            Assert.Equal(Array.Empty<bool[]>(), Interpreter.Evaluate("5 == Int[][]"));
            Assert.Equal(Array.Empty<bool[]>(), Interpreter.Evaluate("false == Bool[][]"));
            Assert.Equal(new bool[][] { [true], [false, false] }, Interpreter.Evaluate("1 == [[1], [2, 3]]"));
            Assert.Equal(new bool[][] { [true], [false, true] }, Interpreter.Evaluate("true == [[true], [false, true]]"));
        }

        [Fact]
        public void Matrices()
        {
            Assert.Equal(Array.Empty<bool[]>(), Interpreter.Evaluate("Int[][] == Int[][]"));
            Assert.Equal(Array.Empty<bool[]>(), Interpreter.Evaluate("Bool[][] == Bool[][]"));
            Assert.Equal(new bool[][] { [false], [false, false] }, Interpreter.Evaluate("[[1], [2, 3]] == [[5], [5, 6]]"));
        }
    }

    public class Inequality
    {
        [Theory]
        [InlineData("1 != 1", false)]
        [InlineData("2 != 3", true)]
        [InlineData("true != false", true)]
        [InlineData("true != true", false)]
        public void Scalars(string code, bool expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("5 != Int[]", new bool[0])]
        [InlineData("false != Bool[]", new bool[0])]
        [InlineData("1 != [1, 2]", new[] { false, true })]
        [InlineData("[1, 2] != 2", new[] { true, false })]
        [InlineData("true != [true, false]", new[] { false, true })]
        [InlineData("[true, false] != true", new[] { false, true })]
        public void ScalarAndVector(string code, bool[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("Int[] != Int[]", new bool[0])]
        [InlineData("Bool[] != Bool[]", new bool[0])]
        [InlineData("[1, 2] != [1, 3]", new[] { false, true })]
        public void Vectors(string code, bool[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Fact]
        public void ScalarAndMatrix()
        {
            Assert.Equal(Array.Empty<bool[]>(), Interpreter.Evaluate("5 != Int[][]"));
            Assert.Equal(Array.Empty<bool[]>(), Interpreter.Evaluate("false != Bool[][]"));
            Assert.Equal(new bool[][] { [false], [true, true] }, Interpreter.Evaluate("1 != [[1], [2, 3]]"));
            Assert.Equal(new bool[][] { [false], [true, false] }, Interpreter.Evaluate("true != [[true], [false, true]]"));
        }

        [Fact]
        public void Matrices()
        {
            Assert.Equal(Array.Empty<bool[]>(), Interpreter.Evaluate("Int[][] != Int[][]"));
            Assert.Equal(Array.Empty<bool[]>(), Interpreter.Evaluate("Bool[][] != Bool[][]"));
            Assert.Equal(new bool[][] { [true], [true, true] }, Interpreter.Evaluate("[[1], [2, 3]] != [[5], [5, 6]]"));
        }
    }

    public class Greater
    {
        [Theory]
        [InlineData("1 > 1", false)]
        [InlineData("2 > 3", false)]
        [InlineData("3 > 2", true)]
        public void Scalars(string code, bool expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("5 > Int[]", new bool[0])]
        [InlineData("3 > [2, 3, 4]", new[] { true, false, false })]
        [InlineData("[2, 3, 4] > 3", new[] { false, false, true })]
        public void ScalarAndVector(string code, bool[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("Int[] > Int[]", new bool[0])]
        [InlineData("[5, 2] > [3, 4]", new[] { true, false })]
        public void Vectors(string code, bool[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Fact]
        public void ScalarAndMatrix()
        {
            Assert.Equal(Array.Empty<bool[]>(), Interpreter.Evaluate("5 > Int[][]"));
            Assert.Equal(new bool[][] { [true], [false, false] }, Interpreter.Evaluate("2 > [[1], [2, 3]]"));
        }

        [Fact]
        public void Matrices()
        {
            Assert.Equal(Array.Empty<bool[]>(), Interpreter.Evaluate("Int[][] > Int[][]"));
            Assert.Equal(new bool[][] { [false], [false, false] }, Interpreter.Evaluate("[[1], [2, 3]] > [[5], [5, 6]]"));
        }
    }

    public class GreaterEqual
    {
        [Theory]
        [InlineData("1 >= 1", true)]
        [InlineData("2 >= 3", false)]
        [InlineData("3 >= 2", true)]
        public void Scalars(string code, bool expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("5 >= Int[]", new bool[0])]
        [InlineData("3 >= [2, 3, 4]", new[] { true, true, false })]
        [InlineData("[2, 3, 4] >= 3", new[] { false, true, true })]
        public void ScalarAndVector(string code, bool[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("Int[] >= Int[]", new bool[0])]
        [InlineData("[5, 2] >= [3, 4]", new[] { true, false })]
        public void Vectors(string code, bool[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Fact]
        public void ScalarAndMatrix()
        {
            Assert.Equal(Array.Empty<bool[]>(), Interpreter.Evaluate("5 >= Int[][]"));
            Assert.Equal(new bool[][] { [true], [true, false] }, Interpreter.Evaluate("2 >= [[1], [2, 3]]"));
        }

        [Fact]
        public void Matrices()
        {
            Assert.Equal(Array.Empty<bool[]>(), Interpreter.Evaluate("Int[][] >= Int[][]"));
            Assert.Equal(new bool[][] { [false], [false, false] }, Interpreter.Evaluate("[[1], [2, 3]] >= [[5], [5, 6]]"));
        }
    }

    public class Less
    {
        [Theory]
        [InlineData("1 < 1", false)]
        [InlineData("2 < 3", true)]
        [InlineData("3 < 2", false)]
        public void Scalars(string code, bool expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("5 < Int[]", new bool[0])]
        [InlineData("3 < [2, 3, 4]", new[] { false, false, true })]
        [InlineData("[2, 3, 4] < 3", new[] { true, false, false })]
        public void ScalarAndVector(string code, bool[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("Int[] < Int[]", new bool[0])]
        [InlineData("[5, 2] < [3, 4]", new[] { false, true })]
        public void Vectors(string code, bool[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Fact]
        public void ScalarAndMatrix()
        {
            Assert.Equal(Array.Empty<bool[]>(), Interpreter.Evaluate("5 < Int[][]"));
            Assert.Equal(new bool[][] { [false], [true, true] }, Interpreter.Evaluate("2 < [[1], [3, 4]]"));
        }

        [Fact]
        public void Matrices()
        {
            Assert.Equal(Array.Empty<bool[]>(), Interpreter.Evaluate("Int[][] < Int[][]"));
            Assert.Equal(new bool[][] { [true], [true, true] }, Interpreter.Evaluate("[[1], [2, 3]] < [[5], [5, 6]]"));
        }
    }

    public class LessEqual
    {
        [Theory]
        [InlineData("1 <= 1", true)]
        [InlineData("2 <= 3", true)]
        [InlineData("3 <= 2", false)]
        public void Scalars(string code, bool expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("5 <= Int[]", new bool[0])]
        [InlineData("3 <= [2, 3, 4]", new[] { false, true, true })]
        [InlineData("[2, 3, 4] <= 3", new[] { true, true, false })]
        [InlineData("[1, 2, 3] <= 2", new[] { true, true, false })]
        public void ScalarAndVector(string code, bool[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("Int[] <= Int[]", new bool[0])]
        [InlineData("[5, 2] <= [3, 4]", new[] { false, true })]
        public void Vectors(string code, bool[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Fact]
        public void ScalarAndMatrix()
        {
            Assert.Equal(Array.Empty<bool[]>(), Interpreter.Evaluate("5 <= Int[][]"));
            Assert.Equal(new bool[][] { [false], [true, true] }, Interpreter.Evaluate("2 <= [[1], [3, 4]]"));
        }

        [Fact]
        public void Matrices()
        {
            Assert.Equal(Array.Empty<bool[]>(), Interpreter.Evaluate("Int[][] <= Int[][]"));
            Assert.Equal(new bool[][] { [true], [true, true] }, Interpreter.Evaluate("[[1], [2, 3]] <= [[5], [5, 6]]"));
        }
    }
}
