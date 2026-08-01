namespace Gleamy.Tests;

public class ArrayTests
{
    public class Vector
    {
        [Fact]
        public void Empty()
        {
            Assert.Equal(Array.Empty<int>(), Interpreter.Evaluate("Int[]"));
            Assert.Equal(Array.Empty<bool>(), Interpreter.Evaluate("Bool[]"));
        }
        
        [Theory]
        [InlineData("[1]", new[] { 1 })]
        [InlineData("[13, 15, 17]", new [] { 13, 15, 17 })]
        public void Integer(string code, int[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    
        [Theory]
        [InlineData("[true]", new [] { true })]
        [InlineData("[true, false]", new [] { true, false })]
        public void Boolean(string code, bool[] expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }

    public class Matrix
    {
        [Fact]
        public void Empty()
        {
            Assert.Equal(Array.Empty<int[]>(), Interpreter.Evaluate("Int[][]"));
            Assert.Equal(Array.Empty<bool[]>(), Interpreter.Evaluate("Bool[][]"));
        }
        
        [Fact]
        public void Integer()
        {   
            Assert.Equal(new[] { new[] { 1 } }, Interpreter.Evaluate("[[1]]"));
            Assert.Equal(new[] { new[] { 1 }, new[] { 2, 3 } }, Interpreter.Evaluate("[[1], [2, 3]]"));
        }

        [Fact]
        public void Boolean()
        {   
            Assert.Equal(new[] { new[] { true } }, Interpreter.Evaluate("[[true]]"));
            Assert.Equal(new[] { new[] { true }, new[] { true, false } }, Interpreter.Evaluate("[[true], [true, false]]"));
        }
    }
}
