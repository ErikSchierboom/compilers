namespace Gleamy.Tests;

public class LiteralsTests
{
    [Theory]
    [InlineData("1", 1)]
    [InlineData("33", 33)]
    public void Numbers(string code, int expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));

    [Theory]
    [InlineData("true", true)]
    [InlineData("false", false)]
    public void Booleans(string code, bool expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));

    public class Chars
    {
        [Theory]
        [InlineData("'a'", 'a')]
        [InlineData("'7'", '7')]
        [InlineData("' '", ' ')]
        public void Unescaped(string code, char expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
        
        [Theory]
        [InlineData("'\n'", '\n')]
        [InlineData("'\r'", '\r')]
        [InlineData("'\t'", '\t')]
        public void Escaped(string code, char expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }
    
    
}
