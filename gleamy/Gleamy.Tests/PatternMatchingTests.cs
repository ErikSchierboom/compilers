namespace Gleamy.Tests;

public sealed class PatternMatchingTests
{
    public sealed class Patterns
    {
        [Theory]
        [InlineData("match 5 { 5 => 1 }", 1)]
        [InlineData("match true { true => 2 }", 2)]
        public void Constant(string code, int expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
        
        [Theory]
        [InlineData("match 5 { !4 => 1 }", 1)]
        [InlineData("match true { !false => 2 }", 2)]
        public void Negation(string code, int expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    
        [Theory]
        [InlineData("match 5 { >  4 => 1 }", 1)]
        [InlineData("match 5 { >= 4 => 2 }", 2)]
        [InlineData("match 6 { <  7 => 3 }", 3)]
        [InlineData("match 6 { <= 7 => 4 }", 4)]
        public void Comparison(string code, int expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Theory]
        [InlineData("match true { _ => 2 }", 2)]
        [InlineData("match 5 { 2 => 0, _ => 1 }", 1)]
        [InlineData("match 4 { _ => 3, 4 => 1 }", 3)]
        public void Discard(string code, int expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
        
        [Theory]
        [InlineData("match 5 { i => i + 6 }", 11)]
        [InlineData("match true { b => !b }", false)]
        public void Binding(string code, object expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }

    public sealed class Evaluation
    {
        [Fact]
        public void ReturnValueOfMatchingPattern()
        {
            const string code = 
                """
                match false { 
                  true => 3,
                  false => 4
                }
                """;

            Assert.Equal(4, Interpreter.Evaluate(code));
        }

        [Fact]
        public void ReturnValueOfFirstMatchingPatternWhenMultiplePatternsMatch()
        {
            const string code = 
                """
                match 2 {
                  > 5 => 0,
                  > 1 => 1,
                  > 0 => 2
                } 
                """;
            Assert.Equal(1, Interpreter.Evaluate(code));
        }
    
        [Fact]
        public void ThrowsWhenNoPatternMatches()
        {
            const string code = 
                """
                match true {
                  false => 1
                } 
                """;
            Assert.Throws<InvalidOperationException>(() => Interpreter.Evaluate(code));
        }
    }
}