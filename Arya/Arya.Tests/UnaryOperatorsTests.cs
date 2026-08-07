namespace Arya.Tests;

public static class UnaryOperatorsTests
{
    public class Plus
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "+1", IntArray.Scalar(1) },
                { "+[1 2 3]", IntArray.Vector(1, 2, 3) },
                { "+[[4 5] [6 7]]", IntArray.Matrix(2, 2, 4, 5, 6, 7) }
            };

        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }
    
    public class Minus
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "-1", IntArray.Scalar(-1) },
                { "-[1 2 3]", IntArray.Vector(-1, -2, -3) },
                { "[-1 -2 -3]", IntArray.Vector(-1, -2, -3) },
                { "-[[4 5] [6 7]]", IntArray.Matrix(2, 2, -4, -5, -6, -7) },
                { "[[-4] [-5] [-6] [-7]]", IntArray.Matrix(4, 1, -4, -5, -6, -7) }
            };

        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }
}