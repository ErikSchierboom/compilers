namespace Arya.Tests;

public static class UnaryOperatorsTests
{
    public class Plus
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "+1", Array<int>.Scalar(1) },
                { "+[1 2 3]", Array<int>.Vector(1, 2, 3) },
                { "+[[4 5] [6 7]]", Array<int>.Matrix([[4, 5], [6, 7]]) },
                { "+[|[1]| |[2 3]|]", Array<Box>.Vector(Array<int>.Vector(1).Box(), Array<int>.Vector(2, 3).Box()) },
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
                { "-1", Array<int>.Scalar(-1) },
                { "-[1 2 3]", Array<int>.Vector(-1, -2, -3) },
                { "[-1 -2 -3]", Array<int>.Vector(-1, -2, -3) },
                { "-[[4 5] [6 7]]", Array<int>.Matrix([[-4, -5], [-6, -7]]) },
                { "[[-4] [-5] [-6] [-7]]", Array<int>.Matrix([[-4], [-5], [-6], [-7]]) },
                { "-[|[1]| |[2 3]|]", Array<Box>.Vector(Array<int>.Vector(-1).Box(), Array<int>.Vector(-2, -3).Box()) },
                { "-[|[-1]| |[2 -3]|]", Array<Box>.Vector(Array<int>.Vector(1).Box(), Array<int>.Vector(-2, 3).Box()) },
                { "-[|[1]| |[2 3]| |[4 5 6]|]", Array<Box>.Vector(Array<int>.Vector(-1).Box(), Array<int>.Vector(-2, -3).Box(), Array<int>.Vector(-4, -5, -6).Box()) },
            };

        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }
}