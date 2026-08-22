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

    public class Not
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "!1", Array<int>.Scalar(-2) },
                { "![1 2 3]", Array<int>.Vector(-2, -3, -4) },
                { "![[4 5] [6 7]]", Array<int>.Matrix([[-5, -6], [-7, -8]]) },
                { "![|[1]| |[2 3]|]", Array<Box>.Vector(Array<int>.Vector(-2).Box(), Array<int>.Vector(-3, -4).Box()) },
            };

        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        public static readonly TheoryData<string, Value> BooleansTestData =
            new()
            {
                { "!true", Array<bool>.Scalar(false) },
                { "![true false false]", Array<bool>.Vector(false, true, true) },
                { "![[true false] [false true]]", Array<bool>.Matrix([[false, true], [true, false]]) },
                { "![|[true]| |[false true]|]", Array<Box>.Vector(Array<bool>.Vector(false).Box(), Array<bool>.Vector(true, false).Box()) },
            };

        [Theory, MemberData(nameof(BooleansTestData))]
        public void Booleans(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }
}
