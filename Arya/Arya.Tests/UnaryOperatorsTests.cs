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
                { "+[[4 5] [6 7]]", IntArray.Matrix([[4, 5], [6, 7]]) }
            };

        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        public static readonly TheoryData<string, Value> BoxedIntegersTestData =
            new()
            {
                { "+[[1] [2 3]]", BoxArray.Vector(IntArray.Vector(1).Box(), IntArray.Vector(2, 3).Box()) },
            };

        [Theory, MemberData(nameof(BoxedIntegersTestData))]
        public void BoxedIntegers(string code, Value expected) =>
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
                { "-[[4 5] [6 7]]", IntArray.Matrix([[-4, -5], [-6, -7]]) },
                { "[[-4] [-5] [-6] [-7]]", IntArray.Matrix([[-4], [-5], [-6], [-7]]) },
            };

        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
        
        public static readonly TheoryData<string, Value> BoxedIntegersTestData =
            new()
            {
                { "-[[1] [2 3]]", BoxArray.Vector(IntArray.Vector(-1).Box(), IntArray.Vector(-2, -3).Box()) },
                { "-[[-1] [2 -3]]", BoxArray.Vector(IntArray.Vector(1).Box(), IntArray.Vector(-2, 3).Box()) },
                { "-[[1] [2 3] [4 5 6]]", BoxArray.Vector(IntArray.Vector(-1).Box(), IntArray.Vector(-2, -3).Box(), IntArray.Vector(-4, -5, -6).Box()) },
            };

        [Theory, MemberData(nameof(BoxedIntegersTestData))]
        public void BoxedIntegers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }
}