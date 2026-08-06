namespace Arya.Tests;

public class BuiltinFunctionsTests
{
    public class Abs
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "abs(-1)", new Integer(1) },
                { "abs([-1 -2 -3])", new Array(new Integer(1), new Integer(2), new Integer(3)) },
                { "abs([[-4 -5] [-6 -7]])", new Array(new Array(new Integer(4), new Integer(5)), new Array(new Integer(6), new Integer(7))) }
            };

        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }
}