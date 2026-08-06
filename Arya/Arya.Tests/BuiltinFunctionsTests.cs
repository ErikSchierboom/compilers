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
    
    public class Lowercase
    {
        public static readonly TheoryData<string, Value> StringsTestData =
            new()
            {
                { """
                  lowercase("")
                  """, new String("") },
                { """
                  lowercase("HI ThErE!")
                  """, new String("hi there!") },
                { """
                  lowercase("123")
                  """, new String("123") },
                { """
                  lowercase(["THIS" "Is" "CooL"])
                  """, new Array(new String("this"), new String("is"), new String("cool")) },
                { """
                  lowercase([["LET'S"] ["Do"] ["iT"]])
                  """, new Array(new String("let's"), new String("do"), new String("it")) },
            };

        [Theory, MemberData(nameof(StringsTestData))]
        public void Strings(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }
}