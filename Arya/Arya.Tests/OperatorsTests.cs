namespace Arya.Tests;

public class OperatorsTests
{
    public class Addition
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "1 + 2", new Integer(3) },
                { "1 + []", new Array() },
                { "1 + [2 3 4]", new Array(new Integer(3), new Integer(4), new Integer(5)) },
                { "[2 3] + [4 5]", new Array(new Integer(6), new Integer(8)) },
                { "[[5] [6 7]] + 2", new Array(new Array(new Integer(7)), new Array(new Integer(8), new Integer(9))) }
            };

        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
        
        public static readonly TheoryData<string, Value> StringsTestData =
            new()
            {
                { """
                  "abc" + 1
                  """, new String("bcd") },
                { """
                  3 + "efg"
                  """, new String("hij") },
                // { "1 + []", new Array() },
                // { "1 + [2 3 4]", new Array(new Integer(3), new Integer(4), new Integer(5)) },
                // { "[2 3] + [4 5]", new Array(new Integer(6), new Integer(8)) },
                // { "[[5] [6 7]] + 2", new Array(new Array(new Integer(7)), new Array(new Integer(8), new Integer(9))) }
            };

        [Theory, MemberData(nameof(StringsTestData))]
        public void Strings(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }
    
    public static readonly TheoryData<string, Value> MultiplicationTestData =
        new()
        {
            { "2 * 3", new Integer(6) }
        };
    
    [Theory, MemberData(nameof(MultiplicationTestData))]
    public void Multiplication(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
    
    public static readonly TheoryData<string, Value> OperatorPrecedenceTestData =
        new()
        {
            { "2 + 3 * 4", new Integer(14) },
            { "(2 + 3) * 4", new Integer(20) }
        };

    public class Append
    {
        public static readonly TheoryData<string, Value> StringTestData =
            new()
            {
                { """
                  "" ++ ""
                  """, new String("") },
                { """
                  "" ++ "hi"
                  """, new String("hi") },
                { """
                  "hel" ++ "lo"
                  """, new String("hello") }
            };

        [Theory, MemberData(nameof(StringTestData))]
        public void Strings(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
        
        public static readonly TheoryData<string, Value> ArraysTestData =
            new()
            {
                { "[] ++ []", new Array() },
                { "[1] ++ []", new Array(new Integer(1)) },
                { "[] ++ [1 2]", new Array(new Integer(1), new Integer(2)) },
                { "[1] ++ [2 3]", new Array(new Integer(1), new Integer(2), new Integer(3)) },
                { "4 ++ [5 6]", new Array(new Integer(4), new Integer(5), new Integer(6)) },
                { "[] ++ [7 9]", new Array(new Integer(7), new Integer(9)) },
            };

        [Theory, MemberData(nameof(ArraysTestData))]
        public void Arrays(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }
    
    [Theory, MemberData(nameof(OperatorPrecedenceTestData))]
    public void OperatorPrecedence(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
}