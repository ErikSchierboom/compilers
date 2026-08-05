namespace Arya.Tests;

public class LiteralsTests
{
    public class Numbers
    {
        public static TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "5", new Integer(5) },
                { "123", new Integer(123) }
            };
    
        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }
    
    public class Strings
    {
        public static TheoryData<string, Value> UnescapedTestData =
            new()
            {
                { """
                  ""
                  """, new String("") },
                { """
                  "a"
                  """, new String("a") },
                { """
                  "2"
                  """, new String("2") },
                { """
                  "hello there 123!"
                  """, new String("hello there 123!") }
            };
    
        [Theory, MemberData(nameof(UnescapedTestData))]
        public void Unescaped(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
        
        public static TheoryData<string, Value> EscapedTestData =
            new()
            {
                { """
                  "\t"
                  """, new String("\t") },
                { """
                  "\r"
                  """, new String("\r") },
                { """
                  "\n"
                  """, new String("\n") },
                { """
                  "\\"
                  """, new String("\\") },
                { """
                  "hey\tyou!\r\n"
                  """, new String("hey\tyou!\r\n") },
            };
    
        [Theory, MemberData(nameof(EscapedTestData))]
        public void Escaped(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }

    public class Arrays
    {
        public static TheoryData<string, Array> VectorsTestData =
            new()
            {
                { "[]", new Array() },
                { "[3]", new Array(new Integer(3)) },
                { "[6 7 8]", new Array(new Integer(6), new Integer(7), new Integer(8)) },
            };
    
        [Theory, MemberData(nameof(VectorsTestData))]
        public void Vectors(string code, Array expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
        
        public static TheoryData<string, Array> MatricesTestData =
            new()
            {
                { "[[]]", new Array(new Array()) },
                { "[[3] [4 5 6]]", new Array(new Array(new Integer(3)), new Array(new Integer(4), new Integer(5), new Integer(6))) }
            };
    
        [Theory, MemberData(nameof(MatricesTestData))]
        public void Matrices(string code, Array expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }
}
