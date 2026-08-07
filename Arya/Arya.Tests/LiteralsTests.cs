namespace Arya.Tests;

public static class LiteralsTests
{
    public class Scalars
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "5", IntArray.Scalar(5) },
                { "123", IntArray.Scalar(123) }
            };

        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        // public class Strings
        // {
        //     public static readonly TheoryData<string, Value> UnescapedTestData =
        //         new()
        //         {
        //             { """
        //               ""
        //               """, new String("") },
        //             { """
        //               "a"
        //               """, new String("a") },
        //             { """
        //               "2"
        //               """, new String("2") },
        //             { """
        //               "hello there 123!"
        //               """, new String("hello there 123!") }
        //         };
        //
        //     [Theory, MemberData(nameof(UnescapedTestData))]
        //     public void Unescaped(string code, Value expected) =>
        //         Assert.Equal(expected, Interpreter.Evaluate(code));
        //     
        //     public static readonly TheoryData<string, Value> EscapedTestData =
        //         new()
        //         {
        //             { """
        //               "\t"
        //               """, new String("\t") },
        //             { """
        //               "\r"
        //               """, new String("\r") },
        //             { """
        //               "\n"
        //               """, new String("\n") },
        //             { """
        //               "\\"
        //               """, new String("\\") },
        //             { """
        //               "hey\tyou!\r\n"
        //               """, new String("hey\tyou!\r\n") },
        //         };
        //
        //     [Theory, MemberData(nameof(EscapedTestData))]
        //     public void Escaped(string code, Value expected) =>
        //         Assert.Equal(expected, Interpreter.Evaluate(code));
        // }
    }

    public class Vectors
    {
        [Fact]
        public void Empty() =>
            Assert.Equal(Arya.EmptyArray.Instance, Interpreter.Evaluate("[]"));
        
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "[3]", IntArray.Vector(3) },
                { "[6 7 8]", IntArray.Vector(6, 7, 8) },
            };

        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }

    public class Matrices
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "[[3 4] [5 6]]", IntArray.Matrix([[3, 4], [5, 6]]) },
                { "[[7] [8] [9]]", IntArray.Matrix([[7], [8], [9]]) },
            };

        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }
}
