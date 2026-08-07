namespace Arya.Tests;

public class LiteralsTests
{
    [Fact]
    public void EmptyVector() =>
        Assert.Equal(Empty.Instance, Interpreter.Evaluate("[]"));
    
    public class Scalars
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "5", Int.Scalar(5) },
                { "123", Int.Scalar(123) }
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
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "[3]", Int.Vector(3) },
                { "[6 7 8]", Int.Vector(6, 7, 8) },
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
                { "[[3 4] [5 6]]", new Int(Shape.Matrix(2, 2), 3, 4, 5, 6) },
                { "[[7] [8] [9]]", new Int(Shape.Matrix(3, 1), 7, 8, 9) }
            };

        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }
}
