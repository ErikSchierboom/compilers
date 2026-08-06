namespace Arya.Tests;

public class LiteralsTests
{
    public class Numbers
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "5", new IntArray(Shape.Scalar, [5]) },
                { "123", new IntArray(Shape.Scalar, [123]) }
            };
    
        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }
    
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

    public class Arrays
    {
        public static readonly TheoryData<string, Array> VectorsTestData =
            new()
            {
                { "[]", new EmptyArray(Shape.Scalar) },
                { "[3]", new IntArray(new Shape([1]), [3]) },
                { "[6 7 8]", new IntArray(new Shape([3]), [6, 7, 8]) },
            };
    
        [Theory, MemberData(nameof(VectorsTestData))]
        public void Vectors(string code, Array expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
        
        public static readonly TheoryData<string, Array> MatricesTestData =
            new()
            {
                { "[[]]", new EmptyArray(new Shape([1])) },
                { "[[3 4] [5 6]]", new IntArray(new Shape([2, 2]), [3, 4, 5, 6]) }
            };
    
        [Theory, MemberData(nameof(MatricesTestData))]
        public void Matrices(string code, Array expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }
}
