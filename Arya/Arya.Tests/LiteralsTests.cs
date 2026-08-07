namespace Arya.Tests;

public class LiteralsTests
{
    public class Integers
    {
        public static readonly TheoryData<string, Value> ScalarsTestData =
            new()
            {
                { "5", IntArray.Scalar(5) },
                { "123", IntArray.Scalar(123) }
            };

        [Theory, MemberData(nameof(ScalarsTestData))]
        public void Scalars(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
        
        public static readonly TheoryData<string, Value> VectorsTestData =
            new()
            {
                { "[3]", IntArray.Vector(3) },
                { "[6 7 8]", IntArray.Vector(6, 7, 8) },
            };

        [Theory, MemberData(nameof(VectorsTestData))]
        public void Vectors(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
        
        public static readonly TheoryData<string, Value> MatricesTestData =
            new()
            {
                { "[[3 4] [5 6]]", IntArray.Matrix([[3, 4], [5, 6]]) },
                { "[[7] [8] [9]]", IntArray.Matrix([[7], [8], [9]]) },
            };

        [Theory, MemberData(nameof(MatricesTestData))]
        public void Matrices(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }

    public class Chars
    {
        public class Vectors
        {
            public class Strings
            {
                public static readonly TheoryData<string, Value> UnescapedTestData =
                    new()
                    {
                        { """
                          ""
                          """, CharArray.Vector("") },
                        { """
                          "a"
                          """, CharArray.Vector("a") },
                        { """
                          "2"
                          """, CharArray.Vector("2") },
                        { """
                          "hello there 123!"
                          """, CharArray.Vector("hello there 123!") }
                    };
        
                [Theory, MemberData(nameof(UnescapedTestData))]
                public void Unescaped(string code, Value expected) =>
                    Assert.Equal(expected, Interpreter.Evaluate(code));
            
                public static readonly TheoryData<string, Value> EscapedTestData =
                    new()
                    {
                        { """
                          "\t"
                          """, CharArray.Vector("\t") },
                        { """
                          "\r"
                          """, CharArray.Vector("\r") },
                        { """
                          "\n"
                          """, CharArray.Vector("\n") },
                        { """
                          "\\"
                          """, CharArray.Vector("\\") },
                        { """
                          "hey\tyou!\r\n"
                          """, CharArray.Vector("hey\tyou!\r\n") },
                    };
        
                [Theory, MemberData(nameof(EscapedTestData))]
                public void Escaped(string code, Value expected) =>
                    Assert.Equal(expected, Interpreter.Evaluate(code));
            }
        }
    }

    public class Vectors
    {
        [Fact]
        public void Empty() =>
            Assert.Equal(EmptyArray.Instance, Interpreter.Evaluate("[]"));
        
        public static readonly TheoryData<string, Value> BoxedTestData =
            new()
            {
                { "[[3] [4 5]]", BoxArray.Vector(IntArray.Vector(3).Box(), IntArray.Vector(4, 5).Box()) },
                { """
                  ["abc" "de" "f"]
                  """, BoxArray.Vector(CharArray.Vector("abc").Box(), CharArray.Vector("de").Box(), CharArray.Vector("f").Box()) },
            };

        [Theory, MemberData(nameof(BoxedTestData))]
        public void Boxed(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }
}
