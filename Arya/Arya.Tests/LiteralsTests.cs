namespace Arya.Tests;

public class LiteralsTests
{
    public class Integers
    {
        public static readonly TheoryData<string, Value> ScalarsTestData =
            new()
            {
                { "5", Array<int>.Scalar(5) },
                { "123", Array<int>.Scalar(123) }
            };

        [Theory, MemberData(nameof(ScalarsTestData))]
        public void Scalars(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
        
        public static readonly TheoryData<string, Value> VectorsTestData =
            new()
            {
                { "[3]", Array<int>.Vector(3) },
                { "[6 7 8]", Array<int>.Vector(6, 7, 8) },
            };

        [Theory, MemberData(nameof(VectorsTestData))]
        public void Vectors(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
        
        public static readonly TheoryData<string, Value> MatricesTestData =
            new()
            {
                { "[[3 4] [5 6]]", Array<int>.Matrix([[3, 4], [5, 6]]) },
                { "[[7] [8] [9]]", Array<int>.Matrix([[7], [8], [9]]) },
            };

        [Theory, MemberData(nameof(MatricesTestData))]
        public void Matrices(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }

    public class Chars
    {
        public class Scalars
        {
            public static readonly TheoryData<string, Value> UnescapedTestData =
                new()
                {
                    { """
                      ' '
                      """, Array<char>.Scalar(' ') },
                    { """
                      'a'
                      """, Array<char>.Scalar('a') },
                    { """
                      '2'
                      """, Array<char>.Scalar('2') },
                    { """
                      '@'
                      """, Array<char>.Scalar('@') }
                };
    
            [Theory, MemberData(nameof(UnescapedTestData))]
            public void Unescaped(string code, Value expected) =>
                Assert.Equal(expected, Interpreter.Evaluate(code));
        
            public static readonly TheoryData<string, Value> EscapedTestData =
                new()
                {
                    { """
                      '\t'
                      """, Array<char>.Scalar('\t') },
                    { """
                      '\r'
                      """, Array<char>.Scalar('\r') },
                    { """
                      '\n'
                      """, Array<char>.Scalar('\n') },
                    { """
                      '\\'
                      """, Array<char>.Scalar('\\') },
                    { """
                      '\''
                      """, Array<char>.Scalar('\'') }
                };
    
            [Theory, MemberData(nameof(EscapedTestData))]
            public void Escaped(string code, Value expected) =>
                Assert.Equal(expected, Interpreter.Evaluate(code));
        }
        
        public class Vectors
        {
            public static readonly TheoryData<string, Value> UnescapedTestData =
                new()
                {
                    { """
                      ['a']
                      """, Array<char>.Vector('a') },
                    { """
                      ['h' ' ' 'i']
                      """, Array<char>.Vector('h', ' ', 'i') },
                };
    
            [Theory, MemberData(nameof(UnescapedTestData))]
            public void Unescaped(string code, Value expected) =>
                Assert.Equal(expected, Interpreter.Evaluate(code));
        
            public static readonly TheoryData<string, Value> EscapedTestData =
                new()
                {
                    { """
                      ['\\']
                      """, Array<char>.Vector('\\') },
                    { """
                      ['\t' '\r' '\n']
                      """, Array<char>.Vector('\t', '\r', '\n') },
                };
    
            [Theory, MemberData(nameof(EscapedTestData))]
            public void Escaped(string code, Value expected) =>
                Assert.Equal(expected, Interpreter.Evaluate(code));
            
            public class Strings
            {
                public static readonly TheoryData<string, Value> UnescapedTestData =
                    new()
                    {
                        { """
                          ""
                          """, Array<char>.Vector([..""]) },
                        { """
                          "a"
                          """, Array<char>.Vector([.."a"]) },
                        { """
                          "2"
                          """, Array<char>.Vector([.."2"]) },
                        { """
                          "hello there 123!"
                          """, Array<char>.Vector([.."hello there 123!"]) }
                    };
        
                [Theory, MemberData(nameof(UnescapedTestData))]
                public void Unescaped(string code, Value expected) =>
                    Assert.Equal(expected, Interpreter.Evaluate(code));
            
                public static readonly TheoryData<string, Value> EscapedTestData =
                    new()
                    {
                        { """
                          "\t"
                          """, Array<char>.Vector([.."\t"]) },
                        { """
                          "\r"
                          """, Array<char>.Vector([.."\r"]) },
                        { """
                          "\n"
                          """, Array<char>.Vector([.."\n"]) },
                        { """
                          "\\"
                          """, Array<char>.Vector([.."\\"]) },
                        { """
                          "hey\tyou!\r\n"
                          """, Array<char>.Vector([.."hey\tyou!\r\n"]) },
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
                { "[[3] [4 5]]", Array<Box>.Vector(Array<int>.Vector(3).Box(), Array<int>.Vector(4, 5).Box()) },
                { """
                  ["abc" "de" "f"]
                  """, Array<Box>.Vector(Array<char>.Vector([.."abc"]).Box(), Array<char>.Vector([.."de"]).Box(), Array<char>.Vector([.."f"]).Box()) },
            };

        [Theory, MemberData(nameof(BoxedTestData))]
        public void Boxed(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }
}
