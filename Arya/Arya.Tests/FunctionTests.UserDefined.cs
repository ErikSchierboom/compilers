namespace Arya.Tests;

public static partial class FunctionTests
{
    public class Lambda
    {
        public class Invoke
        {
            [Fact]
            public void FromVariable()
            {
                const string code =
                    """
                    double = { # * 2 }
                    double(3)
                    """;
                Assert.Equal(Array<int>.Scalar(6), Interpreter.Evaluate(code));
            }

            [Fact]
            public void Inline()
            {
                const string code = "{ # * 2 }(3)";
                Assert.Equal(Array<int>.Scalar(6), Interpreter.Evaluate(code));
            }

            [Fact]
            public void WithoutArguments()
            {
                const string code =
                    """
                    answer = { 42 }
                    answer()
                    """;
                Assert.Equal(Array<int>.Scalar(42), Interpreter.Evaluate(code));
            }
        }

        public class NamedParameters
        {
            [Fact]
            public void WithoutIndexCorrespondToSingleArgument()
            {
                const string code =
                    """
                    double = { x -> x * 2 }
                    double(3)
                    """;
                Assert.Equal(Array<int>.Scalar(6), Interpreter.Evaluate(code));
            }

            public static readonly TheoryData<string, Value> WithIndexTestData =
                new()
                {
                    { "{ x -> x + 1 }(3)", Array<int>.Scalar(4) },
                    { "{ x y -> x + y }(3, 4)", Array<int>.Scalar(7) },
                    { "{ x y z -> x + y * z }(3, 4, 5)", Array<int>.Scalar(23) },
                };

            [Theory, MemberData(nameof(WithIndexTestData))]
            public void WithIndexCorrespondToNthArgument(string code, Value expected) =>
                Assert.Equal(expected, Interpreter.Evaluate(code));
        }

        public class Placeholders
        {
            [Fact]
            public void WithoutIndexCorrespondToSingleArgument()
            {
                const string code =
                    """
                    double = { # * 2 }
                    double(3)
                    """;
                Assert.Equal(Array<int>.Scalar(6), Interpreter.Evaluate(code));
            }

            public static readonly TheoryData<string, Value> WithIndexTestData =
                new()
                {
                    { "{ #1 + 1 }(3)", Array<int>.Scalar(4) },
                    { "{ #1 + #2 }(3, 4)", Array<int>.Scalar(7) },
                    { "{ #1 + #2 * #3 }(3, 4, 5)", Array<int>.Scalar(23) },
                };

            [Theory, MemberData(nameof(WithIndexTestData))]
            public void WithIndexCorrespondToNthArgument(string code, Value expected) =>
                Assert.Equal(expected, Interpreter.Evaluate(code));
        }

        [Fact]
        public void Multiline()
        {
            const string code =
                """
                double = { 
                    a = # * 2
                    b = a + 3
                    b * 4
                }
                double(5)
                """;
            Assert.Equal(Array<int>.Scalar(52), Interpreter.Evaluate(code));
        }
    }
}
