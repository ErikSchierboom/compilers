namespace Arya.Tests;

public class VariablesTests
{
    public class Assignment
    {
        public static readonly TheoryData<string, Value> LiteralTestData =
            new()
            {
                { "a = 5", Array<int>.Scalar(5) },
                { "b = 'z'", Array<char>.Scalar('z') },
                { "c = true", Array<bool>.Scalar(true) },
                { """
                  d = "abc"
                  """, Array<char>.Vector([.."abc"]) },
                { "e = [1 2 3]", Array<int>.Vector(1, 2, 3) },
                { "f = [true false]", Array<bool>.Vector(true, false) },
                { "g = [[1 2] [3 4]]", Array<int>.Matrix([[1, 2], [3, 4]]) },
                { "h = [@[1 2] @[3]]", Array<Box>.Vector(Array<int>.Vector(1, 2).Box(), Array<int>.Vector(3).Box()) },
            };

        [Theory, MemberData(nameof(LiteralTestData))]
        public void Literal(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Fact]
        public void Overwrite()
        {
            const string code =
                """
                a = 5
                a = 7
                a
                """;
            var expected = Array<int>.Scalar(7);
            Assert.Equal(expected, Interpreter.Evaluate(code));
        }

        [Fact]
        public void Variable()
        {
            const string code =
                """
                a = 5
                b = a
                b
                """;
            var expected = Array<int>.Scalar(5);
            Assert.Equal(expected, Interpreter.Evaluate(code));
        }

        [Fact]
        public void Call()
        {
            const string code =
                """
                a = max(1, 3)
                """;
            var expected = Array<int>.Scalar(3);
            Assert.Equal(expected, Interpreter.Evaluate(code));
        }

        [Fact]
        public void Function()
        {
            const string code =
                """
                a = max
                """;
            var function = Assert.IsType<Function>(Interpreter.Evaluate(code), exactMatch: false);
            Assert.Equal("max", function.Name);
            Assert.Equal(2, function.Arity);
        }
    }
}
