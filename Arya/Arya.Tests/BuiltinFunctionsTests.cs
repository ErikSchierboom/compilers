namespace Arya.Tests;

public class BuiltinFunctionsTests
{
    public static readonly TheoryData<string, Value> AbsTestData =
        new()
        {
            { "abs(2)", Array<int>.Scalar(2) },
            { "abs(-1)", Array<int>.Scalar(1) },
            { "abs([-1 -2 -3])", Array<int>.Vector(1, 2, 3) },
            { "abs([[-4 -5] [-6 -7]])", Array<int>.Matrix([[4, 5], [6, 7]]) },
            { "abs([[-1] [-2 -3]])", Array<Box>.Vector(Array<int>.Vector(1).Box(), Array<int>.Vector(2, 3).Box()) },
            { "abs([[1] [-2 3]])", Array<Box>.Vector(Array<int>.Vector(1).Box(), Array<int>.Vector(2, 3).Box()) },
        };

    [Theory, MemberData(nameof(AbsTestData))]
    public void Abs(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));

    public static readonly TheoryData<string, Value> LowercaseTestData =
        new()
        {
            { """
            lowercase('Y')
            """, Array<char>.Scalar('y') },
            { """
            lowercase('d')
            """, Array<char>.Scalar('d') },
            { """
            lowercase('1')
            """, Array<char>.Scalar('1') },
            { """
            lowercase(['A' 'e' 'K'])
            """, Array<char>.Vector('a', 'e', 'k') },
            { """
            lowercase([['A'] ['e' 'x']])
            """, Array<Box>.Vector(Array<char>.Vector('a').Box(), Array<char>.Vector('e', 'x').Box()) },
            { """
            lowercase("")
            """, Array<char>.Vector([..""]) },
            { """
            lowercase("HI ThErE!")
            """, Array<char>.Vector([.."hi there!"]) },
            { """
            lowercase("123")
            """, Array<char>.Vector([.."123"]) },
            { """
            lowercase(["THIS" "Is" "CooL"])
            """, Array<Box>.Vector(Array<char>.Vector([.."this"]).Box(), Array<char>.Vector([.."is"]).Box(), Array<char>.Vector([.."cool"]).Box()) },
        };

    [Theory, MemberData(nameof(LowercaseTestData))]
    public void Lowercase(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));

    public static readonly TheoryData<string, Value> UppercaseTestData =
        new()
        {
            { """
            uppercase('Y')
            """, Array<char>.Scalar('Y') },
            { """
            uppercase('d')
            """, Array<char>.Scalar('D') },
            { """
            uppercase('1')
            """, Array<char>.Scalar('1') },
            { """
            uppercase(['A' 'e' 'K'])
            """, Array<char>.Vector('A', 'E', 'K') },
            { """
            uppercase([['A'] ['e' 'x']])
            """, Array<Box>.Vector(Array<char>.Vector('A').Box(), Array<char>.Vector('E', 'X').Box()) },
            { """
            uppercase("")
            """, Array<char>.Vector([..""]) },
            { """
            uppercase("HI ThErE!")
            """, Array<char>.Vector([.."HI THERE!"]) },
            { """
            uppercase("123")
            """, Array<char>.Vector([.."123"]) },
            { """
            uppercase(["THIS" "Is" "CooL"])
            """, Array<Box>.Vector(Array<char>.Vector([.."THIS"]).Box(), Array<char>.Vector([.."IS"]).Box(), Array<char>.Vector([.."COOL"]).Box()) },
        };

    [Theory, MemberData(nameof(UppercaseTestData))]
    public void Uppercase(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
}
