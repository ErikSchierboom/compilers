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
            { "abs([|[-1]| |[-2 -3]|])", Array<Box>.Vector(Array<int>.Vector(1).Box(), Array<int>.Vector(2, 3).Box()) },
            { "abs([|[1]| |[-2 3]|])", Array<Box>.Vector(Array<int>.Vector(1).Box(), Array<int>.Vector(2, 3).Box()) },
        };

    [Theory, MemberData(nameof(AbsTestData))]
    public void Abs(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));

    public static readonly TheoryData<string, Value> LowercaseTestData =
        new()
        {
            { "lowercase('Y')", Array<char>.Scalar('y') },
            { "lowercase('d')", Array<char>.Scalar('d') },
            { "lowercase('1')", Array<char>.Scalar('1') },
            { "lowercase(['A' 'e' 'K'])", Array<char>.Vector('a', 'e', 'k') },
            { "lowercase([|['A']| |['e' 'x']|])", Array<Box>.Vector(Array<char>.Vector('a').Box(), Array<char>.Vector('e', 'x').Box()) },
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
            lowercase([|"THIS"| |"Is"| |"CooL"|])
            """, Array<Box>.Vector(Array<char>.Vector([.."this"]).Box(), Array<char>.Vector([.."is"]).Box(), Array<char>.Vector([.."cool"]).Box()) },
        };

    [Theory, MemberData(nameof(LowercaseTestData))]
    public void Lowercase(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));

    public static readonly TheoryData<string, Value> UppercaseTestData =
        new()
        {
            { "uppercase('Y')", Array<char>.Scalar('Y') },
            { "uppercase('d')", Array<char>.Scalar('D') },
            { "uppercase('1')", Array<char>.Scalar('1') },
            { "uppercase(['A' 'e' 'K'])", Array<char>.Vector('A', 'E', 'K') },
            { "uppercase([|['A']| |['e' 'x']|])", Array<Box>.Vector(Array<char>.Vector('A').Box(), Array<char>.Vector('E', 'X').Box()) },
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
            uppercase([|"THIS"| |"Is"| |"CooL"|])
            """, Array<Box>.Vector(Array<char>.Vector([.."THIS"]).Box(), Array<char>.Vector([.."IS"]).Box(), Array<char>.Vector([.."COOL"]).Box()) },
        };

    [Theory, MemberData(nameof(UppercaseTestData))]
    public void Uppercase(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));

    public static readonly TheoryData<string, Value> TrimTestData =
        new()
        {
            { "trim(' ')", Array<char>.Scalar(' ') },
            { "trim('a')", Array<char>.Scalar('a') },
            { "trim('1')", Array<char>.Scalar('1') },
            { "trim(['A' 'e' 'K' ' '])", Array<char>.Vector('A', 'e', 'K') },
            { """
              trim([|[' ' 'A']| |['\r' 'e' ' ']|])
              """, Array<Box>.Vector(Array<char>.Vector('A').Box(), Array<char>.Vector('e').Box()) },
            { """
              trim("")
              """, Array<char>.Vector([..""]) },
            { """
              trim(" Hi there! ")
              """, Array<char>.Vector([.."Hi there!"]) },
            { """
              trim("123\r\n")
              """, Array<char>.Vector([.."123"]) },
            { """
              trim([|"Th\tis"| |"\tIs"| |"\tCool\t \t\r"|])
              """, Array<Box>.Vector(Array<char>.Vector([.."Th\tis"]).Box(), Array<char>.Vector([.."Is"]).Box(), Array<char>.Vector([.."Cool"]).Box()) },
            { """
              trim([|"Ab"| |" c"|])
              """, Array<Box>.Vector(Array<char>.Vector([.."Ab"]).Box(), Array<char>.Vector([.."c"]).Box()) },
        };

    [Theory, MemberData(nameof(TrimTestData))]
    public void Trim(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));

    public static readonly TheoryData<string, Value> LengthTestData =
        new()
        {
            { "length(2)", Array<int>.Scalar(1) },
            { "length([])", Array<int>.Scalar(0) },
            { "length([-1 5])", Array<int>.Scalar(2) },
            { "length([[-4 -5] [-6 -7] [3 3]])", Array<int>.Scalar(3) },
            { "length([|[-1]| |[-2 -3]|])", Array<int>.Scalar(2) },
        };

    [Theory, MemberData(nameof(LengthTestData))]
    public void Length(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));

    public static readonly TheoryData<string, Value> CountTestData =
        new()
        {
            { "count(2)", Array<int>.Scalar(1) },
            { "count([])", Array<int>.Scalar(0) },
            { "count([-1 5])", Array<int>.Scalar(2) },
            { "count([[-4 -5] [-6 -7]])", Array<int>.Scalar(4) },
            { "count([|[-1]| |[-2 -3]|])", Array<int>.Scalar(2) },
        };

    [Theory, MemberData(nameof(CountTestData))]
    public void Count(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));

    public static readonly TheoryData<string, Value> TransposeTestData =
        new()
        {
            { "transpose(2)", Array<int>.Scalar(2) },
            { "transpose([])", Array<Any>.Vector() },
            { "transpose([-1 5])", Array<int>.Vector(-1, 5) },
            { "transpose([[-4 -5 -6] [-6 -7 -8]])", Array<int>.Matrix([[-4, -6], [-5, -7], [-6, -8]]) },
            { "transpose([|[-1]| |[-2 -3]|])", Array<Box>.Vector(Array<int>.Vector(-1).Box(), Array<int>.Vector(-2, -3).Box()) },
        };

    [Theory, MemberData(nameof(TransposeTestData))]
    public void Transpose(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
}
