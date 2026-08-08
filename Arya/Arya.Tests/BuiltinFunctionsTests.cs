namespace Arya.Tests;

public class BuiltinFunctionsTests
{
    public static readonly TheoryData<string, Value> AbsTestData =
        new()
        {
            { "abs(2)", IntArray.Scalar(2) },
            { "abs(-1)", IntArray.Scalar(1) },
            { "abs([-1 -2 -3])", IntArray.Vector(1, 2, 3) },
            { "abs([[-4 -5] [-6 -7]])", IntArray.Matrix([[4, 5], [6, 7]]) }
        };

    [Theory, MemberData(nameof(AbsTestData))]
    public void Abs(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
//     
//     public static readonly TheoryData<string, Value> LowercaseTestData =
//         new()
//         {
//             { """
//               lowercase("")
//               """, new String("") },
//             { """
//               lowercase("HI ThErE!")
//               """, new String("hi there!") },
//             { """
//               lowercase("123")
//               """, new String("123") },
//             { """
//               lowercase(["THIS" "Is" "CooL"])
//               """, new Array(new String("this"), new String("is"), new String("cool")) },
//             { """
//               lowercase([["LET'S"] ["Do"] ["iT"]])
//               """, new Array(new Array(new String("let's")), new Array(new String("do")), new Array(new String("it"))) },
//         };
//
//     [Theory, MemberData(nameof(LowercaseTestData))]
//     public void Lowercase(string code, Value expected) =>
//         Assert.Equal(expected, Interpreter.Evaluate(code));
//     
//     public static readonly TheoryData<string, Value> UppercaseTestData =
//         new()
//         {
//             { """
//               uppercase("")
//               """, new String("") },
//             { """
//               uppercase("hi there!")
//               """, new String("HI THERE!") },
//             { """
//               uppercase("123")
//               """, new String("123") },
//             { """
//               uppercase(["this" "Is" "CooL"])
//               """, new Array(new String("THIS"), new String("IS"), new String("COOL")) },
//             { """
//               uppercase([["let's"] ["Do"] ["iT"]])
//               """, new Array(new Array(new String("LET'S")), new Array(new String("DO")), new Array(new String("IT"))) },
//         };
//
//     [Theory, MemberData(nameof(UppercaseTestData))]
//     public void Uppercase(string code, Value expected) =>
//         Assert.Equal(expected, Interpreter.Evaluate(code));
//     
//     public static readonly TheoryData<string, Value> TrimTestData =
//         new()
//         {
//             { """
//               trim("")
//               """, new String("") },
//             { """
//               trim(" ")
//               """, new String("") },
//             { """
//               trim(" abc ")
//               """, new String("abc") },
//             { """
//               trim(" \t this is nice \r\n ")
//               """, new String("this is nice") },
//             { """
//               trim([" This " "\tis" "\r\ncool"])
//               """, new Array(new String("This"), new String("is"), new String("cool")) },
//             { """
//               trim([["\tLet's  "] [" Do \r"] ["\n\nIt"]])
//               """, new Array(new Array(new String("Let's")), new Array(new String("Do")), new Array(new String("It"))) },
//         };
//
//     [Theory, MemberData(nameof(TrimTestData))]
//     public void Trim(string code, Value expected) =>
//         Assert.Equal(expected, Interpreter.Evaluate(code));
}
