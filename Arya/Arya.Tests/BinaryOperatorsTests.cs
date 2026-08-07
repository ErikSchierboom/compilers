namespace Arya.Tests;

public class BinaryOperatorsTests
{
    public class Addition
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "1 + []", EmptyArray.Instance },
                { "[] + 2", EmptyArray.Instance },
                { "1 + 2", IntArray.Scalar(3) },
                { "1 + [2 3 4]", IntArray.Vector(3, 4, 5) },
                { "[2 3] + [4 5]", IntArray.Vector(6, 8) },
                { "[[5 4] [6 7]] + 2", IntArray.Matrix([[7, 6], [8, 9]]) }
            };

         [Theory, MemberData(nameof(IntegersTestData))]
         public void Integers(string code, Value expected) =>
             Assert.Equal(expected, Interpreter.Evaluate(code));
//         
//         public static readonly TheoryData<string, Value> StringsTestData =
//             new()
//             {
//                 { """
//                   "abc" + 1
//                   """, new String("bcd") },
//                 { """
//                   3 + "efg"
//                   """, new String("hij") },
//                 // TODO: allow for nested array
//                 // { "1 + []", new Array() },
//                 // { "1 + [2 3 4]", new Array(3, 4, 5) },
//                 // { "[2 3] + [4 5]", new Array(6, 8) },
//                 // { "[[5] [6 7]] + 2", new Array(new Array(7), new Array(8, 9)) }
//             };
//
//         [Theory, MemberData(nameof(StringsTestData))]
//         public void Strings(string code, Value expected) =>
//             Assert.Equal(expected, Interpreter.Evaluate(code));
     }
//
//     public class Subtraction
//     {
//         public static readonly TheoryData<string, Value> IntegersTestData =
//             new()
//             {
//                 { "2 - 1", 1 },
//                 { "1 - []", new Array() },
//                 { "5 - [2 3 4]", new Array(3, 2, 1) },
//                 { "[7 9] - [2 6]", new Array(5, 3) },
//                 { "[[5] [6 7]] - 2", new Array(new Array(3), new Array(4, 5)) }
//             };
//
//         [Theory, MemberData(nameof(IntegersTestData))]
//         public void Integers(string code, Value expected) =>
//             Assert.Equal(expected, Interpreter.Evaluate(code));
//     }
//
//     public static readonly TheoryData<string, Value> MultiplicationTestData =
//         new()
//         {
//             { "2 * 3", 6 }
//         };
//     
//     [Theory, MemberData(nameof(MultiplicationTestData))]
//     public void Multiplication(string code, Value expected) =>
//         Assert.Equal(expected, Interpreter.Evaluate(code));
//     
//     public static readonly TheoryData<string, Value> OperatorPrecedenceTestData =
//         new()
//         {
//             { "2 + 3 * 4", 14 },
//             { "(2 + 3) * 4", 20 }
//         };
//
//     public class Append
//     {
//         public static readonly TheoryData<string, Value> StringTestData =
//             new()
//             {
//                 { """
//                   "" ++ ""
//                   """, new String("") },
//                 { """
//                   "" ++ "hi"
//                   """, new String("hi") },
//                 { """
//                   "hel" ++ "lo"
//                   """, new String("hello") }
//             };
//
//         [Theory, MemberData(nameof(StringTestData))]
//         public void Strings(string code, Value expected) =>
//             Assert.Equal(expected, Interpreter.Evaluate(code));
//         
//         public static readonly TheoryData<string, Value> ArraysTestData =
//             new()
//             {
//                 { "[] ++ []", new Array() },
//                 { "[1] ++ []", new Array(1) },
//                 { "[] ++ [1 2]", new Array(1, 2) },
//                 { "[1] ++ [2 3]", new Array(1, 2, 3) },
//                 { "4 ++ [5 6]", new Array(4, 5, 6) },
//                 { "[] ++ [7 9]", new Array(7, 9) },
//             };
//
//         [Theory, MemberData(nameof(ArraysTestData))]
//         public void Arrays(string code, Value expected) =>
//             Assert.Equal(expected, Interpreter.Evaluate(code));
//     }
//     
//     [Theory, MemberData(nameof(OperatorPrecedenceTestData))]
//     public void OperatorPrecedence(string code, Value expected) =>
//         Assert.Equal(expected, Interpreter.Evaluate(code));
}