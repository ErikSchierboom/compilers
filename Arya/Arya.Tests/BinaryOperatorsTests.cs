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
                { "[[5 4] [6 7]] + 2", IntArray.Matrix([[7, 6], [8, 9]]) },
                { "[[1 2] [3 4]] + [[5 6] [7 8]]", IntArray.Matrix([[6, 8], [10, 12]]) }
            };

         [Theory, MemberData(nameof(IntegersTestData))]
         public void Integers(string code, Value expected) =>
             Assert.Equal(expected, Interpreter.Evaluate(code));
         
         public static readonly TheoryData<string, Value> CharsTestData =
             new()
             {
                 { """
                   'a' + 0
                   """, CharArray.Scalar('a') },
                 { """
                   'a' + 1
                   """, CharArray.Scalar('b') },
                 { """
                   3 + 'e'
                   """, CharArray.Scalar('h') },
                 { """
                   11 + ['d' 'g' 'i']
                   """, CharArray.Vector('o', 'r', 't') },
                 { """
                   2 + [['e' 'f' 'g'] ['k' 'l' 'm']]
                   """, CharArray.Matrix("ghi", "mno") },
                 { """
                   [['e'] ['k'] ['g']] + 1
                   """, CharArray.Matrix("f", "l", "h") },
             };

         [Theory, MemberData(nameof(CharsTestData))]
         public void Chars(string code, Value expected) =>
             Assert.Equal(expected, Interpreter.Evaluate(code));
         
         public static readonly TheoryData<string, Value> StringsTestData =
             new()
             {
                 { """
                   "abc" + 0
                   """, CharArray.Vector("abc") },
                 { """
                   "abc" + 1
                   """, CharArray.Vector("bcd") },
                 { """
                   3 + "efg"
                   """, CharArray.Vector("hij") },
                 { """
                   "" + 2
                   """, CharArray.Vector("") },
                 { """
                   2 + ["efg" "klm"]
                   """, CharArray.Matrix("ghi", "mno") },
                 { """
                   ["e" "k" "g"] + 1
                   """, CharArray.Matrix("f", "l", "h") },
             };

         [Theory, MemberData(nameof(StringsTestData))]
         public void Strings(string code, Value expected) =>
             Assert.Equal(expected, Interpreter.Evaluate(code));
     }


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