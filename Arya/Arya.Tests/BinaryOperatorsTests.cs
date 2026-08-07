namespace Arya.Tests;

public class BinaryOperatorsTests
{
    public class Addition
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "1 + 2", IntArray.Scalar(3) },
                { "1 + [2 3 4]", IntArray.Vector(3, 4, 5) },
                { "[2 3] + [4 5]", IntArray.Vector(6, 8) },
                { "[[5 4] [6 7]] + 2", IntArray.Matrix([[7, 6], [8, 9]]) },
                { "[[1 2] [3 4]] + [[5 6] [7 8]]", IntArray.Matrix([[6, 8], [10, 12]]) },
                { "[] + 2", EmptyArray.Instance },
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
                   ['d' 'g' 'i'] + 11
                   """, CharArray.Vector('o', 'r', 't') },
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
                   "" + 2
                   """, CharArray.Vector("") },
                 { """
                   ["efg" "klm"] + 2
                   """, CharArray.Matrix("ghi", "mno") },
             };

         [Theory, MemberData(nameof(StringsTestData))]
         public void Strings(string code, Value expected) =>
             Assert.Equal(expected, Interpreter.Evaluate(code));
     }

    public class Subtraction
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "5 - 2", IntArray.Scalar(3) },
                { "1 - 2", IntArray.Scalar(-1) },
                { "10 - [2 3 4]", IntArray.Vector(8, 7, 6) },
                { "[10 20] - [4 5]", IntArray.Vector(6, 15) },
                { "[[5 4] [6 7]] - 2", IntArray.Matrix([[3, 2], [4, 5]]) },
                { "[[1 2] [3 4]] - [[5 6] [7 8]]", IntArray.Matrix([[-4, -4], [-4, -4]]) },
                { "[] - 1", EmptyArray.Instance },
            };

         [Theory, MemberData(nameof(IntegersTestData))]
         public void Integers(string code, Value expected) =>
             Assert.Equal(expected, Interpreter.Evaluate(code));
         
         public static readonly TheoryData<string, Value> CharsTestData =
             new()
             {
                 { """
                   'a' - 0
                   """, CharArray.Scalar('a') },
                 { """
                   'z' - 2
                   """, CharArray.Scalar('x') },
                 { """
                   ['p' 'o' 'y'] - 11
                   """, CharArray.Vector('e', 'd', 'n') },
                 { """
                   [['e' 'f' 'g'] ['k' 'l' 'm']] - 2
                   """, CharArray.Matrix("cde", "ijk") },
                 { """
                   [['e'] ['k'] ['g']] - 1
                   """, CharArray.Matrix("d", "j", "f") },
             };

         [Theory, MemberData(nameof(CharsTestData))]
         public void Chars(string code, Value expected) =>
             Assert.Equal(expected, Interpreter.Evaluate(code));

         public static readonly TheoryData<string, Value> StringsTestData =
             new()
             {
                 { "\"abc\" - 1", CharArray.Vector("`ab") },
                 { "1 - \"abc\"", CharArray.Vector([unchecked((char)(1 - 'a')), unchecked((char)(1 - 'b')), unchecked((char)(1 - 'c'))]) },
             };

         [Theory, MemberData(nameof(StringsTestData))]
         public void Strings(string code, Value expected) =>
             Assert.Equal(expected, Interpreter.Evaluate(code));
    }

    public class Multiplication
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "2 * 3", IntArray.Scalar(6) },
                { "2 * [3 4 5]", IntArray.Vector(6, 8, 10) },
                { "[2 3] * [4 5]", IntArray.Vector(8, 15) },
                { "[[5 4] [6 7]] * 2", IntArray.Matrix([[10, 8], [12, 14]]) },
                { "[[1 2] [3 4]] * [[5 6] [7 8]]", IntArray.Matrix([[5, 12], [21, 32]]) },
                { "[] * 1", EmptyArray.Instance },
            };

         [Theory, MemberData(nameof(IntegersTestData))]
         public void Integers(string code, Value expected) =>
             Assert.Equal(expected, Interpreter.Evaluate(code));
    }

    public class Division
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "6 / 2", IntArray.Scalar(3) },
                { "10 / [2 5]", IntArray.Vector(5, 2) },
                { "[20 30] / [4 5]", IntArray.Vector(5, 6) },
                { "[[6 4] [8 2]] / 2", IntArray.Matrix([[3, 2], [4, 1]]) },
                { "[[10 9] [3 4]] / [[2 3] [3 2]]", IntArray.Matrix([[5, 3], [1, 2]]) },
                { "[] / 1", EmptyArray.Instance },
            };

         [Theory, MemberData(nameof(IntegersTestData))]
         public void Integers(string code, Value expected) =>
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