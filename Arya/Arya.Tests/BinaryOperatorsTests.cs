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

         public static readonly TheoryData<string, Value> BoxedIntegersTestData =
             new()
             {
                 { "[[1] [2 3]] + 10", BoxArray.Vector(IntArray.Vector(11).Box(), IntArray.Vector(12, 13).Box()) },
                 { "10 + [[1] [2 3]]", BoxArray.Vector(IntArray.Vector(11).Box(), IntArray.Vector(12, 13).Box()) },
                 { "[[1] [2 3]] + [[10] [20 30]]", BoxArray.Vector(IntArray.Vector(11).Box(), IntArray.Vector(22, 33).Box()) },
                 { "[[1] [2 3]] + []", EmptyArray.Instance },
                 { "[] + [[1] [2 3]]", EmptyArray.Instance },
             };

         [Theory, MemberData(nameof(BoxedIntegersTestData))]
         public void BoxedIntegers(string code, Value expected) =>
             Assert.Equal(expected, Interpreter.Evaluate(code));

         public static readonly TheoryData<string, Value> BoxedStringsTestData =
             new()
             {
                 { """
                   ["abc" "de"] + 1
                   """, BoxArray.Vector(CharArray.Vector("bcd").Box(), CharArray.Vector("ef").Box()) },
             };

         [Theory, MemberData(nameof(BoxedStringsTestData))]
         public void BoxedStrings(string code, Value expected) =>
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

         public static readonly TheoryData<string, Value> BoxedIntegersTestData =
             new()
             {
                 { "[[1] [2 3]] - 10", BoxArray.Vector(IntArray.Vector(-9).Box(), IntArray.Vector(-8, -7).Box()) },
                 { "10 - [[1] [2 3]]", BoxArray.Vector(IntArray.Vector(9).Box(), IntArray.Vector(8, 7).Box()) },
                 { "[[10] [20 30]] - [[1] [2 3]]", BoxArray.Vector(IntArray.Vector(9).Box(), IntArray.Vector(18, 27).Box()) },
             };

         [Theory, MemberData(nameof(BoxedIntegersTestData))]
         public void BoxedIntegers(string code, Value expected) =>
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

         public static readonly TheoryData<string, Value> BoxedIntegersTestData =
             new()
             {
                 { "[[1] [2 3]] * 10", BoxArray.Vector(IntArray.Vector(10).Box(), IntArray.Vector(20, 30).Box()) },
                 { "[[1] [2 3]] * [[10] [20 30]]", BoxArray.Vector(IntArray.Vector(10).Box(), IntArray.Vector(40, 90).Box()) },
             };

         [Theory, MemberData(nameof(BoxedIntegersTestData))]
         public void BoxedIntegers(string code, Value expected) =>
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

         public static readonly TheoryData<string, Value> BoxedIntegersTestData =
             new()
             {
                 { "[[10] [20 30]] / 10", BoxArray.Vector(IntArray.Vector(1).Box(), IntArray.Vector(2, 3).Box()) },
                 { "[[10] [20 30]] / [[2] [4 5]]", BoxArray.Vector(IntArray.Vector(5).Box(), IntArray.Vector(5, 6).Box()) },
             };

         [Theory, MemberData(nameof(BoxedIntegersTestData))]
         public void BoxedIntegers(string code, Value expected) =>
             Assert.Equal(expected, Interpreter.Evaluate(code));
    }

    public class Modulo
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "5 % 3", IntArray.Scalar(2) },
                { "2 % [1 3 5]", IntArray.Vector(0, 2, 2) },
                { "[1 3 5] % 2", IntArray.Vector(1, 1, 1) },
                { "[9 3] % [7 5]", IntArray.Vector(2, 3) },
                { "[[5 4] [6 7]] % 2", IntArray.Matrix([[1, 0], [0, 1]]) },
                { "[[1 2] [3 4]] % [[5 6] [7 8]]", IntArray.Matrix([[1, 2], [3, 4]]) },
                { "[] % 1", EmptyArray.Instance },
            };

        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        public static readonly TheoryData<string, Value> BoxedIntegersTestData =
            new()
            {
                { "[[10] [21 32]] % 10", BoxArray.Vector(IntArray.Vector(0).Box(), IntArray.Vector(1, 2).Box()) },
                { "[[10] [21 32]] % [[3] [5 7]]", BoxArray.Vector(IntArray.Vector(1).Box(), IntArray.Vector(1, 4).Box()) },
            };

        [Theory, MemberData(nameof(BoxedIntegersTestData))]
        public void BoxedIntegers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }

     public class Append
     {
         public static readonly TheoryData<string, Value> CharsTestData =
             new()
             {
                 { """
                   "" ++ ""
                   """, CharArray.Vector("") },
                 { """
                   "" ++ "hi"
                   """, CharArray.Vector("hi") },
                 { """
                   "hel" ++ "lo"
                   """, CharArray.Vector("hello") }
             };

         [Theory, MemberData(nameof(CharsTestData))]
         public void Chars(string code, Value expected) =>
             Assert.Equal(expected, Interpreter.Evaluate(code));
         
         public static readonly TheoryData<string, Value> StringsTestData =
             new()
             {
                 { """
                   'a' ++ 'b'
                   """, CharArray.Vector("ab") },
                 { """
                   "abc" ++ 'd'
                   """, CharArray.Vector("abcd") },
             };

         [Theory, MemberData(nameof(StringsTestData))]
         public void Strings(string code, Value expected) =>
             Assert.Equal(expected, Interpreter.Evaluate(code));
         
         public static readonly TheoryData<string, Value> ArraysTestData =
             new()
             {
                 { "[] ++ []", EmptyArray.Instance },
                 { "1 ++ []", IntArray.Vector(1) },
                 { "[] ++ [1 2]", IntArray.Vector(1, 2) },
                 { "[1] ++ [2 3]", IntArray.Vector(1, 2, 3) },
                 { "[5 6] ++ 4", IntArray.Vector(5, 6, 4) },
                 { "[] ++ [7 9]", IntArray.Vector(7, 9) },
                 { "[[1 2] [3 4]] ++ [[5 6] [7 8]]", IntArray.Matrix([[1, 2, 5, 6], [3, 4, 7, 8]]) },
             };

         [Theory, MemberData(nameof(ArraysTestData))]
         public void Arrays(string code, Value expected) =>
             Assert.Equal(expected, Interpreter.Evaluate(code));
     }

     public static readonly TheoryData<string, Value> OperatorPrecedenceTestData =
         new()
         {
             { "2 + 3 * 4", IntArray.Scalar(14) },
             { "(2 + 3) * 4", IntArray.Scalar(20) },
         };

     [Theory, MemberData(nameof(OperatorPrecedenceTestData))]
     public void OperatorPrecedence(string code, Value expected) =>
         Assert.Equal(expected, Interpreter.Evaluate(code));
}