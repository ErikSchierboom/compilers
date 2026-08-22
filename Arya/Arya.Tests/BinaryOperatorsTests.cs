namespace Arya.Tests;

public class BinaryOperatorsTests
{
    public class Addition
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "1 + 2", Array<int>.Scalar(3) },
                { "1 + [2 3 4]", Array<int>.Vector(3, 4, 5) },
                { "[2 3] + [4 5]", Array<int>.Vector(6, 8) },
                { "[[5 4] [6 7]] + 2", Array<int>.Matrix([[7, 6], [8, 9]]) },
                { "[[1 2] [3 4]] + [[5 6] [7 8]]", Array<int>.Matrix([[6, 8], [10, 12]]) },
                { "[] + 2", Array<Any>.Empty },
                { "[|[1]| |[2 3]|] + 10", Array<Box>.Vector(Array<int>.Vector(11).Box(), Array<int>.Vector(12, 13).Box()) },
                { "10 + [|[1]| |[2 3]|]", Array<Box>.Vector(Array<int>.Vector(11).Box(), Array<int>.Vector(12, 13).Box()) },
                { "[|[1]| |[2 3]|] + [|[10]| |[20 30]|]", Array<Box>.Vector(Array<int>.Vector(11).Box(), Array<int>.Vector(22, 33).Box()) },
                { "[|[1]| |[2 3]|] + []", Array<Any>.Empty },
                { "[] + [|[1]| |[2 3]|]", Array<Any>.Empty },
            };

        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        public static readonly TheoryData<string, Value> CharsTestData =
            new()
            {
                 { "'a' + 0", Array<char>.Scalar('a') },
                 { "'a' + 1", Array<char>.Scalar('b') },
                 { "['d' 'g' 'i'] + 11", Array<char>.Vector('o', 'r', 't') },
                 { "[['e'] ['k'] ['g']] + 1", Array<char>.Matrix([['f'], ['l'], ['h']]) },
                 { "[|['a' 'b' 'c']| |['d' 'e']|] + 1", Array<Box>.Vector(Array<char>.Vector('b', 'c', 'd').Box(), Array<char>.Vector('e', 'f').Box()) },
            };

        [Theory, MemberData(nameof(CharsTestData))]
        public void Chars(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        public static readonly TheoryData<string, Value> StringsTestData =
            new()
            {
                 { """
                   "abc" + 0
                   """, Array<char>.Vector([.."abc"]) },
                 { """
                   "abc" + 1
                   """, Array<char>.Vector([.."bcd"]) },
                 { """
                   "" + 2
                   """, Array<char>.Vector([..""]) },
                 { """
                   ["efg" "klm"] + 2
                   """, Array<char>.Matrix([[.."ghi"], [.."mno"]]) },
                 { """
                   [|"abc"| |"de"|] + 1
                   """, Array<Box>.Vector(Array<char>.Vector([.."bcd"]).Box(), Array<char>.Vector([.."ef"]).Box()) },
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
                { "5 - 2", Array<int>.Scalar(3) },
                { "1 - 2", Array<int>.Scalar(-1) },
                { "10 - [2 3 4]", Array<int>.Vector(8, 7, 6) },
                { "[10 20] - [4 5]", Array<int>.Vector(6, 15) },
                { "[[5 4] [6 7]] - 2", Array<int>.Matrix([[3, 2], [4, 5]]) },
                { "[[1 2] [3 4]] - [[5 6] [7 8]]", Array<int>.Matrix([[-4, -4], [-4, -4]]) },
                { "[] - 1", Array<Any>.Empty },
                { "|13| - 3", Array<Box>.Scalar(Array<int>.Scalar(10).Box()) },
                { "|[1 2 3]| - 10", Array<Box>.Scalar(Array<int>.Vector(-9, -8, -7).Box()) },
                { "10 - [|[1]| |[2 3]|]", Array<Box>.Vector(Array<int>.Vector(9).Box(), Array<int>.Vector(8, 7).Box()) },
                { "[|[10]| |[20 30]|] - [|[1]| |[2 3]|]", Array<Box>.Vector(Array<int>.Vector(9).Box(), Array<int>.Vector(18, 27).Box()) },
            };

        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        public static readonly TheoryData<string, Value> CharsTestData =
            new()
            {
                 { "'a' - 0", Array<char>.Scalar('a') },
                 { "'z' - 2", Array<char>.Scalar('x') },
                 { "['p' 'o' 'y'] - 11", Array<char>.Vector('e', 'd', 'n') },
                 { "[['e' 'f' 'g'] ['k' 'l' 'm']] - 2", Array<char>.Matrix([[.."cde"], [.."ijk"]]) },
                 { "[['e'] ['k'] ['g']] - 1", Array<char>.Matrix([[.."d"], [.."j"], [.."f"]]) },
            };

        [Theory, MemberData(nameof(CharsTestData))]
        public void Chars(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        public static readonly TheoryData<string, Value> StringsTestData =
            new()
            {
                 { "\"abc\" - 1", Array<char>.Vector([.."`ab"]) },
                 { "1 - \"abc\"", Array<char>.Vector([unchecked((char)(1 - 'a')), unchecked((char)(1 - 'b')), unchecked((char)(1 - 'c'))]) },
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
                { "2 * 3", Array<int>.Scalar(6) },
                { "2 * [3 4 5]", Array<int>.Vector(6, 8, 10) },
                { "[2 3] * [4 5]", Array<int>.Vector(8, 15) },
                { "[[5 4] [6 7]] * 2", Array<int>.Matrix([[10, 8], [12, 14]]) },
                { "[[1 2] [3 4]] * [[5 6] [7 8]]", Array<int>.Matrix([[5, 12], [21, 32]]) },
                { "[] * 1", Array<Any>.Empty },
                { "5 * |6|", Array<Box>.Scalar(Array<int>.Scalar(30).Box()) },
                { "[|[1]| |[2 3]|] * 10", Array<Box>.Vector(Array<int>.Vector(10).Box(), Array<int>.Vector(20, 30).Box()) },
                { "[|[1]| |[2 3]|] * [|[10]| |[20 30]|]", Array<Box>.Vector(Array<int>.Vector(10).Box(), Array<int>.Vector(40, 90).Box()) },
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
                { "6 / 2", Array<int>.Scalar(3) },
                { "10 / [2 5]", Array<int>.Vector(5, 2) },
                { "[20 30] / [4 5]", Array<int>.Vector(5, 6) },
                { "[[6 4] [8 2]] / 2", Array<int>.Matrix([[3, 2], [4, 1]]) },
                { "[[10 9] [3 4]] / [[2 3] [3 2]]", Array<int>.Matrix([[5, 3], [1, 2]]) },
                { "[] / 1", Array<Any>.Empty },
                { "50 / |5|", Array<Box>.Scalar(Array<int>.Scalar(10).Box()) },
                { "[|[10]| |[20 30]|] / 10", Array<Box>.Vector(Array<int>.Vector(1).Box(), Array<int>.Vector(2, 3).Box()) },
                { "[|[10]| |[20 30]|] / [|[2]| |[4 5]|]", Array<Box>.Vector(Array<int>.Vector(5).Box(), Array<int>.Vector(5, 6).Box()) },
            };

        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }

    public class And
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "5 & 3", Array<int>.Scalar(1) },
                { "2 & [1 3 5]", Array<int>.Vector(0, 2, 0) },
                { "[1 3 5] & 2", Array<int>.Vector(0, 2, 0) },
                { "[9 3] & [7 5]", Array<int>.Vector(1, 1) },
                { "[[5 4] [6 7]] & 2", Array<int>.Matrix([[0, 0], [2, 2]]) },
                { "[[1 2] [3 4]] & [[5 6] [7 8]]", Array<int>.Matrix([[1, 2], [3, 0]]) },
                { "[] & 1", Array<Any>.Empty },
                { "[|[7]| |[9 4]|] & 4", Array<Box>.Vector(Array<int>.Vector(4).Box(), Array<int>.Vector(0, 4).Box()) },
                { "[|[6]| |[4 3]|] & [|[4]| |[2 2]|]", Array<Box>.Vector(Array<int>.Vector(4).Box(), Array<int>.Vector(0, 2).Box()) },
            };

        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }

    public class Or
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "5 | 3", Array<int>.Scalar(7) },
                { "2 | [1 3 5]", Array<int>.Vector(3, 3, 7) },
                { "[1 3 5] | 2", Array<int>.Vector(3, 3, 7) },
                { "[9 3] | [7 5]", Array<int>.Vector(15, 7) },
                { "[[5 4] [6 7]] | 2", Array<int>.Matrix([[7, 6], [6, 7]]) },
                { "[[1 2] [3 4]] | [[5 6] [7 8]]", Array<int>.Matrix([[5, 6], [7, 12]]) },
                { "[] | 1", Array<Any>.Empty },
                { "[|[7]| |[9 4]|] | 4", Array<Box>.Vector(Array<int>.Vector(7).Box(), Array<int>.Vector(13, 4).Box()) },
                { "[|[6]| |[4 3]|] | [|[4]| |[2 2]|]", Array<Box>.Vector(Array<int>.Vector(6).Box(), Array<int>.Vector(6, 3).Box()) },
            };

        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }

    public class ShiftLeft
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "1 << 3", Array<int>.Scalar(8) },
                { "2 << [1 3 5]", Array<int>.Vector(4, 16, 64) },
                { "[1 3 5] << 2", Array<int>.Vector(4, 12, 20) },
                { "[9 3] << [7 5]", Array<int>.Vector(1152, 96) },
                { "[[5 4] [6 7]] << 2", Array<int>.Matrix([[20, 16], [24, 28]]) },
                { "[[1 2] [3 4]] << [[5 6] [7 8]]", Array<int>.Matrix([[32, 128], [384, 1024]]) },
                { "[] << 1", Array<Any>.Empty },
                { "[|[7]| |[9 4]|] << 4", Array<Box>.Vector(Array<int>.Vector(112).Box(), Array<int>.Vector(144, 64).Box()) },
                { "[|[6]| |[4 3]|] << [|[4]| |[2 2]|]", Array<Box>.Vector(Array<int>.Vector(96).Box(), Array<int>.Vector(16, 12).Box()) },
            };

        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }

    public class ShiftRight
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "8 >> 2", Array<int>.Scalar(2) },
                { "120 >> [1 3 5]", Array<int>.Vector(60, 15, 3) },
                { "[11 13 15] >> 2", Array<int>.Vector(2, 3, 3) },
                { "[9 3] >> [2 1]", Array<int>.Vector(2, 1) },
                { "[[5 4] [6 7]] >> 2", Array<int>.Matrix([[1, 1], [1, 1]]) },
                { "[[1 2] [3 4]] >> [[5 6] [7 8]]", Array<int>.Matrix([[0, 0], [0, 0]]) },
                { "[] >> 1", Array<Any>.Empty },
                { "[|[7]| |[9 4]|] >> 1", Array<Box>.Vector(Array<int>.Vector(3).Box(), Array<int>.Vector(4, 2).Box()) },
                { "[|[6]| |[10 3]|] >> [|[1]| |[2 2]|]", Array<Box>.Vector(Array<int>.Vector(3).Box(), Array<int>.Vector(2, 0).Box()) },
            };

        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }

    public class Modulo
    {
        public static readonly TheoryData<string, Value> IntegersTestData =
            new()
            {
                { "5 % 3", Array<int>.Scalar(2) },
                { "2 % [1 3 5]", Array<int>.Vector(0, 2, 2) },
                { "[1 3 5] % 2", Array<int>.Vector(1, 1, 1) },
                { "[9 3] % [7 5]", Array<int>.Vector(2, 3) },
                { "[[5 4] [6 7]] % 2", Array<int>.Matrix([[1, 0], [0, 1]]) },
                { "[[1 2] [3 4]] % [[5 6] [7 8]]", Array<int>.Matrix([[1, 2], [3, 4]]) },
                { "[] % 1", Array<Any>.Empty },
                { "[|[10]| |[21 32]|] % 10", Array<Box>.Vector(Array<int>.Vector(0).Box(), Array<int>.Vector(1, 2).Box()) },
                { "[|[10]| |[21 32]|] % [|[3]| |[5 7]|]", Array<Box>.Vector(Array<int>.Vector(1).Box(), Array<int>.Vector(1, 4).Box()) },
            };

        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }

    public class Append
    {
        public static readonly TheoryData<string, Value> CharsTestData =
            new()
            {
                 { "'a' ++ 'b'", Array<char>.Vector([.."ab"]) },
                 { """
                   "abc" ++ 'd'
                   """, Array<char>.Vector([.."abcd"]) },
                 { """
                   "" ++ ""
                   """, Array<char>.Vector([..""]) },
                 { """
                   "" ++ "hi"
                   """, Array<char>.Vector([.."hi"]) },
                 { """
                   |"hel"| ++ |"lo"|
                   """, Array<Box>.Scalar(Array<char>.Vector([.."hello"]).Box()) }
            };

        [Theory, MemberData(nameof(CharsTestData))]
        public void Chars(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        public static readonly TheoryData<string, Value> ArraysTestData =
            new()
            {
                 { "[] ++ []", Array<Any>.Empty },
                 { "1 ++ []", Array<int>.Vector(1) },
                 { "[] ++ [1 2]", Array<int>.Vector(1, 2) },
                 { "[1] ++ [2 3]", Array<int>.Vector(1, 2, 3) },
                 { "[5 6] ++ 4", Array<int>.Vector(5, 6, 4) },
                 { "[] ++ [7 9]", Array<int>.Vector(7, 9) },
                 { "[[1 2] [3 4]] ++ [[5 6] [7 8]]", Array<int>.Matrix([[1, 2, 5, 6], [3, 4, 7, 8]]) },
            };

        [Theory, MemberData(nameof(ArraysTestData))]
        public void Arrays(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }

    public static readonly TheoryData<string, Value> OperatorPrecedenceTestData =
        new()
        {
             { "2 + 3 * 4", Array<int>.Scalar(14) },
             { "(2 + 3) * 4", Array<int>.Scalar(20) },
        };

    [Theory, MemberData(nameof(OperatorPrecedenceTestData))]
    public void OperatorPrecedence(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
}
