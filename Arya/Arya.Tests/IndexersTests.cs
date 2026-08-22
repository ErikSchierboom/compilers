namespace Arya.Tests;

public class IndexersTests
{
    public class Integers
    {
        [Fact]
        public void IndexesStartAtOne() =>
            Assert.Equal(Array<int>.Scalar(7), Interpreter.Evaluate("[7 8 9][1]"));

        public static readonly TheoryData<string, Value> ScalarsTestData =
            new()
            {
                { "4[1]", Array<int>.Scalar(4) },
                { "true[1]", Array<bool>.Scalar(true) },
                { "'a'[1]", Array<char>.Scalar('a') },
                { "[4 5 6][1]", Array<int>.Scalar(4) },
                { "[4 5 6][3]", Array<int>.Scalar(6) },
                { "[4 5 6][-1]", Array<int>.Scalar(6) },
                { "[4 5 6][-2]", Array<int>.Scalar(5) },
                { "[true false true][1]", Array<bool>.Scalar(true) },
                { "[true false true][3]", Array<bool>.Scalar(true) },
                { "[true false true][-1]", Array<bool>.Scalar(true) },
                { "[true false true][-2]", Array<bool>.Scalar(false) },
                { """
                  "abc"[1]
                  """, Array<char>.Scalar('a') },
                { """
                  "abc"[3]
                  """, Array<char>.Scalar('c') },
                { """
                  "abc"[-1]
                  """, Array<char>.Scalar('c') },
                { """
                  "abc"[-2]
                  """, Array<char>.Scalar('b') },
                { "[[4 2] [5 1]][1]", Array<int>.Vector(4, 2) },
                { "[[4 2] [5 1] [6 7]][-2]", Array<int>.Vector(5, 1) },
                { """
                  ["abc" "def"][1]
                  """, Array<char>.Vector([.."abc"]) },
                { """
                  ["ab" "cd" "ef"][-2]
                  """, Array<char>.Vector([.."cd"]) },
            };

        [Theory, MemberData(nameof(ScalarsTestData))]
        public void Scalars(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));

        [Fact]
        public void VectorsCanSelectSameElementMultipleTimes() =>
            Assert.Equal(Array<int>.Vector(4, 4), Interpreter.Evaluate("[4 5 6][[1 1]]"));

        [Fact]
        public void VectorsCanSelectElementsInAnyOrder() =>
            Assert.Equal(Array<int>.Vector(6, 4, 5), Interpreter.Evaluate("[4 5 6][[3 1 2]]"));

        public static readonly TheoryData<string, Value> VectorsTestData =
            new()
            {
                { "4[[1]]", Array<int>.Vector(4) },
                { "true[[1]]", Array<bool>.Vector(true) },
                { "'b'[[1]]", Array<char>.Vector('b') },
                { "[4 5 6][[2]]", Array<int>.Vector(5) },
                { "[4 5 6][[1 3]]", Array<int>.Vector(4, 6) },
                { "[4 5 6][[1 2 3]]", Array<int>.Vector(4, 5, 6) },
                { "[4 5 6][[1 2 -1 3]]", Array<int>.Vector(4, 5, 6, 6) },
                { """
                  "abc"[[2]]
                  """, Array<char>.Vector('b') },
                { """
                  "abc"[[1 3]]
                  """, Array<char>.Vector('a', 'c') },
                { """
                  "abc"[[1 2 3]]
                  """, Array<char>.Vector('a', 'b', 'c') },
                { """
                  "abc"[[1 2 -1 3]]
                  """, Array<char>.Vector('a', 'b', 'c', 'c') },
                { """
                  [[4 2] [5 1] [6 0]][[1 -2]]
                  """, Array<int>.Matrix([[4, 2], [5, 1]]) },
                { """
                  ["ab" "cd" "ef"][[1 -2]]
                  """, Array<char>.Matrix([[.."ab"], [.."cd"]]) },
            };

        [Theory, MemberData(nameof(VectorsTestData))]
        public void Vectors(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }
}
