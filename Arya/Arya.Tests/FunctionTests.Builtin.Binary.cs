namespace Arya.Tests;

public static partial class FunctionTests
{
    public static partial class Builtin
    {
        public class Binary
        {
            public static readonly TheoryData<string, Value> ReshapeTestData =
                new()
                {
                    { "reshape([], [1])", Array<Any>.Vector() },
                    { "reshape([1 2 4 5 6 7], [2 3])", Array<int>.Matrix([[1, 2, 4], [5, 6, 7]]) },
                    { "reshape([[-4 -5 -6 -7] [-6 -7 -8 -9]], [2 2 2])", new Array<int>(new Shape(2, 2, 2),-4, -5, -6, -7, -6, -7, -8, -9) },
                    { "reshape([[11 13] [15 17]], [4])", Array<int>.Vector(11, 13, 15, 17) },
                };

            [Theory, MemberData(nameof(ReshapeTestData))]
            public void Reshape(string code, Value expected) =>
                Assert.Equal(expected, Interpreter.Evaluate(code));

            public static readonly TheoryData<string, Value> ReplicateTestData =
                new()
                {
                    { "replicate([], [])", Array<Any>.Empty },
                    { "replicate(1, 1)", Array<int>.Vector(1) },
                    { "replicate(2, 3)", Array<int>.Vector(2, 2, 2) },
                    { "replicate([1 2 4], [1 0 1])", Array<int>.Vector(1, 4) },
                    { "replicate([1 2 4], [2 1 3])", Array<int>.Vector(1, 1, 2, 4, 4, 4) },
                    { "replicate([1 2 3 4], [1 0])", Array<int>.Vector(1, 3) },
                    { "replicate([[-4 -5] [-6 -7]], [2 1])", Array<int>.Matrix([[-4, -5], [-4, -5], [-6, -7]]) },
                    { "replicate('a', 1)", Array<char>.Vector('a') },
                    { "replicate('e', 4)", Array<char>.Vector('e', 'e', 'e', 'e') },
                    { "replicate(['a' 'c' 'e'], [1 0 1])", Array<char>.Vector('a', 'e') },
                    { "replicate(['a' 'c' 'e'], [2 1 3])", Array<char>.Vector('a', 'a', 'c', 'e', 'e', 'e') },
                    { "replicate(['a' 'b' 'c' 'd'], [1 0])", Array<char>.Vector('a', 'c') },
                    { "replicate([['a' 'b'] ['c' 'd']], [2 1])", Array<char>.Matrix([['a', 'b'], ['a', 'b'], ['c', 'd']]) },
                };

            [Theory, MemberData(nameof(ReplicateTestData))]
            public void Replicate(string code, Value expected) =>
                Assert.Equal(expected, Interpreter.Evaluate(code));

            public static readonly TheoryData<string, Value> MaxTestData =
                new()
                {
                    { "max(1, 2)", Array<int>.Scalar(2) },
                    { "max(3, [2 3 4])", Array<int>.Vector(3, 3, 4) },
                    { "max([2 5], [4 3])", Array<int>.Vector(4, 5) },
                    { "max([[5 4] [6 7]], 6)", Array<int>.Matrix([[6, 6], [6, 7]]) },
                    { "max([[1 5] [3 2]], [[4 1] [2 2]]", Array<int>.Matrix([[4, 5], [3, 2]]) },
                    { "max([], 2)", Array<Any>.Empty },
                    { "max([@[1] @[2 3]], 2)", Array<Box>.Vector(Array<int>.Vector(2).Box(), Array<int>.Vector(2, 3).Box()) },
                    { "max(2, [@[1] @[2 3]])", Array<Box>.Vector(Array<int>.Vector(2).Box(), Array<int>.Vector(2, 3).Box()) },
                    { "max([@[1] @[2 3]], [@[2] @[20 30]])", Array<Box>.Vector(Array<int>.Vector(2).Box(), Array<int>.Vector(20, 30).Box()) },
                    { "max([@[1] @[2 3]], [])", Array<Any>.Empty },
                    { "max([], [@[1] @[2 3]])", Array<Any>.Empty },
                    { "max('a', 'b')", Array<char>.Scalar('b') },
                    { "max('c', 'b')", Array<char>.Scalar('c') },
                    { "max(['d' 'g' 'i'], 'h')", Array<char>.Vector('h', 'h', 'i') },
                    { "max([['e'] ['k'] ['g']], 'f')", Array<char>.Matrix([['f'], ['k'], ['g']]) },
                    { "max([@['a' 'b' 'c'] @['d' 'e']], 'c')", Array<Box>.Vector(Array<char>.Vector('c', 'c', 'c').Box(), Array<char>.Vector('d', 'e').Box()) },
                };

            [Theory, MemberData(nameof(MaxTestData))]
            public void Max(string code, Value expected) =>
                Assert.Equal(expected, Interpreter.Evaluate(code));

            public static readonly TheoryData<string, Value> MinTestData =
                new()
                {
                    { "min(1, 2)", Array<int>.Scalar(1) },
                    { "min(3, [2 3 4])", Array<int>.Vector(2, 3, 3) },
                    { "min([2 5], [4 3])", Array<int>.Vector(2, 3) },
                    { "min([[5 4] [6 7]], 6)", Array<int>.Matrix([[5, 4], [6, 6]]) },
                    { "min([[1 5] [3 2]], [[4 1] [2 2]]", Array<int>.Matrix([[1, 1], [2, 2]]) },
                    { "min([], 2)", Array<Any>.Empty },
                    { "min([@[1] @[2 3]], 2)", Array<Box>.Vector(Array<int>.Vector(1).Box(), Array<int>.Vector(2, 2).Box()) },
                    { "min(2, [@[1] @[2 3]])", Array<Box>.Vector(Array<int>.Vector(1).Box(), Array<int>.Vector(2, 2).Box()) },
                    { "min([@[1] @[2 3]], [@[2] @[20 30]])", Array<Box>.Vector(Array<int>.Vector(1).Box(), Array<int>.Vector(2, 3).Box()) },
                    { "min([@[1] @[2 3]], [])", Array<Any>.Empty },
                    { "min([], [@[1] @[2 3]])", Array<Any>.Empty },
                    { "min('a', 'b')", Array<char>.Scalar('a') },
                    { "min('c', 'b')", Array<char>.Scalar('b') },
                    { "min(['d' 'g' 'i'], 'h')", Array<char>.Vector('d', 'g', 'h') },
                    { "min([['e'] ['k'] ['g']], 'f')", Array<char>.Matrix([['e'], ['f'], ['f']]) },
                    { "min([@['a' 'b' 'c'] @['d' 'e']], 'c')", Array<Box>.Vector(Array<char>.Vector('a', 'b', 'c').Box(), Array<char>.Vector('c', 'c').Box()) },
                };

            [Theory, MemberData(nameof(MinTestData))]
            public void Min(string code, Value expected) =>
                Assert.Equal(expected, Interpreter.Evaluate(code));

            public static readonly TheoryData<string, Value> ReduceTestData =
                new()
                {
                    { "reduce(1, max)", Array<int>.Scalar(1) },
                    { "reduce([3], max)", Array<int>.Scalar(3) },
                    { "reduce([2 3 4], max)", Array<int>.Scalar(4) },
                    { "reduce([2 3 4], { acc elem -> acc * elem })", Array<int>.Scalar(24) },
                    { "reduce([24 2], { acc elem -> acc / elem })", Array<int>.Scalar(12) },
                    { "reduce([[2 5] [4 3]], append)", Array<int>.Vector(2, 5, 4, 3) },
                };

            [Theory, MemberData(nameof(ReduceTestData))]
            public void Reduce(string code, Value expected) =>
                Assert.Equal(expected, Interpreter.Evaluate(code));
        }
    }
}
