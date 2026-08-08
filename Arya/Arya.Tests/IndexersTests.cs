namespace Arya.Tests;

public class IndexersTests
{
    public class Integers
    {
        [Fact]
        public void IndexesStartAtOne() =>
            Assert.Equal(IntArray.Scalar(7), Interpreter.Evaluate("[7 8 9][1]"));
        
        public static readonly TheoryData<string, Value> ScalarsTestData =
            new()
            {
                { "4[1]", IntArray.Scalar(4) },
                { "[4 5 6][1]", IntArray.Scalar(4) },
                { "[4 5 6][3]", IntArray.Scalar(6) },
                { "[[4 2] [5 1]][1]", IntArray.Vector(4, 2) },
            };

        [Theory, MemberData(nameof(ScalarsTestData))]
        public void Scalars(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
        
        [Fact]
        public void VectorsCanSelectSameElementMultipleTimes() =>
            Assert.Equal(IntArray.Vector(4, 4), Interpreter.Evaluate("[4 5 6][[1 1]]"));
        
        [Fact]
        public void VectorsCanSelectElementsInAnyOrder() =>
            Assert.Equal(IntArray.Vector(6, 4, 5), Interpreter.Evaluate("[4 5 6][[3 1 2]]"));
        
        public static readonly TheoryData<string, Value> VectorsTestData =
            new()
            {
                { "[4 5 6][[2]]", IntArray.Vector(5) },
                { "[4 5 6][[1 3]]", IntArray.Vector(4, 6) },
                { "[4 5 6][[1 2 3]]", IntArray.Vector(4, 5, 6) },
                { "[4 5 6][[1 2 1 3]]", IntArray.Vector(4, 5, 4, 6) },
                { "[[4 2] [5 1] [6 0]][[1 3]]", IntArray.Matrix([[4, 2], [6, 0]]) },
            };
        
        [Theory, MemberData(nameof(VectorsTestData))]
        public void Vectors(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }
}
