namespace Arya.Tests;

public class LiteralsTests
{
    public class Numbers
    {
        public static TheoryData<string, Value> IntegersTestData() =>
            new()
            {
                { "5", new Integer(5) },
                { "123", new Integer(123) }
            };
    
        [Theory, MemberData(nameof(IntegersTestData))]
        public void Integers(string code, Value expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }

    public class Arrays
    {
        public static TheoryData<string, Array> VectorsTestData() =>
            new()
            {
                { "[]", new Array() },
                { "[3]", new Array(new Integer(3)) },
                { "[6 7 8]", new Array(new Integer(6), new Integer(7), new Integer(8)) },
            };
    
        [Theory, MemberData(nameof(VectorsTestData))]
        public void Vectors(string code, Array expected) =>
            Assert.Equal(expected, Interpreter.Evaluate(code));
    }
}
