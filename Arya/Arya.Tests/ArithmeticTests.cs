namespace Arya.Tests;

public class ArithmeticTests
{
    [Theory, MemberData(nameof(AdditionTestData))]
    public void Addition(string code, Array expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
    
    public static TheoryData<string, Array> AdditionTestData() =>
        new()
        {
            { "1 + 2", new Array([3], []) },
            { "88 + 123", new Array([211], []) }
        };
    
    [Theory, MemberData(nameof(MultiplicationTestData))]
    public void Multiplication(string code, Array expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
    
    public static TheoryData<string, Array> MultiplicationTestData() =>
        new()
        {
            { "2 * 3", new Array([6], []) },
            { "77 * 123", new Array([9471], []) }
        };
}