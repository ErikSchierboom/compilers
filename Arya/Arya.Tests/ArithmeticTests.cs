namespace Arya.Tests;

public class ArithmeticTests
{
    public static TheoryData<string, Value> AdditionTestData() =>
        new()
        {
            { "1 + 2", new Integer(3) }
        };
    
    [Theory, MemberData(nameof(AdditionTestData))]
    public void Addition(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
    
    public static TheoryData<string, Value> MultiplicationTestData() =>
        new()
        {
            { "2 * 3", new Integer(6) }
        };
    
    [Theory, MemberData(nameof(MultiplicationTestData))]
    public void Multiplication(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
    
    public static TheoryData<string, Value> OperatorPrecedenceTestData() =>
        new()
        {
            { "2 + 3 * 4", new Integer(14) },
            { "(2 + 3) * 4", new Integer(20) }
        };
    
    [Theory, MemberData(nameof(OperatorPrecedenceTestData))]
    public void OperatorPrecedence(string code, Value expected) =>
        Assert.Equal(expected, Interpreter.Evaluate(code));
}