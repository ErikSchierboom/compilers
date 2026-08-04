namespace Arya.Tests;

public class ArithmeticTests
{
    public static TheoryData<string, Value> AdditionTestData() =>
        new()
        {
            { "1 + 2", new Integer(3) },
            { "1 + []", new Array() },
            { "1 + [2 3 4]", new Array(new Integer(3), new Integer(4), new Integer(5)) },
            { "[2 3] + [4 5]", new Array(new Integer(6), new Integer(8)) },
            { "[[5] [6 7]] + 2", new Array(new Array(new Integer(7)), new Array(new Integer(8), new Integer(9))) }
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