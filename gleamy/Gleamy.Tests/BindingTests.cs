namespace Gleamy.Tests;

public sealed class BindingTests
{
    [Fact]
    public void Binding()
    {
        const string code =
            """
            let x = 1;
            let y = 2;
            x + y
            """;
        
        Assert.Equal(3, Interpreter.Evaluate(code));
    }
}