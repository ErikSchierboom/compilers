namespace Gleamy.Tests;

public sealed class FunctionsTests
{
    public sealed class Arguments
    {
        [Fact]
        public void None()
        {
            const string code =
                """
                fn yes() -> Bool {
                    true
                }

                yes()
                """;
            Assert.Equal(true, Interpreter.Evaluate(code));
        }
        
        [Fact]
        public void Single()
        {
            const string code =
                """
                fn double(x: Int) -> Int {
                    x * x
                }

                double(2)
                """;
            Assert.Equal(4, Interpreter.Evaluate(code));
        }
        
        [Fact]
        public void Multiple()
        {
            const string code =
                """
                fn add(x: Int, y: Int, z: Int) -> Int {
                    x + y + z
                }

                add(2, 3, 4)
                """;
            Assert.Equal(9, Interpreter.Evaluate(code));
        }
    }
    
    
}