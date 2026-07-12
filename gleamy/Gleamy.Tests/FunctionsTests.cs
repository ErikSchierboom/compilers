namespace Gleamy.Tests;

public sealed class FunctionsTests
{
    public sealed class Evaluation
    {
        [Fact]
        public void LastStatementIsReturned()
        {
            const string code =
                """
                fn answer() -> Int {
                    let x = 40;
                    let y = 2;
                    x + y
                }

                answer()
                """;
            Assert.Equal(42, Interpreter.Evaluate(code));
        }
        
        [Fact]
        public void Recursion()
        {
            const string code =
                """
                fn factorial(i: Int) -> Int {
                    match i {
                        1 => 1,
                        _ => i * factorial(i - 1)
                    }
                }

                factorial(5)
                """;
            Assert.Equal(120, Interpreter.Evaluate(code));
        }
    }
    
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
