using Gleamy;

const string code = """
                    fn square (x: Int) -> Int {
                        x * x
                    }
                    abs(-3)
                    """;

var result = Interpreter.Evaluate(code);
Console.WriteLine(result);
