using Gleamy;

const string code = """
                    fn inc (x: Int) -> Int {
                        x + 1
                    }
                    abs(inc(-3)) + 2 * 3 - 1
                    """;

var result = Interpreter.Evaluate(code);
Console.WriteLine(result);
