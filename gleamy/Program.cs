using Gleamy;

const string code = """
                    1 & 2
                    """;

var result = Interpreter.Evaluate(code);
Console.WriteLine(result);
