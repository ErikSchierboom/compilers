using Gleamy;

const string code = """
                    1 | 2
                    """;

var tokens = new Scanner(code).Scan();
var tree = new Parser(tokens).Parse();
var result = new Interpreter(tree).Evaluate();

Console.WriteLine(result);
