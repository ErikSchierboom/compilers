using Gleamy;

const string code = """
                    true || false
                    """;

var tokens = new Scanner(code).Scan();
var tree = new Parser(tokens).Parse();
var result = new Interpreter(tree).Evaluate();

Console.WriteLine(result);
