using Gleamy;

const string code = """
                    fn square (x: Int) -> Int {
                        let squared = x * x;
                        squared
                    }
                    
                    square(3)
                    """;

var tokens = new Scanner(code).Scan();
var tree = new Parser(tokens).Parse();
var result = new Interpreter(tree).Evaluate();

Console.WriteLine(result);
