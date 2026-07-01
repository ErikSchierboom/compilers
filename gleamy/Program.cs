using Gleamy;

const string code = """
                    fn is_even (x: Int) -> Bool {
                        match x % 2 {
                            0 => true,
                            _ => false
                        }
                    }
                    
                    is_even(6)
                    """;

var tokens = new Scanner(code).Scan();
var tree = new Parser(tokens).Parse();
var result = new Interpreter(tree).Evaluate();

Console.WriteLine(result);
