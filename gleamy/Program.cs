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
var program = new Binder(tree).Bind();

Console.WriteLine(program);
// foreach (var statement in tree.program)
// {
//     Console.WriteLine(statement);
// }


