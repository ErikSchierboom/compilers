using Gleamy;

const string code = """
                    fn square (x: Int) -> Int {
                        let squared = x * x;
                        squared
                    }
                    
                    fn cube (x: Int) -> Int {
                        let y = square(x);
                        y * x
                    }
                    """;

var tokens = new Scanner(code).Scan();
var tree = new Parser(tokens).Parse();

foreach (var statement in tree.Statements)
{
    Console.WriteLine(statement);
}


