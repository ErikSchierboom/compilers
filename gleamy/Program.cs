using Gleamy;

const string code = """
                    fn cube (x: Int) -> Int {
                        let y = x * x;
                        y * x
                    }
                    """;

var tokens = new Scanner(code).Scan();
var tree = new Parser(tokens).Parse();

foreach (var statement in tree.Statements)
{
    Console.WriteLine(statement);
}


