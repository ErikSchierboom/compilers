using Gleamy;

const string code = """
                    fn cube (x: Int) -> Int {
                        let y = x * x
                        y * x
                    }
                    """;

var tokens = new Scanner(code).Scan();
new Parser(tokens).Parse();

foreach (var token in tokens)
{
    Console.WriteLine(token);
}

internal record Tree(List<Statement> Statements);

internal abstract record Statement;

internal class Parser(List<Token> tokens)
{
    public void Parse()
    {
        throw new NotImplementedException();
    }
}

