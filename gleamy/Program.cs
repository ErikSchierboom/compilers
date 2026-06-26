using Gleamy;

const string code = """
                    pub fn cube(x: Int) -> Int {
                        let y = x * x
                        y * x
                    }
                    """;

var tokens = new Scanner(code).Scan();
foreach (var token in tokens)
{
    Console.WriteLine(token);
}

