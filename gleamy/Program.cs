using Gleamy;

const string code = """
                    pub fn factorial(x: Int) -> Int {
                      match x {
                        case 1 => 1,
                        case _ => x * factorial(x - 1),
                      }
                    }
                    """;

var tokens = new Scanner(code).Scan();
foreach (var token in tokens)
{
    Console.WriteLine(token);
}

