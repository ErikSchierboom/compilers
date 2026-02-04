using BoomScript;

foreach (var expression in Parser.Parse(new SourceText("x = y = 1 + 2 * 3")))
    Console.WriteLine(expression);

Console.WriteLine(Interpreter.Evaluate(new SourceText("x = 1 + 2 * 3\ny = x > 4")));
