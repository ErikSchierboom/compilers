using BoomScript;

var sourceText = new SourceText("x = 1 + 2 * 3\ny = x > 4");

var expressions = Parser.Parse(sourceText);
foreach (var expression in expressions)
    Console.WriteLine(expression);

Console.WriteLine(Binder.Bind(expressions));

Console.WriteLine(Interpreter.Evaluate(sourceText));
