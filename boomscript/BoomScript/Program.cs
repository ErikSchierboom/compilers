using BoomScript;

// var sourceText = new SourceText("x = 1 + 2 * 9\ny = x + 7");
var sourceText = new SourceText("x = 1 + 2 * 3\ny = x + 4\ny");

var expressions = Parser.Parse(sourceText);
var boundProgram = Binder.Bind(expressions);
Console.WriteLine(boundProgram);

var interpreterResult = Interpreter.Evaluate(boundProgram);
Console.WriteLine($"Interpreter result: {interpreterResult}");

var compilerResult = Compiler.Run(boundProgram);
Console.WriteLine($"Compiler result: {compilerResult}");
