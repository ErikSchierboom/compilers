using BoomScript;

var sourceText = new SourceText("x = 1 + 2 * 3\ny = x > 4");

var expressions = Parser.Parse(sourceText);
var boundProgram = Binder.Bind(expressions);
Console.WriteLine(boundProgram);

var value = Interpreter.Evaluate(boundProgram);
Console.WriteLine(value);

var compiledProgram = Compiler.Run(boundProgram);

object[] args2={"Hello."};
object myObject = Activator.CreateInstance(compiledProgram,null,null);
var myMethodInfo = compiledProgram.GetMethod("MyMethod");
myMethodInfo.Invoke(myObject,args2);
