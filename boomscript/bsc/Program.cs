// TODO: implement compiler driver

using BoomScript;

var sourceText = new SourceText("1 + 2 * (3 + 1)");
var syntaxTree = new SyntaxTree(sourceText);
var parser = new Parser(syntaxTree);
var compilationUnit = parser.ParseCompilationUnit();
Console.WriteLine(compilationUnit);