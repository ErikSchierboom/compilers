// TODO: implement compiler driver

using BoomScript;

const string Source = "1 + 2 * (3 + 1)";

Console.WriteLine(new Interpreter().Run(Source));