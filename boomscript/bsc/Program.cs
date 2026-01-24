// TODO: implement compiler driver

using BoomScript;

const string Source = "1 + 3 * -2";

Console.WriteLine(new Interpreter().Run(Source));