// TODO: implement compiler driver

using BoomScript;

const string Source = "x = 2\n" +
                      "(1 + x) * (3 + 2)";

Console.WriteLine(new Interpreter().Run(Source));