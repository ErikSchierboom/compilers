// TODO: implement compiler driver

using BoomScript;

const string Source = "x = 33\n" +
                      "1 + 3 * -2";

Console.WriteLine(new Interpreter().Run(Source));