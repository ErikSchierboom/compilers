// TODO: implement compiler driver

using BoomScript;

const string Source = """
                      fn add (x: int, y: int) -> int {
                        z = x + 2
                        z + y
                      }
                      add(1, 2)
                      """;

Console.WriteLine(new Interpreter().Run(Source));