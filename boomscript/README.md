# Boomscript

- `[0-9]+`: Push a number onto the stack.
- `[a-zA-Z_][a-zA-Z0-9_]*`: Push a word onto the stack.
- `[ <words> ]`: Push an array onto the stack.
- `( <words> )`: Quote a sequence of words and push it onto the stack as a single value.

- `+` (alias for `add`): Pop the top two values, add them, and push the result.
- `-` (alias for `sub`): Pop the top two values, subtract the second from the first, and push
- the result.
- `*` (alias for `mul`): Pop the top two values, multiply them, and push the result.
- `/` (alias for `div`): Pop the top two values, divide the second by the first, and push the
- result.
- `<` (alias for `lt`): Pop the top two values, compare if the second is less than the first, and
- push 1 if true, 0 if false.
- `>` (alias for `gt`): Pop the top two values, compare if the second is greater than the first, and
- push 1 if true, 0 if false.
- `=` (alias for `eq`): Pop the top two values, compare if they are equal, and push 1 if true, 0 if false.

- `!` (alias for `apply`): Pop the top value and execute it.
- `%` (alias for `set`): Pop the top two values and store the second value in a variable with the name of the first
  value.
- `@` (alias for `get`): Pop the top value and push the variable's value using the top value as its name.

- `if`: Pop the top two values, check if the second is non-zero, and execute the first if true.
- `ifelse`: Pop the top three values, check if the third is non-zero, and execute the first if true, and execute the
  second if false.
- `while`: Pop the top two values, and while the second value evaluates to non-zero, execute the first.
- `map`: Pop the top two values, and apply the first value to each element in the second value.
- `filter`: Pop the top two values, and use the first value to filter each element in the second value.

- `neg`: Pop the top value, negate it, and push the result.
- `not`: Pop the top value, apply a bitwise NOT, and push the result.
- `and`: Pop the top two values, apply a bitwise OR, and push the result.
- `or`: Pop the top two values, apply a bitwise NOT, and push the result.

- `pop`: Remove the top value from the stack.
- `dup`: Duplicate the top value on the stack.
- `swap`: Swap the top two values on the stack.
- `over`: Copy the second value on the stack and push it on top.
- `nip`: Pop the second value on the stack.
- `rot`: Rotate the top three values on the stack.
- `pick`: Pop the top value, and copy the value at that index in the stack to the top.
