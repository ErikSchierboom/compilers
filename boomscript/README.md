# Boomscript

- `[0-9]+`: Push a number onto the stack.
- `[a-zA-Z_][a-zA-Z0-9_]*`: Push a word onto the stack.
- `[ <words> ]`: Quote a sequence of words and push it onto the stack as a single value.

- `!`: Pop the top value and execute it.
- `$`: Pop the top two values and store the second value in a variable with the name of the first value.
- `@`: Pop the top value and push the variable's value using the top value as its name.

- `?`: Pop the top two values, check if the second is non-zero, and execute the first if true.
- `#`: Pop the top two values, and while the second value evaluates to non-zero, execute the first.

- `+`: Pop the top two values, add them, and push the result.
- `-`: Pop the top two values, subtract the second from the first, and push
- the result.
- `*`: Pop the top two values, multiply them, and push the result.
- `/`: Pop the top two values, divide the second by the first, and push the
- result.
- `<`: Pop the top two values, compare if the second is less than the first, and
- push 1 if true, 0 if false.
- `>`: Pop the top two values, compare if the second is greater than the first, and
- push 1 if true, 0 if false.
- `=`: Pop the top two values, compare if they are equal, and push 1 if true, 0 if false.
- `_`: Pop the top value, negate it, and push the result.
- `~`: Pop the top value, apply a bitwise NOT, and push the result.
- `&`: Pop the top two values, apply a bitwise OR, and push the result.
- `|`: Pop the top two values, apply a bitwise NOT, and push the result.

- `%`: Remove the top value from the stack.
- `.`: Duplicate the top value on the stack.
- `:`: Swap the top two values on the stack.
- `\`: Copy the second value on the stack and push it on top.
- `;`: Rotate the top three values on the stack.
- `^`: Pop the top value, and copy the value at that index in the stack to the top.
