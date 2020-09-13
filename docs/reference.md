## List of native operations

| Usage | Meaning |
|---|---|
| val name `=` | Assigns `name` the meaning of `val` |
| symbol `do` | Evaluated `symbol` |
| cond then else `?` | Evaluated `then` if `cond` is true, and `else` otherwise |
| cond stmt `while` | If `cond` is true, it evaluates `stmt ;stmt while`, and does nothing otherwise |
| num `copy` | Copies the last `num` symbols on the stack |
| symbol `put` | Prints a symbol |
| y x `+` | Adds `x` and `y` together |
| y x `-` | Subtracts `y` from `x` |
| y x `*` | Multiplies `x` and `y` |
| y x `/` | Divides `x` by `y` |
| y x `==` | Returns 1 if x and y are equal, and 0 otherwise |
| y x `<` | Returns 1 if `x` is less than `y`, and 0 otherwise |
| f `floor` | Converts a float `f` into an int, by flooring it |
| code `exit` | Terminates the program with a given exit code |
