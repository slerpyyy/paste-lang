# Paste
*An esoteric programming language build around macros*

Paste is a dynamically typed, stack based esoteric programming language build around macros, which are defined and applied at runtime.

```hs
(fib =' ;{
    ;n =
    0 1
    (n >' 0) ;{
        xch over +
        (;n =' (n -' 1))
        (n !=' 0)
    } while
    pop
})
(put (fib 24))
```
(the program above prints out the 24th fibonacci number)

## The Language

If you are interested in playing around with Paste, I highly recommend you check out my [Guide to Paste](docs/guide.md) for an overview to what this language has to offer.

## Usage

Here is a short description of what the interpreter can do. You can also get this information from the program directly, by using the `--help` flag.
```
Usage: paste [OPTIONS] INPUT

An esoteric programming language build around macros

Options:
    -h, --help    Print this help message and exit.
    -r, --repl    Enter interactive mode after the script terminated.
    -d, --debug   Show all internal computations during evaluation.
```
