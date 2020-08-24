# Paste
*An esoteric programming language build around macros*

Paste is a dynamically typed, stack based esoteric programming language build around macros, which are defined and applied at runtime.

```c
(fib = ;{
    ;n =
    0 1
    (n > 0) ;{
        xch over +
        (;n = (n - 1))
        (n != 0)
    } while
    pop
})
(put (fib 24))
```
(the program above prints out the 24th fibonacci number)

## The Language

If you are interested in playing around with Paste, I highly recommend you check out my [Guide to Paste](guide.md) for an overview to what this language has to offer.