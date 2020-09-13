# Guide to Paste
*A quick guide to the Paste programming language*

```hs
(gcd =' ;{
    1 ;{
        (copy 2) < ;xch if
        over xch -
        (0 !=' over)
    } while
    xch pop
})
(put (gcd 35 91))
```
## Debug Mode

The Paste interpreter comes with a build-in debug mode, which can be enabled using the `--debug` (or just `-d`) flag in the command line. With debug mode enabled, the interpreter outputs its internal state in every step of the evaluation. This includes the current state of the stack, as well as the part of the program, which is yet to be evaluated.

```hs
> 3 5 min
   ~>  | 3 5 min
   ~>  3 | 5 min
   ~>  3 5 | min
   ~>  3 5 | 2 copy < ;xch if pop
   ~>  3 5 2 | copy < ;xch if pop
   ~>  3 5 3 5 | < ;xch if pop
   ~>  3 5 0 | ;xch if pop
   ~>  3 5 0 xch | if pop
   ~>  3 5 | pop
   ~>  3 5 | ;_ =
   ~>  3 5 _ | =
   ~>  3 |
```

## Symbols

In Paste, everything is a symbol. A symbol may of may not have special meaning attached to it. If a symbol represents an operation or a macro, it is evaluated eagerly. If a symbol has no meaning assigned to it, it is simply placed on the stack during evaluation. Eager evaluation can be deferred, by adding a `;` at the beginning of a token. The `;` itself is not part of the token.
```hs
1 2 +  # stack: 3
4 ;+   # stack: 3 4 +
do     # stack: 7
```

The `;` can also be chained.
```hs
"Test" ;;put do ;;do ;do do do
```

## The Stack

Paste is a stack based language. Every symbol either puts itself onto the stack or does an operation on the stack. Here is a simple example program:
```hs
2 1 3 + * 9 -
```

Let's look at it in a bit more detail:
```hs
2  # 2      puts the symbol 2 onto the stack
1  # 2 1    pushes the symbol 1 onto the stack
3  # 2 1 3  pushes 3 onto the stack
+  # 2 4    pops 2 symbols, adds them together and pushes the result onto the stack
*  # 8      pops 2 symbols and multiplies them together
9  # 8 9    pushes 1 onto the stack
-  # 1      pops 2 symbols and subtracts the first from the second
```

To make working with the stack a bit more comfortable, the following operations and macros are provided by default:
```hs
dup    # a -- a a
pop    # a --
xch    # a b -- b a
over   # a b -- a b a
under  # a b -- b
```

## Macros

Macros are defined using the `=` operator, which pops two symbols off the stack and assigns the first to the second.
```hs
5 n =   # n is now 5
n put   # prints "5"
```

**Note:** This this works with every\* pair of symbols.
```hs
2 4 =   # 4 is now 2
1 4 +   # calculates 1 + 4, which is 3, because 4 is 2
put     # prints "3"
```

Since macros are applied sequentially, circularity is no concern:
```hs
7 5 =  # 7 is now 5
5 7 =  # sets 5 to 5, because 7 is 5
```

\*The symbol `_` (pronounced "meh") cannot be used as a macro. Attempting to assign a macro to `_` is always equivalent to simply popping a symbol off the stack.

## Blocks

Blocks are symbols which contain other symbols. Like every other symbol, they are eagerly evaluated by default. In combination with `;` and these helper symbols, block symbols are one of the most powerful features of the language:
 - `do` pops and evaluated a symbol
 - `if` pops a number and am symbol and only runs the symbol, if the number if different to zero
 - `while` works like `if`, but loops until the popped condition is zero

```hs
{ "Hello World" put }            # always prints "Hello World"
n 5 == ;{ "n is five" put } if   # only prints, when n is equal to 5
0 1 1 1 ;{ "Hi" put } while      # prints "Hi" 3 times
1 ;{ "A" put 1 } while           # infinite loop
```

Block symbols can be given names using macros to simulate function-like behavior:
```hs
;{ 2 copy > ;{ xch } if pop } ;max =  # defines a max "function"
5 2 max   # calculates max of both numbers
put       # prints "5"
```

Named block symbols can also be used to create loops:
```hs
;{ dup put ;b if } b =  # defines a block named "b"
0 1 2 3 4 b             # prints the numbers 4 to 0
```

## Syntax Sugar

Since paste is a stack based language, the token order you naturally end up with is [reverse polish notation](https://en.wikipedia.org/wiki/Reverse_Polish_notation).
To make working with function-like macros a little more intuitive, there is an alternative to the normal code blocks, which uses parenthesis instead of curly brackets.
These alternative blocks reverse the order of their containing symbols during parsing, effectively simulating polish notation:
```hs
(quit)         ~>  {quit}
(put "hello")  ~>  {"hello" put}
(+ 4 3)        ~>  {3 4 +}
(put mod 7 3)  ~>  {3 7 mod put}
```

Additionally, a tick `'` can be used to move a symbol to the beginning of a block. This reordering step is applied before the above mentioned token reversal, which makes working with binary operators very comfortable:
```hs
(a =' 3)         ~>  (= a 3)        ~>  {3 a =}
(b =' (a +' 2))  ~>  (= b (+ a 2))  ~>  {{2 a +} b =}
```
