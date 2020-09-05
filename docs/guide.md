# Guide to Paste
*A quick guide to the Paste programming language*

```py
(gcd = ;{
    1 ;{
        (copy 2) < ;xch if
        over xch -
        (0 != over)
    } while
    xch pop
})
(put (35 gcd 91))
```
## Debug Mode

The Paste interpreter comes with a build-in debug mode, which can be enabled using the `--debug` (or just `-d`) flag in the command line. With debug mode enabled, the interpreter outputs its internal state in every step of the evaluation. This includes the current state of the stack, as well as the part of the program, which is yet to be evaluated.

```py
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

## Objects

In Paste, everything is an object. An object may of may not have special meaning attached to it. If an object represents an operation or a macro, it is evaluated eagerly. If an object has no meaning assigned to it, it is simply placed on the stack during evaluation. Eager evaluation can be deferred, by adding a `;` at the beginning of a token. The `;` itself is not part of the token.
```py
1 2 +  # stack: 3
4 ;+   # stack: 3 4 +
do     # stack: 7
```

The `;` can also be chained.
```py
"Test" ;;put do ;;do ;do do do
```

## The Stack

Paste is a stack based language. Every object either puts itself onto the stack or does an operation on the stack. Here is a simple example program:
```py
2 1 3 + * 9 -
```

Let's look at it in a bit more detail:
```py
2  # 2      puts the object 2 onto the stack
1  # 2 1    pushes the object 1 onto the stack
3  # 2 1 3  pushes 3 onto the stack
+  # 2 4    pops 2 objects, adds them together and pushes the result onto the stack
*  # 8      pops 2 objects and multiplies them together
9  # 8 9    pushes 1 onto the stack
-  # 1      pops 2 objects and subtracts the first from the second
```

To make working with the stack a bit more comfortable, the following operations and macros are provided by default:
```py
dup    # a -- a a
pop    # a --
xch    # a b -- b a
over   # a b -- a b a
under  # a b -- b
```

## Macros

Macros are defined using the `=` operator, which pops two objects off the stack and assigns the first to the second.
```py
5 n =   # n is now 5
n put   # prints "5"
```

**Note:** This this works with every\* pair of objects.
```py
2 4 =   # 4 is now 2
1 4 +   # calculates 1 + 4, which is 3, because 4 is 2
put     # prints "3"
```

Since macros are applied sequentially, circularity is no concern:
```py
7 5 =  # 7 is now 5
5 7 =  # sets 5 to 5, because 7 is 5
```

\*The object `_` (pronounced "meh") cannot be used as a macro. Attempting to assign a macro to `_` is always equivalent to simply popping an object off the stack.

## Blocks

Blocks are objects which contain other objects. Like every other object, they are eagerly evaluated by default. In combination with `;` and these helper objects, block objects are one of the most powerful features of the language:
 - `do` pops and evaluated an object
 - `if` pops a number and am object and only runs the object, if the number if different to zero
 - `while` works like `if`, but loops until the popped condition is zero

```py
{ "Hello World" put }            # always prints "Hello World"
n 5 == ;{ "n is five" put } if   # only prints, when n is equal to 5
0 1 1 1 ;{ "Hi" put } while      # prints "Hi" 3 times
1 ;{ "A" put 1 } while           # infinite loop
```

Block objects can be given names using macros to simulate function-like behavior:
```py
;{ 2 copy > ;{ xch } if pop } ;max =  # defines a max "function"
5 2 max   # calculates max of both numbers
put       # prints "5"
```

Named block objects can also be used to create loops:
```py
;{ dup put ;b if } b =  # defines a block named "b"
0 1 2 3 4 b             # prints the numbers 4 to 0
```

## Syntax Sugar

To make working with binary operators easier, there is an alternative to the normal code blocks, which uses parenthesis instead of curly brackets. These alternative blocks reorder their inner objects during parsing. The reordering works as follows:
```py
(exit)  ~>  {exit}
(put "hello")  ~>  {"hello" put}
(4 - 3)  ~>  {3 4 -}
(1 2 3 4 5)  ~>  {5 4 3 1 2}
```

This is especially useful when working with macros rather than on the stack directly:
```py
(a = 3)
(b = (a + 2))

(a != b) ;(put "this will print") if
```
