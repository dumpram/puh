University of Zagreb
Faculty of Electrical Engineering and Computing

PROGRAMMING IN HASKELL

Academic Year 2015/2016

LECTURE 1: Getting started

v1.1

(c) 2015 Jan Šnajder

==============================================================================

> import Data.Char
> import Data.List

=== THE BASICS ===============================================================

* Your favorite editor
* ghci
* program as a sequence of value/function definitions
* literate programming
* ghci commands

=== DEFINING VALUES AND FUNCTIONS ============================================

> x = 2

Functions are also values, so we can define them similarly. There are no
parentheses surrounding the variable:

> inc x = x + 1

Functions of many variables:

> digits2Number x y = x * 10 + y

So, don't write 'digits2Number(x,y)'. That's very non-Haskell!

We can now apply these functions. Again, there are no parentheses:

> y = inc 2
> z = digits2Number 4 2

Function names should be written with the initial letter in lowercase. Other
than that, the usual rules for identifiers apply.

Some built in functions: 'max', 'min', 'succ', 'div', 'mod'.

Infix format:

> w = 25 `div` 2

Note that, when you define values/functions in the interactive interpreter, you
have to put 'let' in front:

--> let x = 2
--> let inc x = x + 1

Why this is so will be clear by the end of today's lecture. Stick with us for a
moment.

=== STRINGS AND CHARACTERS ===================================================

> name = "Humpty Dumpty"

> letter = 'H'

Concatenating strings:

> s = "One " ++ "two " ++ "three"

You cannot concatenate letters! This won't work:

'a' ++ 'b'

Length will give you the length of a string:

> n1 = length "The quick brown fox jumps over the lazy dog"
> n2 = length s

=== IF-THEN-ELSE =============================================================

> condDec x = if x > 0 then x - 1 else x 

> foo x = (if even x then x*2 else 2) + 1

Not the same as:

> foo' x = if even x then x*2 else 2 + 1

> bigNumber x = if x >= 1000 then True else False

Avoid explicitly returning True/False; instead, simply return the whole Boolean
expression.

> bigNumber' x = x >= 1000

Playing with strings a bit:

> merge s1 s2 = 
>   s1 ++ (if s1 < s2 then " is not " else " is ") ++ s2

> merge2 s1 s2 = 
>   s1 ++ " is " ++ (if s1 < s2 then "not " else "") ++ s2

=== GUARDS ===================================================================

> merge3 s1 s2 
>   | s1 < s2   = s1 ++ " is " ++ s2
>   | otherwise = s1 ++ " is not " ++ s2

> grade score | score < 50 = 1
>             | score < 63 = 2
>             | score < 76 = 3
>             | score < 89 = 4
>             | otherwise  = 5

> showSalary amount bonus
>   | bonus /= 0 = "Salary is " ++ show amount ++ ", and a bonus " ++ show bonus 
>   | otherwise  = "Salary is " ++ show amount

=== EXERCISE 1 ===============================================================

1.1. 

- Define 'concat3' that concatenates three strings, but drops the middle one
  if it's shorter than 2 characters (use 'length' function).

1.2.
- Give a simpler definition of 'showSalary', using only one if-then-else
  construct.
- Additionally check that salary is non-negative. If it's negative, return an
  adequate message.

=== HELLO WORLD ==============================================================

* main function
* do block
* compiling

> main1 = putStrLn "Hello, world!"

"putStrLn" is an "IO function", also called and ACTION. It does something to
the real world (prints out on the screen). The act of printing is a SIDE
EFFECT.

If we have a sequence of actions, we need to open up a "do block", and write
the actions one beneath the other, like this:

> main2 = do 
>   putStrLn "Hello, world!"
>   putStrLn "Hello again!!!"

Note that all code in a "do" block has to be indented and aligned.

You can think of a "do" block as a way of grouping a sequence of individual
actions into a single action.

=== PURE AND IMPURE FUNCTIONS ================================================

IO functions are impure. Everything else is pure.

Basic IO functions: putStr, putStrLn, getLine, getChar, readFile, writeFile.

> main3 = do
>   putStrLn "Enter you lucky number"
>   userInput <- getLine
>   putStrLn ("I guess your lucky number is " ++ userInput)

In Haskell, you can think of IO functions as actions that do something and WRAP
UP the impure result into a filthy package.

The '<-' operator takes an action on its right-hand side and a variable on its
left hand side and stores the return value of the action into the variable.
Otherwise said, it UNWRAPS the impure package and gives you a pure value.

Things to remember: 
* "do" block contains impure code
* in impure code, you can call impure functions (IO actions)
* impure functions result in impure values
* to get a pure value from an impure value, unwrap it with '<-' 

Watch out! You can't mix up pure and impure code just like that.

This won't work:

  main4 = do
    putStrLn "Enter you lucky number"
    putStrLn ("I guess your lucky number is " ++ getLine)

You cannot put impure functions where pure function is expected! 

This won't work for the same reason:

  shoutOutLoud = getLine ++ "!!!"

Conversely, you can't put pure functions where impure functions are expected.

 main5 = do
   putStrLn "Enter you lucky number"
   userInput <- "42"
   putStrLn ("I guess your lucky number is " ++ userInput)

In the above example, "42" is already a pure value, so you cannot unwrap it.

Instead, when you need to define a value in impure code, you must use 'let' and
not '<-'.

> main6 = do
>    putStrLn "Enter you lucky number"
>    let userInput = "42"
>    putStrLn ("I guess your lucky number is " ++ userInput)

To summarize:
* for unwrapping a result of an impure function, use '<-'
* for assigning pure values to variables, use 'let'

Putting it all together:

> main7 = do
>   putStrLn "Enter you lucky number"
>   userInput <- getLine
>   let number  = read userInput
>       number' = inc number
>   putStrLn ("I guess your lucky number is NOT " ++ show number')

A note on error messages:

If you wrongly mix pure and impure code, you'll get a long error message which,
at the moment, will be difficult for you to understand. The error message will
probably read like "Couldn't match expected type ... IO ...". This message is
generated by the type checker, which has detected that you're trying to mix
pure and impure values.

=== A SIDE NOTE ON PURITY ====================================================

What's the big fuss with impure code anyways? Isn't Haskell is a purely
functional language?

Yes, it is. The "impure functions" are not impure in the sense that they
introduce side effects throughout the program. For now, you have to believe us.
It will become perfectly clear later.

If you can't hold your horses, read this:
http://chris-taylor.github.io/blog/2013/02/09/io-is-not-a-side-effect/
http://stackoverflow.com/questions/25576184/sleep-in-haskell/25576375#25576375

There's one interesting thing that we can already note, though.

Only impure functions can call other impure functions. That is, if you need IO
in your function, that function becomes an IO action itself, i.e., it becomes
impure. In other words, impurity propagates from inside out.

In Haskell, we pay a lot attention to separating the purely functional (non-IO)
code from impure (IO) code. Back-end logic is typically pure. Front-end
(working with files, GUI, database, networking, etc.) is impure.

The nice thing about pure-impure separation in Haskell is that it is not only
encouraged but also ENFORCED by the language itself. More precisely, it is the
TYPE SYSTEM that prevents you from mixing up pure and impure code. Because you
cannot compile any code unless the type checker approves it, you won't be even
able to compile such code and get it running!

=== NEXT =====================================================================

In the next couple of lectures, we will focus on writing pure code. In the next
lecture, we will look into the basic operations on lists and tuples.

To prepare, read chapter 2 of LYH:
http://learnyouahaskell.com/starting-out

Prep homework will be handed out on Friday, Oct 16.

