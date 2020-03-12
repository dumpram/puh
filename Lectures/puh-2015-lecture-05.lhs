University of Zagreb
Faculty of Electrical Engineering and Computing

PROGRAMMING IN HASKELL

Academic Year 2015/2016

LECTURE 5: Recursive functions

v1.0

(c) 2015 Jan Snajder

==============================================================================

> import Data.Char
> import Data.List

== RECAP =====================================================================

Last week we've covered the full syntax of Haskell functions. You now know how
to write arbitrary complex functions, at least in terms of syntax. However, to
really unleash the full computational power of Haskell, you need to know how
to write recursive functions.

== INTRO =====================================================================

Recursion: function calls itself.

In FP, many problems are solved using recursion. The main idea: divide the
problem into smaller subproblems and try to solve these subproblems as simplest
cases first.

The simplest case is called "the base case".

A common example is the factorial function:

> fact :: (Eq a, Num a) => a -> a
> fact x = if x==0 then 1 else x * fact (x-1)

Or, better:

> fact' :: (Eq a, Num a) => a -> a
> fact' 0 = 1
> fact' x = x * fact' (x-1)

Another typical example is the Fibonacci Number:

> fib 0 = 0
> fib 1 = 1
> fib n = fib (n-1) + fib (n-2)

(This definition is not the best one. Do you know why?)

Haskellers are quite proud of the quicksort definition:

> quicksort :: (Ord a) => [a] -> [a]
> quicksort [] = []
> quicksort (x:xs) = quicksort ys ++ [x] ++ quicksort zs
>   where ys = [y | y <- xs, y <= x]
>         zs = [z | z <- xs, z  > x]

In what follows, we focus on recursion over a data structure. This is typically
a LIST or a TREE, but more generally it can be any recursive structure. We call
such a recursion STRUCTURAL RECURSION. We use structural recursion to process a
data structure (something we would do with a loop in an imperative language).

The main idea: recurse down the structure by gradually decomposing it using
pattern matching, and combining the results:

> sum' :: Num a => [a] -> a
> sum' []     = 0
> sum' (x:xs) = x + sum' xs

> length' :: [a] -> Int
> length' []     = 0
> length' (_:xs) = 1 + length' xs

> incList :: Num a => [a] -> [a]
> incList []     = []
> incList (x:xs) = (x + 1) : incList xs

(The last function can be defined via list comprehension. How?)

> concat' :: [[a]] -> [a]
> concat' []       = []
> concat' (xs:xss) = xs ++ concat' xss

> maximum' :: Ord a => [a] -> a
> maximum' [x]    = x
> maximum' (x:xs) = x `max` maximum' xs

(What would happen if we were to apply this function to an empty list?)

What is the time complexity of the above functions? (Except for
the 'fib' and 'quicksort' functions.)

For lists of length 'n', the time complexity is O(n).

Notice that there is a recurring pattern in the above functions: 

foo ...                             <-- base case
foo (x:xs) = f x `operator` foo xs  <-- general case

== EXERCISE 1 ================================================================

1.1.
- Define a recursive function to compute the product of a list of elements.
  product' :: Num a => [a] -> a

1.2.
- Define a recursive function 'headsOf' that takes a list of lists and
  returns a list of their heads.
  headsOf :: [[a]] -> [a]
  headsOf [[1,2,3],[4,5],[6]] => [1,4,6]

==============================================================================

A recursive function can of course have many arguments. Arguments that remain
unchanged throughout the recursive calls serve merely to store the state. We
call such arguments CONTEXT VARIABLES. For example:

> addToList :: Num a => a -> [a] -> [a]
> addToList _ []     = []
> addToList n (x:xs) = x + n : addToList n xs

Of course, if required, we can change the variables in each recursive call:

> incIncList :: Num a => a -> [a] -> [a]
> incIncList _ []     = []
> incIncList n (x:xs) = x + n : incIncList (n+1) xs

What if we wanted to define a function that increments the first element by 0,
the second by 1, etc.?

incIncList' [3,2,1] => [3,3,3]

We don't want the user to always provide 0 as the argument. Instead, we define
a WRAPPER FUNCTION:

> incIncList' :: Num a => [a] -> [a]
> incIncList' xs = inc 0 xs
>   where inc _ []     = []
>         inc n (x:xs) = x + n : inc (n+1) xs

== EXERCISE 2 ================================================================

2.1.
- Define a recursive function 'modMult n m xs' that multiplies each element of
  a list 'xs' with 'n' modulo 'm'.

2.2.
- Define a function 'addPredecessor' that adds to each element of a list the
  value of the preceding element. The first element gets no value added.
  addPredecessor :: Num a => [a] -> [a]
  addPredecessor [3,2,1] => [3,5,3]

==============================================================================

In the recursive case we can test additional conditions and act based on these.
This is how we can implement the filtering of a list:

> numPositives :: (Num a, Ord a) => [a] -> Int
> numPositives []     = 0
> numPositives (x:xs) | x > 0     = 1 + numPositives xs
>                     | otherwise = numPositives xs

== EXERCISE 3 ================================================================

3.1.
- Define 'equalTriplets' that filters from a list of triplets (x,y,z) all
  triplets for which x==y==z.
  equalTriplets [(1,2,3),(2,2,2),(4,5,6)] => [(2,2,2)]

3.2.
- Define your own version of the replicate function:
  replicate' :: Int -> a -> [a]

==============================================================================

Let's define 'take':

> take' :: Int -> [a] -> [a]
> take' 0 _      = []
> take' _ []     = []
> take' n (x:xs) = x : take' (n-1) xs

Does this work as expected if n<0 (does it return an unaltered list)?

How can we extend the above definition so that, if n > length xs, the last
element of the list gets repeated?
take'' 5 [1,2,3] => [1,2,3,3,3]

How would you define the same function using standard functions from Prelude?

== EXERCISE 4 ================================================================

4.1.
- Define your own recursive version of the drop function:
  drop' :: Int -> [a] -> [a].
- Define drop'' (a wrapper function) so that for n < 0 the function drops
  the elements from the end of the list. You can use 'reverse'.

4.2.
- Define a recursive function 'takeFromTo n1 n2 xs'.
  takeFromTo :: Int -> Int -> [a] -> [a]

==============================================================================

Here's how 'zip' function is defined:

> zip' :: [a] -> [b] -> [(a,b)]
> zip' []     _      = []
> zip' _      []     = []
> zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

How can we extend this so that it only pairs up (x,y) for which x==y?

We don't always need to process the elements one by one. For example, a
function that takes a list and pairs up the consecutive elements would be
defined like this:

> pairUp :: [a] -> [(a,a)]
> pairUp (x:y:xs) = (x,y) : pairUp xs
> pairUp _        = []

== EXERCISE 5 ================================================================

5.1.
- Define a recursive function 'eachThird' that retains every third element
  in a list.
  eachThird :: [a] -> [a]
  eachThird "zagreb" => "gb"

5.2.
- Define a recursive function 'crossZip' that zips two lists in a "crossing"
  manner:
  crossZip [1,2,3,4,5] [4,5,6,7,8] => [(1,5),(2,4),(3,7),(4,6)]

== ACCUMULATORS ==============================================================

Let's look at the definition of the factorial function again:

> fact1 :: (Eq a, Num a) => a -> a
> fact1 0 = 1
> fact1 x = x * fact1 (x-1)

This function is executed as follows: we go down until we "hit" the base case,
and then build up the result incrementally as we return from the recursive
calls. Actually, the result is built up while on our way back.

But another solution is possible, one in which we recurse down and "accumulate"
the solution incrementally as we descend. When we "hit" the base case, we
simply return the solution accumulated thus far. There is no need to go back,
because there is nothing left to be done on the way back. We can simply "jump
out" of the recursion.

> fact2 :: (Eq a, Num a) => a -> a -> a   -- the second arg. is the accumulator
> fact2 0 n = n
> fact2 x n = fact2 (x-1) (x*n)

We also need a wrapper function that will set the initial value (which equals
one):

> fact3 :: (Eq a, Num a) => a -> a
> fact3 x = fact2 x 1

All recursive function defined above were defined using "standard" recursion
(without the accumulator). Many of those can be defined using the accumulator.
For instance, instead of:

> sum1 :: Num a => [a] -> a
> sum1 []     = 0
> sum1 (x:xs) = x + sum1 xs

> sum2 :: Num a => [a] -> a
> sum2 xs = sum xs 0
>   where sum []     s = s              -- 's' is the accumulator
>         sum (x:xs) s = sum xs (x+s)

But why? Isn't it all the same?

It's not. In principle, the accumulator-style definitions are of less
complexity (space complexity, but sometimes also time complexity).

A somewhat lengthy explanation:

When a recursive call is a part of a larger expression, to be able to compute
the value of the expression we first have invoke the recursive function. It is
only after the recursive call returns that we can compute the value of the whole
expression. Conceptually, this means that we gradually build up a larger and
larger expression, and we can start reducing this expression only after we've
reached the base case. (Technically, this is accomplished by storing the
context and the return address on stack for each recursive call.) The
intermediate structure grows with each recursive call. If there is only one
recursive call, and it's invoked 'n' times, then the space complexity will be
O(n).

On the other hand, if the recursive call is NOT a part of a larger expression,
then no structure is built as we recurse down. In this case, we don't even need
to store the return address on stack. Instead, we can simply make the recursive
call with new parameters (as if were using a GOTO with parameters), because
there is nothing that is waiting to be computed AFTER the recursive call is
completed. We call such recursive functions TAIL RECURSIVE. The compiler will
detect that a function is tail recursive and optimize the code (TAIL CALL
OPTIMIZATION). Since we don't have to store anything on stack, the space
complexity is constant, O(1).

Brent Yorgey:
"A recursive function is tail recursive if the final result of the recursive
call is the final result of the function itself.  If the result of the
recursive call must be further processed (say, by adding 1 to it, or consing
another element onto the beginning of it), it is not tail recursive."
http://www.haskell.org/pipermail/haskell-cafe/2009-March/058607.html

Thus: 'sum1' has a space complexity of O(n), while 'sum2' has a space
complexity of O(1).

Let's define an accumulator-style version of 'maximum'. The standard version
is:

> maximum1 :: Ord a => [a] -> a
> maximum1 []  = error "empty list"
> maximum1 [x] = x
> maximum1 (x:xs) = x `max` maximum' xs

Accumulator-style version:

> maximum2 :: (Ord a, Num a) => [a] -> a
> maximum2 [] = error "empty list"
> maximum2 xs = maximum xs 0
>   where maximum []     m = m
>         maximum (x:xs) m = maximum xs (max x m)

There's actually no need to limit ourselves to the 'Num' typeclass here, so
let's give a slightly more generic definition:

> maximum3 :: (Ord a, Bounded a) => [a] -> a
> maximum3 [] = error "empty list"
> maximum3 xs = maximum xs minBound
>   where maximum []     m = m
>         maximum (x:xs) m = maximum xs (max x m)

Actually, a more clever definition avoids using a bound altogether:

> maximum4 :: (Ord a) => [a] -> a
> maximum4 []     = error "empty list"
> maximum4 (x:xs) = maximum xs x
>   where maximum []     m = m
>         maximum (x:xs) m = maximum xs (max x m)

The difference between standard and accumulator-style recursion is quite
drastic in the case of the 'reverse' function. The standard definition is as
follows:

> reverse1 :: [a] -> [a]
> reverse1 []     = []
> reverse1 (x:xs) = reverse1 xs ++ [x]

Space complexity is O(n) but time complexity is as much as O(n^2).
Can you say why?

Accumulator-style version:

> reverse2 :: [a] -> [a]
> reverse2 xs = rev xs []
>   where rev []     ys = ys
>         rev (x:xs) ys = rev xs (x:ys)

What is the complexity of this function?

Another advantage of accumulator-style definitions is that in some cases it is
perhaps easier to comprehend the behavior of such functions (accumulator-style
definitions are closer to imperative-style programming).

So it seems that using accumulator-style recursion is a good idea. But there is
a caveat to it. Because Haskell is lazy, the accumulator will not be fully
evaluated and might grow gradually in memory, causing a MEMORY LEAKAGE. Thus,
using accumulators only makes sense if they are STRICT. More on this in later
lectures.

== EXERCISE 6 ================================================================

6.1.
- Write an accumulator-style recursive definition of
  length' :: [a] -> Int

> length'' xs = len 0 xs
>   where len n []     = n
>         len n (_:xs) = len (n+1) xs

6.2
- Write an accumulator-style recursive definition of
    maxUnzip :: [(Int,Int)] -> (Int,Int)
  that returns the maximum element at the first position and the maximum
  element at the second position in a pair, i.e., it's equivalent to:
    maxUnzip zs = (maximum xs, maximum ys)
      where (xs,ys) = unzip zs
  If the list is empty, return an "empty list" error.
- Now write a standard recursive definition (without an accumulator).

== GUARDED RECURSION ==========================================================

Sometimes, using an accumulator doesn't even make sense to begin with. For
example, if we do structural recursion on a list and modify each element in
turn. Look at the 'incList' function:

> incList1 :: Num a => [a] -> [a]
> incList1 []     = []
> incList1 (x:xs) = x + 1 : incList1 xs

The space complexity of this function is O(1). (The list that is being
constructed is not counted in the space complexity. Only additional memory
allocated during the computation is counted, but here no extra memory is being
allocated.)

We might give it a shot with an accumulator-style version:

> incList2 :: Num a => [a] -> [a]
> incList2 xs = inc xs []
>   where inc []     ys = ys
>         inc (x:xs) ys = inc xs ((x+1):ys)

The problem here is that we can only prepend to the list and thus the
accumulated list will be in reverse order. This would have been OK for
'reverse', but here it's no good. Moreover, accumulator-style doesn't buy us
anything, because the space complexity of the function was already O(1).

Another example is 'unzip'. We may give it a try with accumulators (two in this
case):

> unzip' :: [(a,b)] -> ([a],[b])
> unzip' zs = unz zs [] []
>   where unz []         xs ys = (xs,ys)
>         unz ((x,y):zs) xs ys = unz zs (x:xs) (y:ys)

But this is again not good for the same reason as above: we end up with lists
in reverse order. We might first reverse the input list, but that would require
two list traversals (one for the reversal and one for unzipping).

Hence in this case too we should resort to "standard" recursion:

> unzip'' :: [(a,b)] -> ([a],[b])
> unzip'' []         = ([],[])
> unzip'' ((x,y):zs) = (x:xs,y:ys)
>   where (xs,ys) = unzip'' zs

The two functions above ('incList' and 'unzip') have one thing in common: we
use recursion to create the output list(s) incrementally. The result is
immediately becoming available and continues to grow as the recursion
progresses. Because Haskell is LAZY, if we choose to consume just the first
part of the result, the recursion will never generate results beyond that point
since these results are not really needed. In other words, the result of the
function can be CONSUMED LAZILY. This is called GUARDED RECURSION. In guarded
recursion, the recursive call occurs within a "data constructor" (cons operator
':' in this case). Because of laziness, the expression will be evaluated up to
the data constructor and the recursive call delayed until needed.

Notice that guarded recursion is not tail recursive. However, there is nothing
left to be done after exiting the recursive call, so space complexity is O(1).
Hence we call such recursion TAIL RECURSION MODULO CONS.

SUMMARY:

Tail recursion reduces space complexity. To achieve it, use:

* Accumulator-style, but only if you need the whole result (e.g., sum, max,
  length, etc.).

* If you don't need the whole result at once but wish to consume it lazily,
  use guarded recursion, which will give you tail recursion modulo cons.

== CORECURSION ===============================================================

Corecursion is "dual" to recursion: instead of decomposing a structure, we
build it up. In recursion, each recursive call is applied to a structure that
is smaller than the input structure. Conversely, in corecursion, the recursive
call is applied to a larger structure than the input structure and there is no
base case. The structure that we build up can be finite or infinite. Of course,
because of laziness, we will build only as much as needed.

> ones :: [Integer]
> ones = 1 : ones

> cycle' :: a -> [a]
> cycle' x = x : cycle' x

In each step we can use a part of the already constructed structure.

List of natural numbers:

> nats :: [Integer]
> nats = 0 : next nats
>   where next (x:xs) = x + 1 : next xs

A bit more complex: a list of Fibonacci Numbers:

> fibs :: [Integer]
> fibs = 0 : 1 : next fibs
>   where next (x:ys@(y:_)) = (x+y) : next ys

More details here:
http://programmers.stackexchange.com/questions/144274/whats-the-difference-between-recursion-and-corecursion

== NEXT ======================================================================

In Haskell, everything's a function. Next, we'll look at HIGHER ORDER FUNCTIONS
(HOF), which are functions that take or return other functions. HOF allow for
functional design patterns, which make our code more structured, more modular,
and more comprehensible.

