# Spreads #

An always terminating, statically typed spreadsheet!

## Motivation ##

Have you ever felt that your spreadsheet was holding you back? Wondered if there was a better alternative?
Tired of thinking about what really is the last argument of COUNTIF? If life really makes any sense?

Meet SPREADS!

Spreads is an always-terminating, well typed spreadsheet. Based on the [Morte](https://github.com/Gabriel439/Haskell-Morte-Library) language, Spreads allows you to:
 - Define functions!
 - Always terminate!
 - Have a reasonable COUNTIF interface!
 - Have a reasonable VLOOKUP, HLOOKUP, MATCH and friends interface!
 
Furthermore, Spreads has a layers concept, that allows you to clearly separate the Presentation of a value and the Value in itself.
No more "What the hell, favourite spreadsheet, 5 is not a string?"
 
## Status ##

Spreads is not really itself yet. For now, this repo should be seen as a very early stage prototype. Notable *missing* elements are:
 - Almost all of normal spreadsheet functions!
 - Arithmetic functions!
 - Documentation!
 - Almost everything!

If I were to risk designing a plan, I would say it would be the following:
 1. Come up with a reasonable way to infer some types (for arithmetic operations).
 2. Come up with a reasonable way to print multiline functions.
 3. Iron out details about how the Presentation layer works.
 3. Complete the standard library.
 4. ???
 5. Profit

## Is it any good? ##

Not yet.

## I really want to try this early stage prototype that does nothing except having a multi-cell Morte interpreter, how do I do it? ##

* Clone the repo
* Install [the haskell tool `stack`](http://haskellstack.org/)
* `stack setup`
* `stack build`
* `stack exec -- Spreads`
