Advent of Code 2016
===================

I'm working through the [Advent of Code](http://adventofcode.com/), trying to
solve them all in haskell. I'm not going for any sort of efficiency initially,
as long as the solutions run fast enough on my computer to get the answers
within a reasonable amount of time.

I'm using trifecta and lens, so the initial dependency build might take a bit
of time on a weaker processor. Such is the way with these nice libraries.

Build with `stack build`

Run with `stack exec aoc2016 <day>`

Individual solutions are in `src/Days/DayX.hs` where X is the day number.

Input data for each problem can be found in `input/dayX`. Try adding your own
data to see if my code works on it! My code accepts the input data in the
format provided by the Advent of Code website, and parses it at runtime.

Running with `stack exec aoc2016 <day> test` will cause the program to read
input data from `input/dayXtest`, which I use to try my code with the sample
data provided in the problem.
