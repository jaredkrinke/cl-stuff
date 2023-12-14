# cl-stuff
This is a monorepo for all my [Common Lisp](https://lisp-lang.org/) code.

Why Common Lisp? Originally, I wanted to [try out REPL-driven development](https://log.schemescape.com/posts/programming-languages/learning-lisp-in-2023.html). That went well enough that I'm still exploring.

# Projects
| Directory | Explanation |
|:----------|:------------|
| **aoc23**         | My solutions to [Advent of Code 2023](https://adventofcode.com/2023) |
| **battlesnake**   | A simple, rule-driven [Battlesnake](https://play.battlesnake.com/) client -- my first project |
| [**cl-micropm**](https://github.com/jaredkrinke/cl-micropm) | A trivial package manager for Common Lisp, based on [Risto-Stevcev/cl-micropm](https://github.com/Risto-Stevcev/cl-micropm) -- I only use this because [Quicklisp](https://www.quicklisp.org/beta/) doesn't download packages securely |
| **cryptopals**    | Some initial work on [Cryptopals](https://www.cryptopals.com/) challenges |
| **deps**          | This is where all dependencies from **cl-micropm** get added as submodules |
| **dirmon**        | Prototype for enumerating file system changes under a given directory tree |
| [**foolander**](https://github.com/jaredkrinke/cl-stuff/tree/main/foolander) | In-browser snake game that runs entirely server-side and uses absolutely no JavaScript -- created for [Autumn Lisp Game Jam 2023](https://itch.io/jam/autumn-lisp-game-jam-2023) |
| **halp**          | My personal utility library -- every Common Lisp developer is required to have one of these |
| **ssg**           | Experimental static site generator that supports incremental rebuilds |
| **symbolic**      | My attempts at solving the exercises from [Common Lisp: A Gentle Introduction to Symbolic Computation](https://www.cs.cmu.edu/~dst/LispBook/) |
