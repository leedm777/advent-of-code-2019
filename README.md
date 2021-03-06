# advent-of-code-2019

My solutions for the [2019 Advent of Code](https://adventofcode.com/2019)

## Usage

    $ lein run dayXX # for day00 through day25

## Things I learned

 - `day00`
   - You have to require a namespace before `find-ns` can find it
   - You can return `(reduced X)` to short circuit a `(reduce)` to return
     `X`.
 - `day01`
   - You need to be really quick to score points
   - `quot` returns just the quotient of a division; simpler than converting
     to `int`
 - `day02`
   - `(assoc vector location value)` is the proper way to update a vector
   - The `->>` and `->` macros are a pain if you need to mix first/last args
   - Use `as->` to mix and match better
   - `case` is more like switch-case than `cond`
 - `day03`
   - Lots of off by one errors trying to figure out this solution
   - Cider (or maybe the REPL in general) requires-in a lot of things that aren't
     required in by defaults, so tests and stuff may pass in the REPL, but will fail
     during `lein run` or `lein test`
   - There's a million different ways to lookup key `k` in map `m`
     - `(m k)`
     - `(get m k)`
     - `(:k m) ;; if k is a keyword`
   - My solution would have been much simpler if I had focused on tracking just the path
     instead of overall state (points, position, etc)
   - Cider keystrokes
     - `C-c M-n (M-)n` - switch to current namespace
     - `C-c C-z` - switch to repl buffer
 - `day04`
   - Even simple things can be hard if you think about them too much.
   - Wasn't obvious from the docs, but `(update)` can update vectors :-)
   - `(partition-by)` is super cool. "Applies f to each value in coll,
     splitting it each time f returns a new value."
   - `(re-find)` returns first match; `(re-matches)` has to match the whole string.
 - `day05`
   - `zero?` exists
   - `if-not` exists
   - Really should've had a single map with machine state that each method
     operated on. See https://github.com/sooheon/aoc/blob/6e490e55f5cc9797f43060b89a94e9756e5be9be/aoc-clojure/src/main/aoc/2019/day05.clj
     - Which I couldn't resist and actually ended up doing...
 - `day06`
   - Data structures matter a lot. I started mapping center->outward, but couldn't get what
     I needed. I reversed it to map out->center, and then it became easy.
   - Interestingly, the path function I built for the 2nd problem would've been
     helpful for the first. Refactored according.
 - `day07`
   - `int-code` is getting more and more complicated
     - I don't know if I had added the halted state earlier if it would've helped or hurt
       trying to pause it.
     - I also wonder how awful a solution threads would've been.
 - `day08`
   - `=` considers vectors and lists equal, so `(= [1 2 3] '(1 2 3))`. Very handy.
   - `(map vector a b)` will zip `a` and `b` into vectors
     ```clojure
     (map vector [:a1 :a2 :a3] [:b2 :b2 :b3])
     ;; => ([:a1 :b2] [:a2 :b2] [:a3 :b3])
     ```
 - `day09`
   - There's no built-in sparse vector in Clojure. That surprisingly would've been easier
     in JavaScript
   - The bigint support, though, was really helpful :-)
   - Took 17s to run part 2. I wonder if my computer is really slow, or if that was typical
 - `day10`
   - Coordinate systems are hard. I kept doing math where positive numbers went up and to
     the right instead of down and to the right.
   - Also, lots of off-by-one errors; finding the 200th element of required `(nth 199)`
   - Clojure has `Math/atan2`, which would've solved a lot of my problems.
   - And `interleave` would've made running through the sorted lists easier
   - `lazy-cat` could've made my fill memory much easier...
 - `day11`
   - Math coordinates (postive y up) and computer coordinates (positive y down)
     will always confuse me.
   - I'm glad I made IntCode input/output queues the other day.
 - `day12`
   - Laziness causes stack overflow errors when a ton of work gets chained up.
     - Had to use `mapv` instead of `map` in a few places to avoid SO errors
   - Optimizing part 2 required actual cleverness; hadn't really needed that yet.
     - Each axis is independent, and the cycles for each axis is pretty findable. the
       total cycle time is the least-common-multiple of the cycle time for each axis.
 - `day13`
   - Today's puzzle made me unreasonably happy
   - I might want to optimize my int-code computer

## License

Copyright © 2019 David M. Lee, II

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
