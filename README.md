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
