# [Sliding Tiles Puzzle](https://4onen.github.io/SlidingTilesPuzzle/)

(Click [here](https://4onen.github.io/SlidingTilesPuzzle/) to play!)

The sliding tiles puzzle is a classic puzzle game where the objective
is to line up all numbers, left-to-right, top-to-bottom, in order, with
the empty space in the lower-right. Any tile adjacent to the empty
space can be slid into its place, moving the tile one location and the
space one location. By repeating this move dozens or hundreds of times,
a solvable board may be brought to the desired final configuration.

Of note: in the space of all possible boards, half of the boards cannot
be solved. This is because the placement of the space and numbers 
necessitates a certain number of swaps of tiles, which for an arbitrary
board may not be possible while leaving the empty space in the
lower-right.

This implementation of the puzzle will never generate an unsolvable
board, no matter the size of board selected. This is because, given an
arbitrary board, if the board is unsolvable it can be made solvable by
removing and swapping any two tiles (outside of standard game rules.)
The unsolvability check is a simple parity check of an *O(n^2)* boolean
condition across all *n* tiles.

(Note: Because not all functions necessary to generate the board are
tail-recursive, boards in excess of ~4000 tiles may fail to generate
and, indeed, crash the application. Practically, this is a non-issue
because nobody really wants more than a 10x10 board to contend with,
much less 50x80.)