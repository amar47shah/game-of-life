# Conway's Game of Life

## Setup and Run
1. Install [stack](http://docs.haskellstack.org/en/stable/README.html).
2. Clone and `cd` into the repository.
3. `stack setup` to install GHC.
4. `stack build` to compile.
5. `stack exec game-of-life-exe` to run.
6. Zoom out (or in) in your terminal to fit the board.
7. You can change `start` in `src/Life.hs` to any pattern exported from `src/Life/Pattern.hs`.
8. You can also adjust `size`, `ticks`, and `wait` in `app/Main.hs`.
8. Don't forget to compile again after adjusting the code.
