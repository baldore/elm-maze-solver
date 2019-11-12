# Maze Solver

Create your own maze and see what is the shortest path.

## Experience

I learned a lot of this project. I was able to apply so many things about functional programming.
Also, Elm is incredible. The hardest part was to think about the tests and how to solve the problem
using functional programming.

However, the actual code was easy to write and (at least for me) looks extremely clean. I expect
Elm to keep growing and I'll be looking forward to see what else will come for this great technology.

`Update 11/12/2019`: The code needs a lot of improvements. Maybe I will work on it if I want to dedicate again to Elm in the future.

## How to run it

Just run `npm start` and go to `http://localhost:8000/src/index.html`.

## Issues

I made a lot of mistakes using Elm@0.18 that in the moment were not issues, such as variable shadowing. I think I will have to migrate step by step the code if I want to make it work with the latest version, but that is not in my current plans.

## TODO

-[x] Read the article and (hopefully) understand the algorithm.
-[x] Implement the algorithm.
-[ ] Add error messages for
  - Missing Start/End cell.
  - Maze is not solvable.
  Also, show an error message that should disappear after some time.
-[ ] Create Github page to see the maze solver working.
-[ ] Fix tests after removing webpack 1.

## Thanks

The solution was taken from https://www.codeproject.com/articles/9040/maze-solver-shortest-path-finder. Very
good explanation.
