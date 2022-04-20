# 6945-final-project
Final project for MIT's 6.945.

## Project Structure

All source code lives in `src`. Sections of the SDF codebase have been copied over -- in particular, `src/common/` and `src/user-defined-types/`. The code for graph primitives lives in `src/graph/`. The mechanism in `load.scm` allows for the loading of all relevant files into the current environment, if `(load [path-to-load.scm])` is executed. Currently, `run.scm` contains an example of how all the pieces can come together to make an application.

## How to Use

There are two ways of running `run.scm`. The first is to open up a REPL separately, and then run each statement in `run.scm`. This works, but requires that the REPL be initialized properly with the statement `(load "[path-to-load.scm]")`.

The other way of running `run.scm` is to run (in terminal) `mit-scheme --load run.scm`, which has the benefit of being able to use the statement `(load "load")`, rather than inserting the path to `load.scm` in the load statement.


