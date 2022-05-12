# 6945-final-project
Final project for MIT's 6.945.

## Project Structure

All source code lives in `src`. Sections of the SDF codebase have been copied over -- in particular, `src/common/` and `src/user-defined-types/`. The code for graph primitives lives in `src/graph/`. The mechanism in `load.scm` allows for the loading of all relevant files into the current environment, if `(load [path-to-load.scm])` is executed. Currently, `run.scm` contains an example of how all the pieces can come together to make an application.

## How to Use

We have several demos demonstrating the capabilties of our code. The first is in `src/molecule-demo.scm`, which demonstrates some of the molecular manipuations we are able to do using a molecular representation extension of our library. 

The second is in `src/save-demo-1.scm` and `src/save-demo-2.scm`. The first demo creates graph objects which are then saved to a save file `save-demo.save`, and the second demo reloads those objects.


