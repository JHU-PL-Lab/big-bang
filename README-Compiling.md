Compiling
=========
This file contains information related to compiling the projects in this directory for use.

The primary BigBang projects (TinyBang, TinyBang Nested, and LittleBang) may be compiled by the Makefile in the root of this project directory.  Executing `make` should be sufficient if all of the necessary dependencies are installed:

    * GHC 7.6.1 or greater and Cabal 1.8 or greater (e.g. Haskell Platform 2012)
    * cabal-dev

cabal-dev is a Cabal package and, on a machine with the other two requirements, may be installed via `cabal install cabal-dev`.  cabal-dev is installed to `~/.cabal/bin` by default; this directory must be on `PATH` or the Makefile will not run correctly by default.  (Alternately, the location of the cabal-dev binary may be set by, e.g., `make CABAL_DEV=~/.cabal/bin/cabal-dev`.)

If the project requires other dependencies which are not installed, cabal-dev will download, compile, and install them into a sandbox location.

Upon a successful build, the interpreters for the languages may be found at e.g. `tiny-bang/dist/build/tiny-bang/tiny-bang`.

If system wide installation is desired, it should be sufficient to change into each project directory and run `cabal configure && cabal build && cabal install`.
