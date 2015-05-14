Prerequisite
============

OASIS is needed to compile this project.  OASIS may be installed via opam:

    opam install oasis

Building
========

It's recommended that you build the project in an appropriate `opam` switch:

    opam switch 4.02.1

First, install the dependencies:

    opam install batteries menhir ounit

Then, we must use the OASIS metadata to generate a configuration script:

    oasis setup -setup-update dynamic

Then, we can configure the build environment:

    ./configure

And finally, we can make the project:

    make

From this point onward, the re-execution of "make" is sufficient to rebuild the
project (even when the _oasis file changes).
