Prerequisite
============

OASIS is needed to compile this project.  OASIS may be installed via opam:

    opam install oasis

Building
========

First, we must use the OASIS metadata to generate a configuration script:

    oasis setup -setup-update dynamic

Then, we can configure the build environment:

    ./configure

And finally, we can make the project:

    make

The re-execution of the "oasis setup" step is only necessary when the _oasis configuration file changes.

