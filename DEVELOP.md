Project Organization
====================

The BigBang project is written in OCaml and makes use of OASIS to manage the
build process.  OASIS may be installed from opam via "opam install oasis".
Although OASIS provides the ability to customize the build process by making
changes to the generated setup.ml file, we currently do not use this mechanism
and the setup file is excluded from the repository.  Instead, users should
generate the setup file themselves and then compile the project using the
following commands:

    oasis setup -setup-update dynamic
    ./configure
    make


Project Sources
---------------

Source files in the project appear under the `src` directory.  This directory
contains a number of subdirectories, each of which represents a single library
in the `_oasis` configuration file.  Those directories contain only a flat
presentation of OCaml source files.


Coding Style
============

This project uses the standard coding conventions for the OCaml language, which
are summarized at `http://caml.inria.fr/resources/doc/guides/guidelines.en.html`,
particularly with regard to identifier naming.
