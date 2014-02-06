BigBang
=======
This repository contains a number of development projects related to the BigBang research project.  The contents of each directory are summarized below.


`common`
--------
Contains resources common to numerous projects.  This is intended for Makefile fragments or other very general tools.


`tiny-bang`
-----------
The core TinyBang implementation.  This includes a TinyBang interpreter and typechecker.


`tiny-bang-nested`
------------------
The TingBang Nested implementation.  This is a language translation tool which parses a nested syntax for TinyBang and translates it into the ANF used in the `tiny-bang` project.


`little-bang`
-------------
The LittleBang implementation.  This is a language translation tool which parses more advanced language features and desugars them into the form used by `tiny-bang-nested`.


`eclipse-plugin`
----------------
Contains an Eclipse PDE project for a BigBang Eclipse plugin.


`related-languages`
-------------------
Contains implementations of languages related to BigBang.  These projects may not be actively maintained; they are often forks of old code modified for a specific research subproject.
