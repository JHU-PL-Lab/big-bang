FROM avsm/docker-opam-build:debian-stable-ocaml-4.02.1-system
MAINTAINER Leandro Facchinetti <lfacchi2@jhu.edu>

RUN opam install oasis batteries menhir ounit monadlib ocaml-monadic
