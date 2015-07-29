# Reference: https://github.com/ocaml/oasis2opam/blob/f1ee33f94f146e7fee618d30512dbfb9b3c68cc9/.travis-ci.sh

OPAM_PKGS="oasis batteries menhir monadlib ocaml-monadic"
OPAM_PKGS_TEST="ounit"

export OPAMYES=1

if [ -f "$HOME/.opam/config" ]; then
    opam update
    opam upgrade
else
    opam init
fi

if [ -n "${OPAM_SWITCH}" ]; then
    opam switch ${OPAM_SWITCH}
fi
eval `opam config env`

opam install $OPAM_PKGS

export OCAMLRUNPARAM=b
oasis setup
opam install $OPAM_PKGS_TEST
./ensure-oasis-configuration
make test
