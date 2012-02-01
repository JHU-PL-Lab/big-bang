CONFIGFLAGS=--disable-executable-profiling --disable-library-profiling
REGISTER_TAG=.do_register

.PHONY : all
all : build register

.PHONY : build
build : .is_configured
	cabal build

.PHONY : configure
configure :
	cabal configure && touch .is_configured

.PHONY : register
register : build
	test -e $(REGISTER_TAG) && cabal register --inplace || true

.is_configured :
	cabal configure $(CONFIGFLAGS) && touch .is_configured

.PHONY : clean
clean :
	cabal clean
	rm -f .is_configured
