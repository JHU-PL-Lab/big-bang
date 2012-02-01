CONFIGFLAGS=--disable-executable-profiling --disable-library-profiling
REGISTER_TAG=.do_register

.PHONY : all
all : build register

.PHONY : build
build : .is_configured
	cabal build

.PHONY : register
register : build
	if test -e $(REGISTER_TAG); then \
		cabal register --inplace; \
	fi

.is_configured :
	cabal configure $(CONFIGFLAGS) && touch .is_configured

.PHONY : clean
clean :
	cabal clean
	rm -f .is_configured
