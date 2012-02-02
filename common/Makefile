CONFIGFLAGS=--disable-executable-profiling --disable-library-profiling
REGISTER_TAG=.do_register

.PHONY : all
all : configure build register

.PHONY : build
# Forcing configuration on every build for the time being.
#build : .is_configured
# If project B depends on project A and project A changes, project B must be
# reconfigured.  Unfortunately, it's hard to tell when project A *changes*;
# for the time being, we'll just reconfigure and pay the time.
build : configure
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
	$(RM) .is_configured
