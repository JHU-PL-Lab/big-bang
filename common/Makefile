CABAL_DB_NAME=../.big-bang.cabal.db
PACKAGE_NAME:=$(shell ls -1 *.cabal | grep -E -o "^[^.]+")
CONFIGFLAGS=--disable-executable-profiling --disable-library-profiling \
            --package-db=$(CABAL_DB_NAME)
REGISTER_TAG=.do_register

.PHONY : all
all : build register

$(CABAL_DB_NAME) :
	ghc-pkg init $(CABAL_DB_NAME)

.PHONY : build
build : .is_configured
	cabal build

.PHONY : configure
configure : clean_configure .is_configured

.PHONY : clean_configure
clean_configure :
	$(RM) .is_configured

.PHONY : register
register : build
	if test -e $(REGISTER_TAG); then \
	    ghc-pkg --package-conf=$(CABAL_DB_NAME) unregister --force $(PACKAGE_NAME) || true; \
	    ghc-pkg --package-conf=dist/package.conf.inplace describe $(PACKAGE_NAME) |\
	        ghc-pkg --package-conf=$(CABAL_DB_NAME) register - ; \
	fi

.is_configured : | $(CABAL_DB_NAME)
	cabal configure $(CONFIGFLAGS) && touch .is_configured

.PHONY : clean
clean : clean_configure
	cabal clean

