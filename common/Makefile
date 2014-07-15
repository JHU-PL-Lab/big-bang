CABAL=cabal
SANDBOX_DIR=../cabal-sandbox

.PHONY : all
all: build install

cabal.sandbox.config:
	$(CABAL) sandbox --sandbox=$(SANDBOX_DIR) init

.PHONY: build
build: cabal.sandbox.config
	$(CABAL) install --only-dependencies
	$(CABAL) configure
	$(CABAL) build

.PHONY: install
install: build
	$(CABAL) sandbox add-source .

.PHONY: clean
clean:
	$(CABAL) clean
	$(CABAL) sandbox delete-source .
	-$(CABAL) sandbox hc-pkg -- unregister --force $(shell basename *.cabal .cabal)

.PHONY: clean-all
clean-all: clean
	$(CABAL) sandbox delete
