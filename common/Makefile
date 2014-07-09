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
	$(CABAL) install --force-reinstalls  

.PHONY: clean
clean:
	$(CABAL) clean
	$(RM) cabal.sandbox.config

.PHONY: clean-all
clean-all: clean
	$(RM) -r $(SANDBOX_DIR)
