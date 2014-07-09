CABAL=cabal
SANDBOX_DIR=../cabal-sandbox

.PHONY : all
all: build install

$(SANDBOX_DIR):
	$(CABAL) sandbox --sandbox=$(SANDBOX_DIR) init

.PHONY: build
build: $(SANDBOX_DIR)
	$(CABAL) install --only-dependencies
	$(CABAL) configure
	$(CABAL) build

.PHONY: install
install: build
	$(CABAL) install --force-reinstalls  

.PHONY: clean
clean:
	$(CABAL) clean

.PHONY: clean-all
clean-all: clean
	$(RM) -r $(SANDBOX_DIR)
