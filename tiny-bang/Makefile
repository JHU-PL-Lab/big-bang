CABAL=cabal
CABAL_DEV=cabal-dev
SANDBOX_DIR=../cabal-dev-sandbox
CABAL_CMD=$(CABAL_DEV) -s $(SANDBOX_DIR) --with-cabal-install=$(CABAL)

.PHONY : all
all: build install

$(SANDBOX_DIR):
	mkdir -p $(SANDBOX_DIR)

.PHONY: build
build: $(SANDBOX_DIR)
	$(CABAL_CMD) install-deps
	$(CABAL_CMD) configure
	$(CABAL_CMD) build

.PHONY: install
install: build
	$(CABAL_CMD) install --force-reinstalls  

.PHONY: clean
clean:
	$(CABAL_CMD) clean

