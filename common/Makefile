CABAL=cabal
SANDBOX_DIR=../cabal-sandbox

.PHONY : all
all: init build

$(SANDBOX_DIR)/bin/alex:
	$(CABAL) install alex

$(SANDBOX_DIR)/bin/happy:
	$(CABAL) install happy

.PHONY : build-tools
build-tools: $(SANDBOX_DIR)/bin/alex $(SANDBOX_DIR)/bin/happy

cabal.sandbox.config:
	$(CABAL) sandbox --sandbox=$(SANDBOX_DIR) init

.PHONY : init
init: cabal.sandbox.config
	$(CABAL) sandbox add-source .

.PHONY: build
build: init build-tools
	$(CABAL) install --only-dependencies
	$(CABAL) configure
	$(CABAL) build
	$(CABAL) install

.PHONY: clean
clean:
	$(CABAL) clean
	-$(CABAL) sandbox hc-pkg -- unregister --force $(shell basename *.cabal .cabal)
	
.PHONY: purge
purge: clean
	$(CABAL) sandbox delete-source .
	$(CABAL) sandbox delete
	rm -f cabal.sandbox.config

