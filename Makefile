CONFIGFLAGS=--disable-executable-profiling --disable-library-profiling
CONFIGURE=cabal configure $(CONFIGFLAGS)
BUILD=cabal build
REGISTER=cabal register --inplace
CLEAN=cabal clean

.PHONY : all utils interpreter tests micro-bang
all : utils interpreter tests micro-bang

utils :
	cd $@ && $(CONFIGURE) && $(BUILD) && $(REGISTER)

interpreter :
	cd $@ && $(CONFIGURE) && $(BUILD) && $(REGISTER)

micro-bang :
	cd $@ && $(CONFIGURE) && $(BUILD)

tests :
	cd $@ && $(CONFIGURE) && $(BUILD)

.PHONY : clean clean-interpreter clean-tests clean-micro-bang
clean : clean-utils clean-interpreter clean-tests clean-micro-bang

clean-utils :
	cd utils && $(CLEAN)

clean-interpreter :
	cd interpreter && $(CLEAN)

clean-micro-bang :
	cd micro-bang && $(CLEAN)

clean-tests :
	cd tests && $(CLEAN)

