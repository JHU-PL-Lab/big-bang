CONFIGFLAGS=--disable-executable-profiling --disable-library-profiling
CONFIGURE=cabal configure $(CONFIGFLAGS)
BUILD=cabal build
#REGISTER=cabal register --inplace
CLEAN=cabal clean

.PHONY : all interpreter tests
all : interpreter tests

interpreter :
	cd $@ && $(CONFIGURE) && $(BUILD)
#	cd $@ && $(CONFIGURE) && $(BUILD) && $(REGISTER)

tests : CONFIGFLAGS:=$(CONFIGFLAGS) --package-db=../interpreter/dist/package.conf.inplace
tests :
	cd $@ && $(CONFIGURE) && $(BUILD)

.PHONY : clean clean-interpreter clean-tests
clean : clean-interpreter clean-tests

clean-interpreter :
	cd interpreter && $(CLEAN)

clean-tests :
	cd tests && $(CLEAN)
