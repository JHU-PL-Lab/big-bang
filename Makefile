PROJECTS=tiny-bang tiny-bang-tests tiny-bang-nested tiny-bang-nested-tests little-bang little-bang-tests

.PHONY : all
all: $(PROJECTS)

.PHONY : clean
clean : $(foreach proj,$(PROJECTS),clean_$(proj))

.PHONY : $(PROJECTS)
$(PROJECTS): %:
	cd $@ && $(MAKE)

.PHONY : $(foreach proj,$(PROJECTS),clean_$(proj))
$(foreach proj,$(PROJECTS),clean_$(proj)): clean_%:
	cd $(patsubst clean_%,%,$@) && $(MAKE) clean

.PHONY : purge
purge: clean
	rm -rf cabal-dev-sandbox
