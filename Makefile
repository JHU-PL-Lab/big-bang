PROJECTS=tiny-bang tiny-bang-nested little-bang

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
