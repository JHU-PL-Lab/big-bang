PROJECTS=tiny-bang tiny-bang-tests tiny-bang-nested tiny-bang-nested-tests little-bang little-bang-tests

### A target to build everything
.PHONY : all
all: $(PROJECTS)

### The various build targets for the projects
.PHONY : $(PROJECTS)
$(PROJECTS): %:
	cd $@ && $(MAKE)

### A target to initialize everything
.PHONY : init
init : $(foreach proj,$(PROJECTS),init_$(proj))

.PHONY : $(foreach proj,$(PROJECTS),init_$(proj))
$(foreach proj,$(PROJECTS),init_$(proj)): init_%:
	cd $(patsubst init_%,%,$@) && $(MAKE) init

### A target to clean everything
.PHONY : clean
clean : $(foreach proj,$(PROJECTS),clean_$(proj))

.PHONY : $(foreach proj,$(PROJECTS),clean_$(proj))
$(foreach proj,$(PROJECTS),clean_$(proj)): clean_%:
	cd $(patsubst clean_%,%,$@) && $(MAKE) clean

### A target to purge (uninit) everything
.PHONY : $(foreach proj,$(PROJECTS),purge_$(proj))
$(foreach proj,$(PROJECTS),purge_$(proj)): purge_%:
	cd $(patsubst purge_%,%,$@) && $(MAKE) purge

.PHONY : purge 
purge: clean $(foreach proj,$(PROJECTS),purge_$(proj))
	rm -rf cabal-sandbox

