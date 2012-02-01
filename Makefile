PROJECTS=utils interpreter micro-bang tests

.PHONY : all
all : $(PROJECTS)

.PHONY : clean
clean : $(foreach proj,$(PROJECTS),clean_$(proj))

.PHONY : $(PROJECTS)
$(PROJECTS): %:
	cd $@ && $(MAKE)

.PHONY : $(foreach proj,$(PROJECTS),clean_$(proj))
$(foreach proj,$(PROJECTS),clean_$(proj)): clean_%:
	cd $(patsubst clean_%,%,$@) && $(MAKE) clean
