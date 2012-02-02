PROJECTS=utils interpreter micro-bang tests

.PHONY : all
all : $(PROJECTS)

.PHONY : clean
clean : $(foreach proj,$(PROJECTS),clean_$(proj))

.PHONY : configure
configure : $(foreach proj,$(PROJECTS),configure_$(proj))

.PHONY : $(PROJECTS)
$(PROJECTS): %:
	cd $@ && $(MAKE)

.PHONY : $(foreach proj,$(PROJECTS),clean_$(proj))
$(foreach proj,$(PROJECTS),clean_$(proj)): clean_%:
	cd $(patsubst clean_%,%,$@) && $(MAKE) clean

.PHONY : $(foreach proj,$(PROJECTS),configure_$(proj))
$(foreach proj,$(PROJECTS),configure_$(proj)): configure_%:
	cd $(patsubst configure_%,%,$@) && $(MAKE) configure
