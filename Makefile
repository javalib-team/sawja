.PHONY:all buddycaml sawja install clean cleanall cleandoc doc

all:sawja

sawja:
	$(MAKE) -C src

# Package-specific targets
buddycaml installbuddycaml removebuddycaml:%buddycaml:
	$(MAKE) -C buddycaml $*

install remove:
	$(MAKE) -C src $@

distclean:clean
	$(RM) Makefile.config

clean cleanall :
	$(MAKE) -C src $@
	$(MAKE) -C doc $@
	$(MAKE) -C buddycaml $@
	$(RM) *~

cleandoc doc:
	$(MAKE) -C src $@
	$(RM) *~

# Documentation for release (generate INSTALL and README)
cleandocr docr:
	$(MAKE) -C doc $@
