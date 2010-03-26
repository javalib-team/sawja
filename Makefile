.PHONY:all sawja install clean cleanall cleandoc doc

all:sawja

sawja:
	$(MAKE) -C src

install remove:
	$(MAKE) -C src $@

distclean:clean
	$(RM) Makefile.config

clean cleanall cleandoc doc:
	$(MAKE) -C src $@
	$(MAKE) -C doc $@
	$(RM) *~
