.PHONY:all sawja install clean cleanall cleandoc doc

all:sawja

sawja:
	$(MAKE) -C src

install:
	$(MAKE) -C src $@

clean cleanall cleandoc doc:
	$(MAKE) -C src $@
	$(MAKE) -C doc $@
	$(RM) *~
