.PHONY:all sawja cleanall doc

all:sawja

sawja:
	$(MAKE) -C src

clean cleanall cleandoc:
	$(MAKE) -C src $@
	$(RM) *~

doc:
	$(MAKE) -C src doc
