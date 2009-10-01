.PHONY:all jaws cleanall doc

all:jaws

jaws:
	$(MAKE) -C src

clean cleanall:
	$(MAKE) -C src $@

doc:
	$(MAKE) -C src doc
