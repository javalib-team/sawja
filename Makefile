.PHONY:all jaws cleanall doc

all:jaws

jaws:
	$(MAKE) -C src

clean:
	$(MAKE) -C src clean

doc:
	$(MAKE) -C src doc