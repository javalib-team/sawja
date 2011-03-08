-include ../Makefile.config

DEST=..
doc: $(DEST)/INSTALL $(DEST)/README
all:doc

.DELETE_ON_ERROR:$(DEST)/INSTALL $(DEST)/README

$(DEST)/INSTALL:install.html
	$(LYNX) -dump $< > $@

$(DEST)/README:intro.html aboutversion.html history.html documentation.html demonstration.html contacts.html
	echo "" > $@
	for i in $^ ; do $(LYNX) -dump "$$i" >> $@ ;done

clean:
	rm -f *~

cleanall:clean

cleandoc:clean
	rm -f $(DEST)/INSTALL $(DEST)/README
