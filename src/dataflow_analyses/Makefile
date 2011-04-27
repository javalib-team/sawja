INCLUDE=-package sawja
OCAMLOPT=ocamlfind ocamlopt $(INCLUDE) 
OCAMLC=ocamlfind ocamlc $(INCLUDE) 
OCAMLDEP=ocamlfind ocamldep
SRC_FOR_LIB=
SRC_FOR_MAIN= main
SRC=$(SRC_FOR_LIB) $(SRC_FOR_MAIN)
SRCMLI=

test: run
	@echo "USAGE: run [file.class] or [file.jar]"
	@echo "TEST on test/C.class:"
	javac -g test/C.java
	./run test/C.class

all: $(SRCMLI:=.cmi) $(SRC:=.cmo) $(SRC:=.cmx)


run: all
	$(OCAMLOPT) -linkpkg -o $@ $(SRC:=.cmx)

run.bc: all
	$(OCAMLC) -linkpkg -o $@ $(SRC:=.cmo)

dvad-plugin: dvad_plugin.cmx
	$(OCAMLOPT) -linkpkg -o $@ $^

clean:
	rm -f *.o *.a dvad_plugin.cm[iox] $(SRC:=.cm[iox]) *~ *.annot test/*~ test/*.class run dvad-plugin

cleanall:clean


.depend: $(SRC:=.ml)
	$(OCAMLDEP) $(SRCMLI:=.mli) $(SRC:=.ml) > .depend

.SUFFIXES: .ml .mli .cmo .cmi .cmx .cma .cmxa

%.cmx %.o: %.ml
	$(OCAMLOPT) -dtypes -c $<
%.cmo: %.ml
	$(OCAMLC) -dtypes -c $<
%.cmi: %.mli
	$(OCAMLC) -dtypes -c $<

#-include .depend
