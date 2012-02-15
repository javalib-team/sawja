# Indicates ocamlfind to use ONLY if Makefile.config is not generated
FINDER=
OCAMLC= $(FINDER) ocamlc
-include ../Makefile.config

BUDDYH=/usr/include
BUDDYLIB=/usr/lib

.PHONY:all clean opt bc

all:opt bc

install: libbdd
	$(FINDER) install buddy META *.ml *.cmi *.a *.o *.cma *.cmxa

uninstall remove:
	$(FINDER) remove buddy

opt: buddy_stubs.o buddy.cmx
	ocamlmklib -custom -o obuddy -linkall buddy.cmx buddy_stubs.o -lbdd

bc: buddy_stubs.o buddy.cmo
	ocamlmklib -custom -o obuddy -linkall buddy.cmo buddy_stubs.o -lbdd

buddy.cmx : buddy.ml
	ocamlopt -dtypes -w Ae -c buddy.ml
buddy.cmo : buddy.ml
	$(OCAMLC) -dtypes -w Ae -c buddy.ml

buddy_stubs.o : buddy_stubs.c
	$(OCAMLC) -c -custom -ccopt "-Wall -Werror -fPIC" -ccopt -static -I $(BUDDYH) buddy_stubs.c

libbdd:
	cp $(BUDDYLIB)/libbdd.a .

clean cleanall: 
	rm -f *.cmxa *.cmi *.cmo *.cma *.cmx *.a *.o *.so *.annot
