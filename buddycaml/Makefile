BUDDYH=/usr/local/include
BUDDYLIB=/usr/local/lib

.PHONY:all clean opt bc

all:opt bc

install:
	ocamlfind install buddy META *.{ml,cmi,a,cma,cmxa,so}

uninstall remove:
	ocamlfind remove buddy

opt: buddy_stubs.o buddy.cmx
	ocamlmklib -verbose -o obuddy -linkall buddy.cmx buddy_stubs.o -L$(BUDDYLIB) -lbdd

bc: buddy_stubs.o buddy.cmo
	ocamlmklib -o obuddy -linkall buddy.cmo buddy_stubs.o -L$(BUDDYLIB) -lbdd

buddy.cmx : buddy.ml
	ocamlopt -dtypes -w Ae -c buddy.ml
buddy.cmo : buddy.ml
	ocamlc -dtypes -w Ae -c buddy.ml

buddy_stubs.o : buddy_stubs.c
	ocamlc -c -ccopt "-Wall -Werror -fPIC" -I $(BUDDYH) buddy_stubs.c

clean cleanall: 
	rm -f *.{cmxa,cmi,cmo,cma,cmx,a,o,so,annot}
