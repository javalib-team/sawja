# This file is part of Sawja
# Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
# Copyright (c)2009 David Pichardie (INRIA)
# Copyright (c)2010, 2011 Vincent Monfort (INRIA)
#
# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public
# License along with this program.  If not, see 
# <http://www.gnu.org/licenses/>.

INCLUDE=-package sawja
OCAMLOPT=ocamlfind ocamlopt $(INCLUDE) -w Aer
OCAMLC=ocamlfind ocamlc $(INCLUDE) -w Aer
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
