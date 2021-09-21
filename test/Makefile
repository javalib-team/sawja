# This file is part of Sawja
# Copyright (c)2021 David Pichardie (Facebook)
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



-include ../Makefile.config
#INCLUDE:=$(INCLUDE) -package sawja
INCLUDE:=$(INCLUDE) -I ../src/
LIBS =unix zip extLib str javalib sawja


MODULES = sawjap
EXECS = sawjap

.SUFFIXES : .cmo .cmx .cmi .ml .mli

sawjap: $(MODULES:=.cmx)
	$(OCAMLOPT) $(INCLUDE) $(LIBS:=.cmxa) $< -o $@ 

%.ml: %.mll
	ocamllex $^	

%.ml %.mli: %.mly
	ocamlyacc $^

.ml.cmo:
	$(OCAMLC) $(INCLUDE) -c $<
%.cmx %.o:%.ml
	$(OCAMLOPT) $(INCLUDE) -c $<
.mli.cmi:
	$(OCAMLC) $(INCLUDE) -c $<
clean:
	$(RM) $(EXECS) $(MODULES:=.cm[iox]) $(MODULES:=.o) *~ 
