# This file is part of Jaws
# Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
# Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
# Copyright (c)2009 Nicolas Barre (INRIA)
#
# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this program.  If not, see 
# <http://www.gnu.org/licenses/>.



-include ../Makefile.config
INCLUDE:=$(INCLUDE)
DOCDIR:=../$(DOCDIR)

CSS=\"style.css\"

MODULES=wlist jProgram jControlFlow jNativeStubs jCRA jRTA jRRTA jPrintHtml partial cmn bir jBir fBir bBir cfaBir jaws 
MODULE_INTERFACES=wlist jProgram jControlFlow jNativeStubs jCRA jRTA jRRTA jPrintHtml partial cmn jBir fBir bBir cfaBir jaws 

.SUFFIXES : .cmo .cmx .cmi .ml .mli
.PHONY : all install clean cleanall cleandoc doc

all: jaws.cma jaws.cmxa

install: jaws.cma jaws.cmxa
	mkdir -p $(INSTALL_DIR)
	cp -f jaws.cma jaws.cmxa jaws.a $(MODULE_INTERFACES:=.cmi) $(INSTALL_DIR)
	cp -f $(MODULE_INTERFACES:=.mli) $(INSTALL_DIR)

ocaml:jaws.cma
	$(OCAMLMKTOP) $(INCLUDE) -o $@ unix.cma zip.cma extLib.cma ptrees.cma str.cma javaLib.cma jaws.cma

.depend:$(MODULE_INTERFACES:=.mli) $(MODULES:=.ml)
	$(OCAMLDEP) $(PP) $^ > $@
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),cleanall)
-include .depend
endif
endif

jNativeStubs.cmo:jNativeStubs.ml
	$(OCAMLC) $(INCLUDE) $(PP) -c $<
jNativeStubs.cmx jNativeStubs.o:jNativeStubs.ml
	$(OCAMLOPT) $(INCLUDE) $(PP) -c $<

jPrintHtml.cmo:jPrintHtml.ml
	$(OCAMLC) $(INCLUDE) -pp "/usr/bin/perl -0777 -p -e 'BEGIN{open(CSS,$(CSS));\$$css = <CSS>;}s/\(\* CSS \*\)/\$$css/;'" -c $<

jPrintHtml.cmx jPrintHtml.o:jPrintHtml.ml
	$(OCAMLOPT) $(INCLUDE) -pp "/usr/bin/perl -0777 -p -e 'BEGIN{open(CSS,$(CSS));\$$css = <CSS>;}s/\(\* CSS \*\)/\$$css/;'" -c $<

.ml.cmo:
	$(OCAMLC) $(INCLUDE) -c $<
%.cmx %.o:%.ml
	$(OCAMLOPT) $(INCLUDE) -c $<
.mli.cmi:
	$(OCAMLC) $(INCLUDE) -c $<

jaws.cma: $(MODULE_INTERFACES:=.cmi) $(MODULES:=.cmo)
	$(OCAMLC) -a $(MODULES:=.cmo) -o $@

jaws.cmxa: $(MODULE_INTERFACES:=.cmi) $(MODULES:=.cmx)
	$(OCAMLOPT) -a $(MODULES:=.cmx) -o $@
# endif

doc: $(MODULE_INTERFACES:=.cmi) $(MODULES:=.ml) ../intro.ocamldoc
	mkdir -p $(DOCDIR)
	$(OCAMLDOC) $(INCLUDE) -d $(DOCDIR) -html -stars		\
		-colorize-code -intro ../intro.ocamldoc -t Jaws		\
		$(MODULE_INTERFACES:=.mli)
	echo "</body></html>" >> $(DOCDIR)/index.html # workarround for a bug in ocamldoc

clean:
	rm -rf .depend *.cmi *.cmo *.cmx *.annot *.obj *.o *.a *~ 

cleanall: clean cleandoc
	rm -rf ocaml tests tests.opt sample sample.opt *.cmi	\
		*.cma *.cmxa
cleandoc:
	rm -rf $(DOCDIR)
