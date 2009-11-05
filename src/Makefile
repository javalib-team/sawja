# This file is part of Sawja
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

MODULES=wlist jProgram jControlFlow jNativeStubs jCRA jRTA jRRTA cmn jsrInline bir jBir a3Bir jPrintHtml
MODULE_INTERFACES=wlist jProgram jControlFlow jNativeStubs jCRA jRTA jRRTA cmn jsrInline jBir a3Bir jPrintHtml

.SUFFIXES : .cmo .cmx .cmi .ml .mli
.PHONY : all install remove clean cleanall cleandoc doc

all: sawja.cma sawja.cmxa

install: META sawja.cma sawja.cmxa $(MODULE_INTERFACES:=.mli) $(MODULE_INTERFACES:=.cmi)
	$(INSTALL) sawja $^ bir.cmi sawja.a

remove:
	ocamlfind remove sawja

ocaml:sawja.cma
	$(OCAMLMKTOP) $(INCLUDE) -linkpkg -o $@ sawja.cma

.depend:$(MODULE_INTERFACES:=.mli) $(MODULES:=.ml)
	$(OCAMLDEP) $(PP) $^ > $@

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),cleanall)
ifneq ($(MAKECMDGOALS),cleandoc)
-include .depend
endif
endif
endif

jNativeStubs.cmo:jNativeStubs.ml
	$(OCAMLC) $(INCLUDE) $(PP) -c $<
jNativeStubs.cmx jNativeStubs.o:jNativeStubs.ml
	$(OCAMLOPT) $(INCLUDE) $(PP) -c $<

jPrintHtml.cmo:jPrintHtml.ml
	$(OCAMLC) $(INCLUDE) -pp "$(PERL) -0777 -p -e 'BEGIN{open(CSS,$(CSS));\$$css = <CSS>;}s/\(\* CSS \*\)/\$$css/;'" -c $<

jPrintHtml.cmx jPrintHtml.o:jPrintHtml.ml
	$(OCAMLOPT) $(INCLUDE) -pp "$(PERL) -0777 -p -e 'BEGIN{open(CSS,$(CSS));\$$css = <CSS>;}s/\(\* CSS \*\)/\$$css/;'" -c $<

.ml.cmo:
	$(OCAMLC) $(INCLUDE) -c $<
%.cmx %.o:%.ml
	$(OCAMLOPT) $(INCLUDE) -c $<
.mli.cmi:
	$(OCAMLC) $(INCLUDE) -c $<

sawja.cma: $(MODULE_INTERFACES:=.cmi) $(MODULES:=.cmo)
	$(OCAMLC) -a $(MODULES:=.cmo) -o $@

sawja.cmxa: $(MODULE_INTERFACES:=.cmi) $(MODULES:=.cmx)
	$(OCAMLOPT) -a $(MODULES:=.cmx) -o $@

JAVALIB=`ocamlfind query javalib`
doc: $(MODULE_INTERFACES:=.cmi) $(MODULES:=.ml) ../intro.ocamldoc
	mkdir -p $(DOCDIR)

	$(OCAMLDOC) $(RECODE) $(INCLUDE) -d $(DOCDIR) -colorize-code -t Sawja	\
		-html -stars -intro ../intro.ocamldoc $(MODULE_INTERFACES:=.mli) \
		$(JAVALIB)/{javalib,jBasics,jCode}.mli
	echo "</body></html>" >> $(DOCDIR)/index.html # workarround for a bug in ocamldoc

clean:
	$(RM) .depend *.cmo *.cmx *.annot *.obj *.o *~ 
cleanall: clean
	$(RM) ocaml *.cmi *.cma *.cmxa *.a
cleandoc:
	$(RM) -r $(DOCDIR)
