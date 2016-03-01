# This file is part of Sawja
# Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
# Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
# Copyright (c)2009 Nicolas Barre (INRIA)
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



-include ../Makefile.config
DOCDIR:=../$(DOCDIR)
DATAFLOW=dataflow_analyses/
HEAP_PARSER=heap_parser/
INCLUDE:=$(INCLUDE) -I $(DATAFLOW) -I $(HEAP_PARSER)
SED=sed

CSS=\"style.css\"

ifeq ($(BUDDY),yes)
CLASS_DOMAIN = classDomainBDD.ml
else
CLASS_DOMAIN = classDomainPtrees.ml
endif

MODULES = jUtil iter iter2 wlist \
	jProgram classDomain jsrInline \
	jControlFlow jPrintUtil jPrintHtml jPrintPlugin argPlugin\
	jCodePP cmn bir birA3 jBir jBirPP a3Bir a3BirPP \
	$(DATAFLOW)live_bir $(DATAFLOW)live_a3bir \
	jBirSSA jBirSSAPP a3BirSSA a3BirSSAPP \
	safe reachableMethods jNativeStubs jType \
	$(HEAP_PARSER)/parserType $(HEAP_PARSER)/parse_heap $(HEAP_PARSER)lex_heap  $(HEAP_PARSER)heapParser \
	jCRA jRTA jRTAWithHeap jRRTA XTA \
	jCFAOptions jCFADom jCFAPrinter jCFA \
	$(DATAFLOW)reachDef $(DATAFLOW)availableExpr \
	$(DATAFLOW)availableExprSSA

MODULE_INTERFACES = iter wlist \
	jProgram classDomain jNativeStubs jsrInline \
	jControlFlow jPrintUtil jPrintHtml jPrintPlugin argPlugin\
	jCodePP jBir jBirPP a3Bir a3BirPP jBirSSA jBirSSAPP a3BirSSA a3BirSSAPP \
	$(DATAFLOW)live_bir $(DATAFLOW)live_a3bir \
	safe reachableMethods jUtil jType \
	$(HEAP_PARSER)/parserType $(HEAP_PARSER)parse_heap $(HEAP_PARSER)heapParser\
	jCRA XTA jRTA jRTAWithHeap jRRTA \
	jCFAOptions jCFADom jCFAPrinter jCFA \
	$(DATAFLOW)reachDef $(DATAFLOW)availableExpr


MODULE_INTERFACES_DOC = $(filter-out cmn ssaBir jPrintUtil jUtil ,$(MODULE_INTERFACES))
HIDE_MOD_IN_DOC = -hide JPrintPlugin.NewCodePrinter

FOR_PACK= -for-pack Sawja_pack

.SUFFIXES : .cmo .cmx .cmi .ml .mli
.PHONY : all install remove clean cleanall cleandoc doc
.DELETE_ON_ERROR:.depend META

all: sawja.cma sawja.cmxa $(SHARED)

META: META.source ../Makefile.config
	if [ "$(BUDDY)" = "yes" ]; then \
		$(SED) 's/requires = "javalib"/requires = "javalib,buddy"/' $< > $@ ; \
	else \
		$(SED) 's/requires = "javalib"/requires = "javalib,ptrees"/' $< > $@ ; \
	fi

install: META sawja.cma sawja.cmxa $(MODULE_INTERFACES:=.mli) sawja_pack.cmi sawja_pack.cmo sawja_pack.cmx sawja_pack.o 
	$(INSTALL) sawja $^ bir.cmi sawja.a

remove:
	$(FINDER) remove sawja

ocaml:sawja.cma
	$(OCAMLMKTOP) $(INCLUDE) -linkpkg -o $@ sawja.cma

.depend:$(MODULE_INTERFACES:=.mli) $(MODULES:=.ml)
	$(OCAMLDEP) -I $(DATAFLOW) $(patsubst jNativeStubs.%,,$^) > $@
	$(OCAMLDEP) $(PP) jNativeStubs.ml jNativeStubs.mli >> $@

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
	$(OCAMLOPT) $(INCLUDE) $(PP) $(FOR_PACK) -c $<

classDomain.ml:$(CLASS_DOMAIN)
	cp $< $@

jPrintHtml.cmo:jPrintHtml.ml
	$(OCAMLC) $(INCLUDE) -pp "$(PERL) -p -e 'BEGIN{open(CSS,$(CSS));\$$css=\"\\\"\\\"\";while(<CSS>){chomp(\$$_);\$$css=\"\$$css^\\n\\\"\$$_\\\\n\\\"\";} print \"# 1 \\\"$<\\\"\n\";}s/\\\"\(\* CSS \*\)\\\"/\$$css/;'" -c $<

jPrintHtml.cmx jPrintHtml.o:jPrintHtml.ml
	$(OCAMLOPT) $(INCLUDE) -pp "$(PERL) -p -e 'BEGIN{open(CSS,$(CSS));\$$css=\"\\\"\\\"\";while(<CSS>){chomp(\$$_);\$$css=\"\$$css^\\n\\\"\$$_\\\\n\\\"\";} print \"# 1 \\\"$<\\\"\n\";}s/\\\"\(\* CSS \*\)\\\"/\$$css/;'" $(FOR_PACK) -c $<

safe.ml: safe_domain.ml safe_var.ml safe_state.ml safe_constraints.ml safe_solver.ml
	$(RM) $@
	for i in $^; do \
	  echo `basename $$i .ml` | \
	    awk 'BEGIN{ORS="";OFS="";}{print "module " toupper(substr($$0,6,1)) substr($$0,7,length($$0)-6) "= struct\n";}' >> $@;\
	  echo "# 1 \""$$i"\"" >> $@;\
	  cat $$i >> $@;\
	  echo " end">> $@;\
	done

%.ml: %.mll
	ocamllex $^	

%.ml %.mli: %.mly
	ocamlyacc $^

.ml.cmo:
	$(OCAMLC) $(INCLUDE) -c $<
%.cmx %.o:%.ml
	$(OCAMLOPT) $(INCLUDE) $(FOR_PACK) -c $<
.mli.cmi:
	$(OCAMLC) $(INCLUDE) -c $<

sawja.cma: sawja_pack.cmo
	$(OCAMLC) -a -o $@ $^

sawja.cmxa: sawja_pack.cmx
	$(OCAMLOPT) -a -o $@ $^

# sawja.cmxs: sawja.cmxa
# 	$(OCAMLOPT) -shared -o $@ $^

sawja_pack.cmo: $(MODULE_INTERFACES:=.cmi) $(MODULES:=.cmo)
	$(OCAMLC) -pack $(MODULES:=.cmo) -o $@

sawja_pack.cmx: $(MODULE_INTERFACES:=.cmi) $(MODULES:=.cmx)
	$(OCAMLOPT) -pack $(MODULES:=.cmx) -o $@

doc: $(MODULE_INTERFACES_DOC:=.cmi) ../intro.ocamldoc
	cp $(JAVALIB)/javalib.mli ./javalib_pack.Javalib.mli
	cp $(JAVALIB)/jBasics.mli ./javalib_pack.JBasics.mli
	cp $(JAVALIB)/jCode.mli ./javalib_pack.JCode.mli
	$(SED) -i '0,/^/s/^/open Javalib_pack\n/' ./javalib_pack.Javalib.mli
	$(SED) -i '0,/^/s/^/open Javalib_pack\n/' ./javalib_pack.JBasics.mli
	$(SED) -i '0,/^/s/^/open Javalib_pack\n/' ./javalib_pack.JCode.mli
	mkdir -p $(DOCDIR)
	$(OCAMLDOC) $(HIDE_MOD_IN_DOC) $(RECODE) $(INCLUDE) -d $(DOCDIR) -colorize-code -t Sawja	\
		-html -stars -intro ../intro.ocamldoc $(MODULE_INTERFACES_DOC:=.mli) javalib_pack.Javalib.mli javalib_pack.JBasics.mli javalib_pack.JCode.mli
	echo "</body></html>" >> $(DOCDIR)/index.html # workaround for a bug in ocamldoc
	rm ./javalib_pack.Javalib.mli ./javalib_pack.JBasics.mli ./javalib_pack.JCode.mli

doc-devel: $(MODULE_INTERFACES_DOC:=.cmi) ../intro.ocamldoc
	$(SED) -i 's/(\*\*\/\*\*)/(\*\*\*\/\*\*\*)/g' *.mli
	cp $(JAVALIB)/javalib.mli ./javalib_pack.Javalib.mli
	cp $(JAVALIB)/jBasics.mli ./javalib_pack.JBasics.mli
	cp $(JAVALIB)/jCode.mli ./javalib_pack.JCode.mli
	$(SED) -i '0,/^/s/^/open Javalib_pack\n/' ./javalib_pack.Javalib.mli
	$(SED) -i '0,/^/s/^/open Javalib_pack\n/' ./javalib_pack.JBasics.mli
	$(SED) -i '0,/^/s/^/open Javalib_pack\n/' ./javalib_pack.JCode.mli
	mkdir -p $(DOCDIR)
	$(OCAMLDOC) $(HIDE_MOD_IN_DOC) $(RECODE) $(INCLUDE) -d $(DOCDIR) -colorize-code -t Sawja	\
		-html -stars -intro ../intro.ocamldoc $(MODULE_INTERFACES:=.mli) javalib_pack.Javalib.mli javalib_pack.JBasics.mli javalib_pack.JCode.mli
	echo "</body></html>" >> $(DOCDIR)/index.html # workaround for a bug in ocamldoc
	rm ./javalib_pack.Javalib.mli ./javalib_pack.JBasics.mli ./javalib_pack.JCode.mli
	$(SED) -i 's/(\*\*\*\/\*\*\*)/(\*\*\/\*\*)/g' *.mli

clean:
	$(RM) safe.ml classDomain.ml .depend *.cmi *.cmo *.cmx *.annot *.obj *.o *~ \
	  $(DATAFLOW:=*.cm[iox]) $(DATAFLOW:=*.o) $(DATAFLOW:=*.obj) $(DATAFLOW:=*.annot) $(DATAFLOW)*.~ \
	  $(HEAP_PARSER:=*.cm[iox]) $(HEAP_PARSER:=*.o) $(HEAP_PARSER:=*.annot) $(HEAP_PARSER)lex_heap.ml \
	  $(HEAP_PARSER)parse_heap.ml $(HEAP_PARSER)parse_heap.mli

cleanall: clean
	$(RM) ocaml *.cma *.cmxa *.cmxs *.a *.cmi META $(DATAFLOW)*.cmi
cleandoc:
	$(RM) -r $(DOCDIR)
