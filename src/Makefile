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
DOCDIR:=../$(DOCDIR)
DATAFLOW=dataflow_analyses/
INCLUDE:=$(INCLUDE) -I $(DATAFLOW)

CSS=\"style.css\"

ifeq ($(BUDDY),yes)
CLASS_DOMAIN = classDomainBDD.ml
else
CLASS_DOMAIN = classDomainPtrees.ml
endif

MODULES = $(DATAFLOW)iter jUtil wlist \
	jProgram classDomain jsrInline cmn bir jBir a3Bir \
	$(DATAFLOW)live_bir $(DATAFLOW)live_a3bir \
	ssaBir jBirSSA a3BirSSA \
	jControlFlow jPrintUtil jPrintHtml jPrintPlugin argPlugin\
	safe reachableMethods jNativeStubs \
	jCRA jRTA jRRTA XTA \
	$(DATAFLOW)reachDef $(DATAFLOW)availableExpr \
	$(DATAFLOW)availableExprSSA

MODULE_INTERFACES = $(DATAFLOW)iter wlist \
	jProgram classDomain jNativeStubs jsrInline \
	cmn jBir a3Bir ssaBir jBirSSA a3BirSSA\
	$(DATAFLOW)live_bir \
	jControlFlow jPrintUtil jPrintHtml jPrintPlugin argPlugin\
	safe reachableMethods \
	jCRA XTA jRTA jRRTA


MODULE_INTERFACES_DOC = $(filter-out cmn ssaBir jPrintUtil ,$(MODULE_INTERFACES))
HIDE_MOD_IN_DOC = -hide JPrintPlugin.NewCodePrinter

FOR_PACK= -for-pack Sawja_pack

.SUFFIXES : .cmo .cmx .cmi .ml .mli
.PHONY : all install remove clean cleanall cleandoc doc
.DELETE_ON_ERROR:.depend META

all: sawja.cma sawja.cmxa $(SHARED)

META: META.source ../Makefile.config
	if [ "$(BUDDY)" = "yes" ]; then \
		sed 's/requires = "javalib"/requires = "javalib,buddy"/' $< > $@ ; \
	else \
		sed 's/requires = "javalib"/requires = "javalib,ptrees"/' $< > $@ ; \
	fi

install: META sawja.cma sawja.cmxa $(MODULE_INTERFACES:=.mli) sawja_pack.cmi sawja_pack.cmo sawja_pack.cmx sawja_pack.o 
	$(INSTALL) sawja $^ bir.cmi sawja.a

remove:
	ocamlfind remove sawja

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
	sed -i '0,/^/s/^/open Javalib_pack\n/' ./javalib_pack.Javalib.mli
	sed -i '0,/^/s/^/open Javalib_pack\n/' ./javalib_pack.JBasics.mli
	sed -i '0,/^/s/^/open Javalib_pack\n/' ./javalib_pack.JCode.mli
	mkdir -p $(DOCDIR)
	$(OCAMLDOC) $(HIDE_MOD_IN_DOC) $(RECODE) $(INCLUDE) -d $(DOCDIR) -colorize-code -t Sawja	\
		-html -stars -intro ../intro.ocamldoc $(MODULE_INTERFACES_DOC:=.mli) javalib_pack.Javalib.mli javalib_pack.JBasics.mli javalib_pack.JCode.mli
	echo "</body></html>" >> $(DOCDIR)/index.html # workarround for a bug in ocamldoc
	rm ./javalib_pack.Javalib.mli ./javalib_pack.JBasics.mli ./javalib_pack.JCode.mli

doc-devel: $(MODULE_INTERFACES_DOC:=.cmi) ../intro.ocamldoc
	sed -i 's/(\*\*\/\*\*)/(\*\*\*\/\*\*\*)/g' *.mli
	cp $(JAVALIB)/javalib.mli ./javalib_pack.Javalib.mli
	cp $(JAVALIB)/jBasics.mli ./javalib_pack.JBasics.mli
	cp $(JAVALIB)/jCode.mli ./javalib_pack.JCode.mli
	sed -i '0,/^/s/^/open Javalib_pack\n/' ./javalib_pack.Javalib.mli
	sed -i '0,/^/s/^/open Javalib_pack\n/' ./javalib_pack.JBasics.mli
	sed -i '0,/^/s/^/open Javalib_pack\n/' ./javalib_pack.JCode.mli
	mkdir -p $(DOCDIR)
	$(OCAMLDOC) $(HIDE_MOD_IN_DOC) $(RECODE) $(INCLUDE) -d $(DOCDIR) -colorize-code -t Sawja	\
		-html -stars -intro ../intro.ocamldoc $(MODULE_INTERFACES:=.mli) javalib_pack.Javalib.mli javalib_pack.JBasics.mli javalib_pack.JCode.mli
	echo "</body></html>" >> $(DOCDIR)/index.html # workarround for a bug in ocamldoc
	rm ./javalib_pack.Javalib.mli ./javalib_pack.JBasics.mli ./javalib_pack.JCode.mli
	sed -i 's/(\*\*\*\/\*\*\*)/(\*\*\/\*\*)/g' *.mli

clean:
	$(RM) safe.ml classDomain.ml .depend *.cmi *.cmo *.cmx *.annot *.obj *.o *~ \
	  $(DATAFLOW)*.{cmo,cmx,annot,o,obj} $(DATAFLOW)*.~
cleanall: clean
	$(RM) ocaml *.cma *.cmxa *.cmxs *.a *.cmi META $(DATAFLOW)*.cmi
cleandoc:
	$(RM) -r $(DOCDIR)
