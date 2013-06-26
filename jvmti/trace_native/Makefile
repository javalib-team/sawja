# This file is part of SAWJA
# Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
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

-include ../../Makefile.config

JDK_PATH=$(JAVA_HOME)

INC=$(INCLUDE) -package sawja

LIBS=.libs
CAMLLIBS=zip.cma extLib.cma ptrees.cma javalib.cma sawja.cma

INCLUDES=-I ${JDK_PATH}/include -I ${JDK_PATH}/include/linux


all: NativeStubsGen.cmo NativeStubsGen.cmx

tracer: $(LIBS)/libCallsTracer.so

$(LIBS)/libCallsTracer.so: CallsTracer.c
	if [ ! -d $(LIBS) ]; then mkdir $(LIBS); fi
	libtool --mode=compile gcc -c CallsTracer.c ${INCLUDES} -o ${LIBS}/CallsTracer.lo
	libtool --mode=link gcc -rpath ${PWD} ${INCLUDES} ${LIBS}/CallsTracer.lo -o libCallsTracer.la
# sous MacOS, la ligne suivante semble donner des résultats un peu
# meilleur (i.e. ça va plus loin dans la compilation)
# gcc -o CallsTracer.so -c ${INCLUDES} CallsTracer.c \
#   && ld -o libCallsTracer.so -arch i386 -dylib -undefined dynamic_lookup -ldylib1.o CallsTracer.o
stubs: NativeStubsGen.cmo NativeStubsGen.cmx

NativeStubsGen.cmo: NativeStubsGen.ml NativeStubsGen.mli
	$(OCAMLC) $(INC) $(PP) -linkpkg -o NativeStubsGen.cmo $(CAMLLIBS) NativeStubsGen.mli NativeStubsGen.ml 

NativeStubsGen.cmx: NativeStubsGen.ml NativeStubsGen.mli
	$(OCAMLOPT) $(INC) $(PP) -linkpkg -o NativeStubsGen.cmx $(CAMLLIBS:.cma=.cmxa) NativeStubsGen.mli NativeStubsGen.ml 

clean:
	rm -rf $(LIBS) *.la *.cm* *.o
