# This file is part of SAWJA
# Copyright (c)2013 Pierre Vittet (INRIA)
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

JDK_PATH=$(JAVA_HOME)

INCLUDES=-I ${JDK_PATH}/include -I ${JDK_PATH}/include/linux
GCC=gcc

.PHONY: all run java inspector

all: java inspector

java: sawja/HeapInit.class
inspector:HeapInspector.so

sawja/HeapInit.class: sawja/HeapInit.java
	@javac $^ 

HeapInspector.so: HeapInspector.c
	@$(GCC) -O2 -Wall -fPIC -c $^ ${INCLUDES} -o HeapInspector.o
	@$(GCC) -O2 -shared ${INCLUDES} HeapInspector.o -o libHeapInspector.so
run: java inspector
	@java -classpath . -agentlib:HeapInspector=sawja.HeapInit sawja.HeapInit

clean: 
	rm -f *.o
	rm -f *.so

