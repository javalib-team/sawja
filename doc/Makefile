# This file is part of Sawja
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

DEST=..

docr: $(DEST)/INSTALL $(DEST)/README
	$(MAKE) -C tutorial release

all:docr

.DELETE_ON_ERROR:$(DEST)/INSTALL $(DEST)/README

$(DEST)/INSTALL:install.html
	lynx -dump $< > $@

$(DEST)/README:intro.html aboutversion.html history.html documentation.html demonstration.html contacts.html
	echo "" > $@
	for i in $^ ; do lynx -dump "$$i" >> $@ ;done

clean:
	rm -f *~

cleandocr:clean
	rm -f $(DEST)/INSTALL $(DEST)/README

cleanall:clean
