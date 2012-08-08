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

.PHONY:all buddycaml sawja install clean cleanall cleandoc doc

all:sawja

sawja:
	$(MAKE) -C src

# Package-specific targets
buddycaml installbuddycaml removebuddycaml:%buddycaml:
	$(MAKE) -C buddycaml $*

install remove:
	$(MAKE) -C src $@

distclean:clean
	$(RM) Makefile.config

clean cleanall :
	$(MAKE) -C src $@
	$(MAKE) -C doc $@
	$(MAKE) -C buddycaml $@
	$(RM) *~

cleandoc doc doc-devel:
	$(MAKE) -C src $@
	$(RM) *~

# Documentation for release (generate INSTALL and README)
cleandocr docr:
	$(MAKE) -C doc $@
