# This file is part of Sawja
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

# This is the tutorial Makefile
# You need to install pandoc

# Just for test on tutorial code
-include ../../Makefile.config

all: sawja_tut.pdf sawja-tut.html

release: sawja_tut.pdf sawja-tut-release.html

.DELETE_ON_ERROR:sawja-tut.html 

sawja-tut.html: sawja_tutorial.md header.html
	pandoc -s --base-header-level 2 -c http://javalib.gforge.inria.fr/style.css \
	-B header.html -A footer.html --toc -T 'Sawja tutorial' -f \
		markdown -t html sawja_tutorial.md | tidy -utf8 -q > sawja-tut.html

sawja-tut-release.html: sawja_tutorial.md header.html
	cp ../../../../site/style.css .
	pandoc -s --base-header-level 2 -c style.css -B header.html  -A footer.html \
	--toc -T 'Sawja tutorial' -f \
		markdown -t html sawja_tutorial.md | tidy -utf8 -q > sawja-tut.html

sawja_tut.pdf: sawja_tutorial.md
	pandoc -T 'Sawja tutorial' --toc header.md sawja_tutorial.md -o sawja-tut.pdf


test: tutorial.ml
	$(OCAMLC) -package sawja -c $^
	@echo "tutorial.ml file is compiling with version of Sawja installed."
	@echo "please check sawja_tutorial.md correspond to the same code."
	rm tutorial.cmi tutorial.cmo tutorial.annot

clean:
	rm -f sawja_tut.pdf sawja-tut.html style.css
