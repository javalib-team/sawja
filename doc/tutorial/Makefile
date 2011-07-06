# This is the tutorial Makefile
# You need to install pandoc

# Just for test on tutorial code
-include ../../Makefile.config

all: sawja_tutorial.tex sawja_tutorial.pdf sawja-tut.html

release: sawja_tutorial.tex sawja_tutorial.pdf sawja-tut-release.html

.DELETE_ON_ERROR:sawja-tut.html sawja_tutorial.tex

sawja_tutorial.tex: sawja_tutorial.md
	cat header.tex > sawja_tutorial.tex
	pandoc -f markdown -t latex -o sawja_tutorial_part.tex sawja_tutorial.md
	cat sawja_tutorial_part.tex >> sawja_tutorial.tex
	cat footer.tex >> sawja_tutorial.tex
	rm sawja_tutorial_part.tex

sawja-tut.html: sawja_tutorial.md header.html
	pandoc -s --base-header-level 2 -c http://javalib.gforge.inria.fr/style.css \
	-B header.html -A footer.html --toc -T 'Sawja tutorial' -f \
		markdown -t html sawja_tutorial.md | tidy -utf8 -q > sawja-tut.html

sawja-tut-release.html: sawja_tutorial.md header.html
	cp ../../../../site/style.css .
	pandoc -s --base-header-level 2 -c style.css -B header.html  -A footer.html \
	--toc -T 'Sawja tutorial' -f \
		markdown -t html sawja_tutorial.md | tidy -utf8 -q > sawja-tut.html

sawja_tutorial.pdf: sawja_tutorial.tex
	pdflatex sawja_tutorial.tex
	pdflatex sawja_tutorial.tex
	rm -f sawja_tutorial.aux 
	rm -f sawja_tutorial.log 
	rm -f sawja_tutorial.toc 
	rm -f sawja_tutorial.out

test: tutorial.ml
	$(OCAMLC) -package sawja -c $^
	@echo "tutorial.ml file is compiling with version of Sawja installed."
	@echo "please check sawja_tutorial.md correspond to the same code."
	rm tutorial.cmi tutorial.cmo tutorial.annot

clean:
	rm -f sawja_tutorial.tex sawja_tutorial.pdf sawja-tut.html style.css