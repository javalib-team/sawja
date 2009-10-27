# This is the tutorial Makefile
# You need to install pandoc

all: jaws_tutorial.tex jaws_tutorial.pdf

jaws_tutorial.tex: jaws_tutorial.rst
	cat header.tex > jaws_tutorial.tex
	pandoc -R -f rst -t latex -o jaws_tutorial_part.tex jaws_tutorial.rst
	cat jaws_tutorial_part.tex >> jaws_tutorial.tex
	cat footer.tex >> jaws_tutorial.tex
	rm jaws_tutorial_part.tex

jaws_tutorial.pdf: jaws_tutorial.tex
	pdflatex jaws_tutorial.tex
	pdflatex jaws_tutorial.tex

clean:
	rm -f jaws_tutorial.tex jaws_tutorial.pdf