# This is the tutorial Makefile
# You need to install pandoc

all: sawja_tutorial.tex sawja_tutorial.pdf

sawja_tutorial.tex: sawja_tutorial.md
	cat header.tex > sawja_tutorial.tex
	pandoc -f markdown -t latex -o sawja_tutorial_part.tex sawja_tutorial.md
	cat sawja_tutorial_part.tex >> sawja_tutorial.tex
	cat footer.tex >> sawja_tutorial.tex
	rm sawja_tutorial_part.tex
	pandoc -f markdown -t html -o sawja_tutorial.html sawja_tutorial.md

sawja_tutorial.pdf: sawja_tutorial.tex
	pdflatex sawja_tutorial.tex
	pdflatex sawja_tutorial.tex

clean:
	rm -f sawja_tutorial.tex sawja_tutorial.pdf