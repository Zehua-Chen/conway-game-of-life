# Generated Makefile

all: bin/index.pdf

.PHONY: clean
clean:
	rm -rf bin

bin:
	mkdir bin

bin/index.pdf: index.tex | bin
	pdflatex -output-directory=bin -interaction=batchmode index.tex
	pdflatex -output-directory=bin -interaction=batchmode index.tex

