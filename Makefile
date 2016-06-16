base=guile-tjit

.PHONY: all clean

all: ${base}.pdf

${base}.pdf: ${base}.tex overview.eps bench.eps nash.bib
	latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" \
	 -use-make ${base}.tex

bench.eps: bench.plot bench.dat
	gnuplot -e "outfile='bench.eps'" bench.plot

clean:
	rm -f *.log *.pdf *.aux *.dvi *~

