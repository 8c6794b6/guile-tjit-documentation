base=guile-tjit

.PHONY: all clean

all: ${base}.pdf

${base}.pdf: ${base}.tex ${base}.bib overview.eps bench.eps 
	latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" \
	 -use-make ${base}.tex

bench.eps: bench.plot bench.dat
	gnuplot -e "outfile='bench.eps'" bench.plot

clean:
	rm -f *.log *.pdf *.aux *.out *.dvi *.fls *.blg *.fdb_latexmk *~

