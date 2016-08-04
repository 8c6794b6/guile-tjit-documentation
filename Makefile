base=nash

.PHONY: all clean

all: ${base}.pdf

${base}.pdf: ${base}.tex ${base}.bib overview.eps bench.eps dist.eps hist.eps
	latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" \
	 -use-make ${base}.tex

bench.eps: bench.plot bench.dat
	gnuplot -e "outfile='bench.eps'" bench.plot

dist.eps: dist.plot dist.dat
	gnuplot -e "outfile='dist.eps'" dist.plot

hist.eps: hist.plot hist.dat
	gnuplot -e "outfile='hist.eps'" hist.plot

clean:
	rm -f *.log *.pdf *.aux *.out *.dvi *.fls *.blg *.fdb_latexmk *~

