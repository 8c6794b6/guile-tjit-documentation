gv = overview.gv.svg overview-01.gv.svg overview-02.gv.svg overview-03.gv.svg

plot = hist.plot.svg dist.plot.svg

all: $(gv) $(plot)

%.gv.svg: %.gv
	dot -Tsvg $< -o $@

%.plot.svg: %.plot
	gnuplot -e 'outfile="$@"' $<

clean:
	rm -rf $(gv) $(plot)
