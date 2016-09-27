# -*- mode: gnuplot -*-
# GNUplot Script to plot distribution data

# set terminal canvas enhanced size 1560,520 font "Helvetica,12"
# set terminal png enhanced size 1400,600
# set terminal postscript eps enhanced color size 5.8,2.4 font "Helvetica,11"
set terminal svg enhanced size 1600,400 font "Helvetica,20"
if (!exists("outfile")) outfile='dist.eps'
set output outfile

set style fill solid 0.25 border 1
# set style boxplot outliers pointtype 7
set style boxplot nooutliers
set style data boxplot

# set title "Standard score of native code compilers"
unset title
set ylabel "Distribution of benchmark results"

# set key off
set key left noautotitle horizontal samplen 0.05 spacing 0.8

set label 1 'Nash' at graph 0.046,0.90 left font "Helvetica,14"

header = "`head -1 ../dist.dat | cut -b 2-`"
set for [i=1:words(header)] xtics (word(header, i) i)
set grid y linestyle 3 lw 1
# set xtics nomirror rotate by -90
set xtics rotate by -45 nomirror offset -1
set ytics

plot for [i=1:words(header)] '../dist.dat' using (i):i notitle, \
     '../dist-nash.dat' using 1 with points pointtype 5 pointsize 1 title ' '

# plot for [i=1:words(header)] '../dist.dat' using (i):i notitle, \
#      '../dist-nash.dat' using 1 with points pointtype 5 pointsize 1 title ' ', \
#      '../dist-pycket.dat' using 1 with points pointtype 7 pointsize 1 lc rgb "#e69f00" title ' '

# plot for [i=1:words(header)] 'dist.dat' using (i):i notitle, \
#      'dist-nash.dat' using 1 with points pointtype 5 pointsize 1 title 'Nash', \
#      'dist-pycket.dat' using 1 with points pointtype 7 pointsize 1 lc rgb "#e69f00" title 'Pycket'
