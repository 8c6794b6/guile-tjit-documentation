# -*- mode: gnuplot -*-
#
# GNUplot script to plot histogram of Guile tjit and interpreter benchmark
# results.

# set terminal canvas enhanced size 1560,520 font "Helvetica,12"
set terminal postscript eps enhanced color size 5.8,2.4 font "Helvetica,11"
if (!exists("outfile")) outfile='hist.eps'
set output outfile

# set title "Total time normalized to VM-regular (Log scale)"
unset title
set ylabel "Total time normalized to VM-regular (log scale)"

# set key spacing 2
unset key
# set key noautotitle horizontal samplen 0.025
set style data histogram
set style histogram cluster gap 1
set style fill solid 0.25

# set label 1 'Nash' at graph 0.945,0.82 left rotate by 90 font "Helvetica,10"
# set label 2 'Interp' at graph 0.98,0.82 left rotate by 90 font "Helvetica,10"

set label 1 'Nash' at graph 0.02, 0.77 left rotate by 90 font "Helvetica,10"
set object 1 rectangle front fc lt 1 fs solid 0.25 \
at graph 0.02, 0.92 size 0.25,0.5
set label 2 'Interp' at graph 0.04, 0.77 left rotate by 90 font "Helvetica,10"
set object 2 rectangle front fc lt 2 fs solid 0.25 \
at graph 0.04, 0.92 size 0.25,0.5

set logscale y
set grid y linestyle 0 lw 1
# set xtics nomirror rotate by 90
set xtics rotate nomirror
set ytics rotate

plot for [col=2:3] "hist.dat" using col:xticlabel(1) title columnheader
