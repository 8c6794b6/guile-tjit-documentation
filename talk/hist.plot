# -*- mode: gnuplot -*-
#
# GNUplot script to plot histogram of Guile tjit and interpreter benchmark
# results.

# set terminal canvas enhanced size 1560,520 font "Helvetica,12"
set terminal svg enhanced size 1600,400 font "Helvetica,20"
if (!exists("outfile")) outfile='hist.svg'
set output outfile

# set title "Total time normalized to VM-regular (Log scale)"
# set title "Total time normalized to VM-regular (log scale)"
unset title
unset ylabel

# set key spacing 2
set key spacing 2 left
# set key noautotitle horizontal samplen 0.025
set style data histogram
set style histogram cluster gap 1
set style fill solid 0.25

# set label 1 'Nash' at graph 0.945,0.82 left rotate by 90 font "Helvetica,10"
# set label 2 'Interp' at graph 0.98,0.82 left rotate by 90 font "Helvetica,10"

# set label 1 'Nash' at graph 0.02, 0.77 left rotate by 90 font "Helvetica,10"
# set object 1 rectangle front fc lt 1 fs solid 0.25 \
# at graph 0.02, 0.92 size 0.25,0.5
# set label 2 'Interp' at graph 0.04, 0.77 left rotate by 90 font "Helvetica,10"
# set object 2 rectangle front fc lt 2 fs solid 0.25 \
# at graph 0.04, 0.92 size 0.25,0.5

# set logscale y
set grid y linestyle 3 lw 1
# set xtics nomirror rotate by 90
set xtics rotate by -45 nomirror offset -1
set ytics

plot for [col=2:2] "../hist.dat" using col:xticlabel(1) title columnheader
