# -*- mode: gnuplot -*-
# set title "Total time (Normalized to Guile VM regular)"
unset title
# set title "Benchmark results"
# set ylabel "Total time normalized to Guile regular VM (smaller is better)" \
#   offset 2
set ylabel "Total time normalized to Guile VM-regular" #  offset 3
# unset ylabel
unset xlabel
# set key vertical left noautotitle
unset key
# set linetype 1 lc rgb "#FF00FF" lw 1 pt 3
# set linetype 2 lc rgb "#00FFFF" lw 1 pt 3
set linetype 3 lc rgb "#e69f00" lw 1 pt 3
set label 1 'Nash'   at graph 0.02, 0.85 left rotate by 90
set label 2 'Racket' at graph 0.035, 0.85 left rotate by 90
set label 3 'Pycket' at graph 0.050, 0.85 left rotate by 90
set object 1 rect front fc lt 1 at graph 0.02, 0.81 size 0.2,0.1
set object 2 rect front fc lt 2 at graph 0.035, 0.81 size 0.2,0.1
set object 3 rect front fc lt 3 at graph 0.050, 0.81 size 0.2,0.1
# set terminal svg enhanced size 1838 820 font "Arial,16"
# set terminal svg enhanced size 1838 820 font "Arial,20"
# set terminal postscript eps enhanced size 1838,820 font "Helvetica,20"
set terminal postscript eps enhanced color size 15,6 font "Helvetica,25"
set style data histogram
set style histogram cluster gap 2
set style fill solid 1.0 border -1
# set style fill solid 1.0
# set lmargin 3
# set yrange [0:3.5]
set yrange [0:2]
# set logscale y
set border 31 linewidth 2
set grid y linestyle 0 lw 2
# set boxwidth 0.9
set tics scale 0.0
# set xtic border in scale 0,0 rotate by -45 offset character -1.2,0
# set xtic rotate by -45 out offset character -1.2,0
set xtic rotate out
set ytic rotate out # offset character 1,0
# set xtic rotate out
if (!exists("outfile")) outfile='bench.eps'
set output outfile
plot for [col=2:4] "bench.dat" using col:xticlabel(1) title columnheader
