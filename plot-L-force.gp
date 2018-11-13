#!/bin/gnuplot -p

set term postscript eps enhanced color
set output "out/plot-L-force.eps"
set xlabel "L"
set ylabel "Force"
set xrange [0:8]
set yrange [-10:10]
plot "out/table-3-L-force" with lines title "N = 3", \
     "out/table-5-L-force" with lines title "N = 5", \
     "out/table-8-L-force" with lines title "N = 8"
