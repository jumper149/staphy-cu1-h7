#!/bin/gnuplot -p

set term postscript eps enhanced color
set output "plot-L-force.eps"
set xlabel "L"
set ylabel "Force"
plot "table-3-L-force" with lines title "N = 3", \
     "table-5-L-force" with lines title "N = 5", \
     "table-8-L-force" with lines title "N = 8"
