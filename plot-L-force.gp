#!/bin/gnuplot -p

set term postscript eps enhanced color
set output "plot-L-force.eps"
set xlabel "L"
set ylabel "Force"
plot "table-3-L-force" with lines, "table-5-L-force" with lines, "table-8-L-force" with lines
