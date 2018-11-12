#!/bin/bash

echo "starting calculation"
chez-scheme --script "calc-force.ss"

echo "starting gnuplot"
gnuplot -p "plot-L-force.gp"

echo "starting LaTeX"
cd tex
pdflatex main.tex
cd ..
