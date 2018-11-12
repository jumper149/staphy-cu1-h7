#!/bin/bash

chez-scheme --script "calc-force.ss"

gnuplot -p "plot-L-force.gp"
