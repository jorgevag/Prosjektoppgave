#!/bin/bash

# Reproducing Graphs in Figure 9:

# Run Simulation
./GranFilm -p Figure3/input3.sif -o Figure3/3.dat
# Plot result
./myGranPlot.py 1 Figure3/3.dat -t 3 4
./myGranPlot.py 1 Figure3/3.dat -t 5 6

