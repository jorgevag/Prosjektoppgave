#!/bin/bash

# Run Simulation
#./GranFilm -p InputOutput/waterOnGlass.sif -o InputOutput/waterOnGlass.dat
#./GranFilm -p InputOutput/VO2OnGlass300K.sif -o InputOutput/VO2OnGlass300K.dat #Doesn't work because data isn't overlapping

./GranFilm -p InputOutput/VO2vacuum300K.sif -o InputOutput/VO2vacuum300K.dat
./joiPlotThree.py -i InputOutput/VO2vacuum300K.dat,0,1

