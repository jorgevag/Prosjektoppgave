#!/bin/bash

# Reproducing Graphs in Figure 9:

./GranFilm -py -p Figure9/input9a.sif -o Figure9/9a.dat
#echo simulate 9b  BbBbBbB
./GranFilm -py -p Figure9/input9b.sif -o Figure9/9b.dat
#echo simulate 9c  CcCcCcC
./GranFilm -py -p Figure9/input9c.sif -o Figure9/9c.dat
./plotFigure9.py

#./joiGranPlot.py -i Figure9/9a.dat,0,1 -i Figure9/9a.dat_RefTrans,0,1

