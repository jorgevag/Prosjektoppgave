#!/bin/bash

#try to plot data from the different reports
./joiPlot.py -i ../Kang2012/reDielectricFunctionVO2.csv,0,1,0,2,0,3,0,4,0,5,0,6,0,7,0,8,0,9
./joiPlot.py -i ../Kang2012/imDielectricFunctionVO2.csv,0,1,0,2,0,3,0,4,0,5,0,6,0,7,0,8,0,9

./joiPlot.py -i ../Lappalainen2009/reIndexOfRefractionVO2.csv,0,1,0,2
./joiPlot.py -i ../Lappalainen2009/imIndexOfRefractionVO2.csv,0,1,0,2

./joiPlot.py -i ../Guinneton2004/reOpticalIndexVO2.csv,0,1,0,2
./joiPlot.py -i ../Guinneton2004/imOpticalIndexVO2.csv,0,1,0,2

./joiPlot.py -i ../Bemkahoul2011/nRefractiveIndex.csv,0,1,0,2
./joiPlot.py -i ../Bemkahoul2011/kExtictCoeff.csv,0,1,0,2
