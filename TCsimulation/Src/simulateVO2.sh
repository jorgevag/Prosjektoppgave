#!/bin/bash

#Test code:
#for VARIABLE in vo2_1.nk vo2_2.nk vo2_3.nk
#do
   #echo $VARIABLE
#done

#for nk_data in vo2_RT.nk vo2_300K.nk vo2_30C.nk vo2_40C.nk vo2_60C.nk vo2_65C.nk vo2_67C.nk vo2_69C.nk vo2_72C.nk vo2_75C.nk vo2_80C.nk vo2_100C.nk vo2_380K.nk
#do
   #echo $nk_data, ${nk_data%.*},  ${nk_data%.*}.dat 
#done






## Simulate and plot for all data
#for nk_data in vo2_RT.nk vo2_300K.nk vo2_30C.nk vo2_40C.nk vo2_60C.nk vo2_65C.nk vo2_67C.nk vo2_69C.nk vo2_72C.nk vo2_75C.nk vo2_80C.nk vo2_100C.nk vo2_380K.nk
#do
   ## Update input file ('.sif'-file):
   #./updateInput.py -i ../Input/VO2.sif -n ../../ThermochromicData/VO2/DataBase/$nk_data -m VO2 
   #echo ../../ThermochromicData/VO2/DataBase/$nk_data


   ## Run simulation:
   #./GranFilm -py -p ../Input/VO2.sif -o ../Output/${nk_data%.*}.dat
   #echo ../Output/${nk_data%.*}.dat
#done

#for nk_data in vo2_RT.nk # vo2_300K.nk vo2_30C.nk vo2_40C.nk vo2_60C.nk vo2_65C.nk vo2_67C.nk vo2_69C.nk vo2_72C.nk vo2_75C.nk vo2_80C.nk vo2_100C.nk vo2_380K.nk
#do
   ## Plot data:
   #./joiPlotThree.py -i ../Output/${nk_data%.*}.dat,0,1
#done

## Plot R_i = Re(r_i)^2 + Im(r_i)^2 = |r_i|^2 =  (r_i)(r_i*)
#./SpecificPlots/plotRefTrans.py -i ../Output/vo2_RT.dat_RefTrans,../Output/vo2_300K.dat_RefTrans,../Output/vo2_30C.dat_RefTrans,../Output/vo2_40C.dat_RefTrans,../Output/vo2_60C.dat_RefTrans,../Output/vo2_65C.dat_RefTrans,../Output/vo2_67C.dat_RefTrans,../Output/vo2_69C.dat_RefTrans,../Output/vo2_72C.dat_RefTrans,../Output/vo2_75C.dat_RefTrans,../Output/vo2_80C.dat_RefTrans,../Output/vo2_100C.dat_RefTrans,../Output/vo2_380K.dat_RefTrans -R -p



## Running simulation for two Half-infinite media (vacuum/VO2) to check the flat fresnel coefficients for 72 degrees celsius
## in order to check the consistency of the granfilm output.
#for nk_data in vo2_72C.nk
#do
   ## Update input file ('.sif'-file):
   #./updateInput.py -i ../Input/VO2.sif -n ../../ThermochromicData/VO2/DataBase/$nk_data -m VO2 
   #echo ../../ThermochromicData/VO2/DataBase/$nk_data


   ## Run simulation:
   #./GranFilm -py -p ../Input/VO2.sif -o ../Output/${nk_data%.*}.dat
   #echo ../Output/${nk_data%.*}.dat
#done
#./SpecificPlots/testFlatFresnelR.py


## Plot the reflection of the bare SiO2 substrate (doesn't matter which file we use so we just use RT):
#./SpecificPlots/plotBareSubstrate.py -i ../Output/vo2_RT.dat_RefTrans -s -p -R




## Plotting dR/R for data from Kang M et al. 2012, d approx 80nm for square lattice
#for nk_data in vo2_RT.nk vo2_40C.nk vo2_60C.nk vo2_65C.nk vo2_67C.nk vo2_69C.nk vo2_72C.nk vo2_75C.nk vo2_80C.nk
#do
   ## Update input file ('.sif'-file):
   #./updateInput.py -i ../Input/80nmVO2.sif -n ../../ThermochromicData/VO2/DataBase/$nk_data -m VO2 
   #echo ../../ThermochromicData/VO2/DataBase/$nk_data


   ## Run simulation:
   #./GranFilm -py -p ../Input/80nmVO2.sif -o ../Output/${nk_data%.*}.dat
   #echo ../Output/${nk_data%.*}.dat
#done

#./SpecificPlots/plotdR.py -i ../Output/vo2_RT.dat,../Output/vo2_40C.dat,../Output/vo2_60C.dat,../Output/vo2_65C.dat,../Output/vo2_67C.dat,../Output/vo2_69C.dat,../Output/vo2_72C.dat,../Output/vo2_75C.dat,../Output/vo2_80C.dat




# Plotting dR/R for data from Kang M et al. 2012, for radius from 10-15nm on glass substrate (SiO2)
for nk_data in vo2_RT.nk vo2_40C.nk vo2_60C.nk vo2_65C.nk vo2_67C.nk vo2_69C.nk vo2_72C.nk vo2_75C.nk vo2_80C.nk
do
   # Update input file ('.sif'-file):
   ./updateInput.py -i ../Input/VO2SiO2.sif -n ../../ThermochromicData/VO2/DataBase/$nk_data -m VO2 
   echo ../../ThermochromicData/VO2/DataBase/$nk_data

   # Run simulation:
   ./GranFilm -py -p ../Input/VO2SiO2.sif -o ../Output/${nk_data%.*}.dat
   echo ../Output/${nk_data%.*}.dat
done

./SpecificPlots/plotdR.py -i ../Output/vo2_RT.dat,../Output/vo2_40C.dat,../Output/vo2_60C.dat,../Output/vo2_65C.dat,../Output/vo2_67C.dat,../Output/vo2_69C.dat,../Output/vo2_72C.dat,../Output/vo2_75C.dat,../Output/vo2_80C.dat


