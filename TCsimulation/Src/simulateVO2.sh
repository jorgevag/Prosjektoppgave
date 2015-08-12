#!/bin/bash

#for VARIABLE in vo2_1.nk vo2_2.nk vo2_3.nk
#do
   #echo $VARIABLE
#done

#for nk_data in vo2_RT.nk vo2_300K.nk vo2_30C.nk vo2_40C.nk vo2_60C.nk vo2_65C.nk vo2_67C.nk vo2_69C.nk vo2_72C.nk vo2_75C.nk vo2_80C.nk vo2_100C.nk vo2_380K.nk
#do
   #echo $nk_data, ${nk_data%.*},  ${nk_data%.*}.dat 
#done








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


#./SpecificPlots/plotGFRefl.py -i ../Output/vo2_RT.dat_RefTrans,../Output/vo2_300K.dat_RefTrans,../Output/vo2_30C.dat_RefTrans,../Output/vo2_40C.dat_RefTrans,../Output/vo2_60C.dat_RefTrans,../Output/vo2_65C.dat_RefTrans,../Output/vo2_67C.dat_RefTrans,../Output/vo2_69C.dat_RefTrans,../Output/vo2_72C.dat_RefTrans,../Output/vo2_75C.dat_RefTrans,../Output/vo2_80C.dat_RefTrans,../Output/vo2_100C.dat_RefTrans,../Output/vo2_380K.dat_RefTrans -R -p
