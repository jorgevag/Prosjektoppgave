#!/bin/bash

## Plotting the data from Digitizer from all the Reports/Articles:
#./joiPlotThree.py -i ../VO2/Kang2012/reEpsVO2.csv,s=1,d=comma,0,1,0,2,0,3,0,4,0,5,0,6,0,7,0,8,0,9
#./joiPlotThree.py -i ../VO2/Kang2012/imEpsVO2.csv,s=1,d=comma,0,1,0,2,0,3,0,4,0,5,0,6,0,7,0,8,0,9

#./joiPlotThree.py -i ../VO2/Lappalainen2009/reIndexOfRefractionVO2.csv,s=1,d=comma,0,1,0,2
#./joiPlotThree.py -i ../VO2/Lappalainen2009/imIndexOfRefractionVO2.csv,s=1,d=comma,0,1,0,2

#./joiPlotThree.py -i ../VO2/Guinneton2004/nVO2.csv,s=1,d=comma,0,1,0,2
#./joiPlotThree.py -i ../VO2/Guinneton2004/kVO2.csv,s=1,d=comma,0,1,0,2

#./joiPlotThree.py -i ../VO2/Benkahoul2011/nVO2.csv,s=1,d=comma,0,1,0,2
#./joiPlotThree.py -i ../VO2/Benkahoul2011/kVO2.csv,s=1,d=comma,0,1,0,2


# Converting the data: 
##   Converting data from Kang 2012:
#./convertData.py -i ../VO2/Kang2012/reEpsVO2.csv,../VO2/Kang2012/imEpsVO2.csv,s=1,d=comma,unit=eV,isPermittivity=true,0,1,0,1 -o ../VO2/DataBase/vo2_RT.nk
#./convertData.py -i ../VO2/Kang2012/reEpsVO2.csv,../VO2/Kang2012/imEpsVO2.csv,s=1,d=comma,unit=eV,isPermittivity=true,0,2,0,2 -o ../VO2/DataBase/vo2_40C.nk
#./convertData.py -i ../VO2/Kang2012/reEpsVO2.csv,../VO2/Kang2012/imEpsVO2.csv,s=1,d=comma,unit=eV,isPermittivity=true,0,3,0,3 -o ../VO2/DataBase/vo2_60C.nk
#./convertData.py -i ../VO2/Kang2012/reEpsVO2.csv,../VO2/Kang2012/imEpsVO2.csv,s=1,d=comma,unit=eV,isPermittivity=true,0,4,0,4 -o ../VO2/DataBase/vo2_65C.nk
#./convertData.py -i ../VO2/Kang2012/reEpsVO2.csv,../VO2/Kang2012/imEpsVO2.csv,s=1,d=comma,unit=eV,isPermittivity=true,0,5,0,5 -o ../VO2/DataBase/vo2_67C.nk
#./convertData.py -i ../VO2/Kang2012/reEpsVO2.csv,../VO2/Kang2012/imEpsVO2.csv,s=1,d=comma,unit=eV,isPermittivity=true,0,6,0,6 -o ../VO2/DataBase/vo2_69C.nk
#./convertData.py -i ../VO2/Kang2012/reEpsVO2.csv,../VO2/Kang2012/imEpsVO2.csv,s=1,d=comma,unit=eV,isPermittivity=true,0,7,0,7 -o ../VO2/DataBase/vo2_72C.nk
#./convertData.py -i ../VO2/Kang2012/reEpsVO2.csv,../VO2/Kang2012/imEpsVO2.csv,s=1,d=comma,unit=eV,isPermittivity=true,0,8,0,8 -o ../VO2/DataBase/vo2_75C.nk
#./convertData.py -i ../VO2/Kang2012/reEpsVO2.csv,../VO2/Kang2012/imEpsVO2.csv,s=1,d=comma,unit=eV,isPermittivity=true,0,9,0,9 -o ../VO2/DataBase/vo2_80C.nk

##   Converting data from Guinneton 2004:
#./convertData.py -i ../VO2/Guinneton2004/nVO2.csv,../VO2/Guinneton2004/kVO2.csv,s=1,d=comma,unit=m,isPermittivity=false,0,1,0,1 -o ../VO2/DataBase/vo2_300K.nk
#./convertData.py -i ../VO2/Guinneton2004/nVO2.csv,../VO2/Guinneton2004/kVO2.csv,s=1,d=comma,unit=m,isPermittivity=false,0,2,0,2 -o ../VO2/DataBase/vo2_380K.nk

##   Converting data from Benkahoul 2011:
#./convertData.py -i ../VO2/Benkahoul2011/nVO2.csv,../VO2/Benkahoul2011/kVO2.csv,s=1,d=comma,unit=m,isPermittivity=false,0,1,0,1 -o ../VO2/DataBase/vo2_30C.nk
#./convertData.py -i ../VO2/Benkahoul2011/nVO2.csv,../VO2/Benkahoul2011/kVO2.csv,s=1,d=comma,unit=m,isPermittivity=false,0,2,0,2 -o ../VO2/DataBase/vo2_100C.nk
