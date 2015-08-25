#!/usr/bin/env python
import numpy
import scipy.constants as sc
import matplotlib.pyplot as pyplot


# # # # # # # # # # # # # # # # # #  
# # # # # # #   Read nk - data    #  
# # # # # # # # # # # # # # # # # #  
# Read nk data:
#nkfile = open('../../../ThermochromicData/VO2/DataBase/vo2_72C.nk','r')
nkfile = open('../../ThermochromicData/VO2/DataBase/vo2_72C.nk','r')
nkline1 = nkfile.readline()
nkdata = numpy.loadtxt(nkfile) # Load file data
nkfile.close()


# Get 'unit' and energy interval [x1,x2]:
nkinfo = nkline1.split()
#unit = int(nkinfo[0])
x1 = float(nkinfo[1])
x2 = float(nkinfo[2])
points = int(nkinfo[3])

print 'nkline1=',nkline1 
print 'x1=',x1,' ,x2=',x2
x = numpy.linspace(x1,x2,num=points,endpoint=True)
print 'x[0]=',x[0],' ,x[-1]=',x[-1], ' len(x)=',len(x)




# # # # # # # # # # # # # # # # # # #  
# # # # # # #   Theoretical Values  #  
# # # # # # # # # # # # # # # # # # #  
pi = sc.pi
theta_i = pi/4.0 
N1 = 1 + 0j #permittivity of vacuum

print 'nkdata[0,:]=',nkdata[0,:]

N2 = nkdata[:,0].astype(float) + nkdata[:,1].astype(float)*1j
print 'N2[0]',N2[0]
theta_t = numpy.arcsin( (N1/N2)*numpy.sin(theta_i) )

r_s = ( N1*numpy.cos(theta_i) - N2*numpy.cos(theta_t) )/( N1*numpy.cos(theta_i) + N2*numpy.cos(theta_t) )
r_p = ( N2*numpy.cos(theta_i) - N1*numpy.cos(theta_t) )/( N1*numpy.cos(theta_t) + N2*numpy.cos(theta_i) )

R_s = r_s*numpy.conjugate(r_s)
R_p = r_p*numpy.conjugate(r_p)


# # # # # # # # # # # # # # # # # # #  
# # # # # # #   GranFilm  Values    #  
# # # # # # # # # # # # # # # # # # #  
#fileRefTrans = open('../../Output/vo2_72C.dat_RefTrans','r')
fileRefTrans = open('../Output/vo2_72C.dat_RefTrans','r')
rtdata = numpy.loadtxt(fileRefTrans) # Load file data
fileRefTrans.close()



# # # # # # # # # # # # # # # #  
# # # # # # #   Plot Values   #  
# # # # # # # # # # # # # # # #  
# Theoretical Values
pyplot.figure(1)
pyplot.xlabel('eV', fontsize=18)

# Both R_p and R_s only have a real component
pyplot.plot(x, R_p.real, label='$R_p$ $72^{\circ}$C theoretical', color='r')
pyplot.plot(x, R_s.real, label='$R_s$ $72^{\circ}$C theoretical', color='k')
pyplot.legend(loc='best')

# GranFilm Values
pyplot.figure(2)
pyplot.xlabel('eV', fontsize=18)

pyplot.plot(rtdata[:,0], (rtdata[:,3]**2 + rtdata[:,4]**2), label='$R_p$ $72^{\circ}$C GranFilm')
pyplot.plot(rtdata[:,0], (rtdata[:,7]**2 + rtdata[:,8]**2), label='$R_s$ $72^{\circ}$C GranFilm')
pyplot.legend(loc='best')

pyplot.show()
