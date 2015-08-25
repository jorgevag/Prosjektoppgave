#!/usr/bin/python
import numpy
import matplotlib.pyplot as pyplot
import scipy.interpolate

import matplotlib
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm

# Load dielectric constant data from files:
reData = numpy.loadtxt('reEpsVO2.csv',skiprows=1,delimiter=',')
imData = numpy.loadtxt('imEpsVO2.csv',skiprows=1,delimiter=',')

T = numpy.array([23., 40., 60.,65.,67.,69.,72.,75.,80.])

E1 = reData[:,0]
E2 = imData[:,0]

Emin= max( E1.min(), E2.min() )
Emax= min( E1.max(), E2.max() )


TT = numpy.linspace(T.min(),T.max(), 100) # temperature interpolation points
EE = numpy.linspace(Emin,Emax, 100) # energy interpolation points for real values
ReEPSILON = numpy.zeros( (len(EE),len(TT)) )
ImEPSILON = numpy.zeros( (len(EE),len(TT)) )


N=T.shape[0]
reEpsilon=numpy.zeros([N])
imEpsilon=numpy.zeros([N])

for e in range(len(EE)):
    # Interpolate the energy values in EE
    for n in range(N):
        # --- real part
        reE = scipy.interpolate.interp1d(E1, reData[:,n+1] )
        reEpsilon[n] = reE( EE[e] )
        # --- imag part
        imE = scipy.interpolate.interp1d(E2, imData[:,n+1] )
        imEpsilon[n] = imE( EE[e] )

    # Interpolate in Temperature
    reE = scipy.interpolate.interp1d(T, reEpsilon )
    imE = scipy.interpolate.interp1d(T, imEpsilon )

    # Interpolate the temperatures in TT
    ReEPSILON[:,e] = reE(TT)
    ImEPSILON[:,e] = imE(TT)


# # # # # # # # # # # # # # # # # 
# # # # # # # # #    Plotting   #  
# # # # # # # # # # # # # # # # #  
# Meshgrid for plotting
EEgrid,TTgrid = numpy.meshgrid(EE,TT)

#fig = pyplot.figure(1)
#ax = Axes3D(fig)
#surf = ax.plot_surface(EEgrid, TTgrid, ReEPSILON, rstride=1, cstride=1, cmap=cm.coolwarm,linewidth=0, antialiased=False)
##ax.set_zlim(-1.01, 1.01)
##from matplotlib.ticker import LinearLocator, FormatStrFormatter
##ax.zaxis.set_major_locator(LinearLocator(10))
##ax.zaxis.set_major_formatter(FormatStrFormatter('%.02f'))
#fig.colorbar(surf, shrink=0.8)#, aspect=5)
#ax.set_xlabel('Energy[eV]', fontsize=16)
#ax.set_ylabel('Temperature[$^{\circ}$C]', fontsize=16)
#ax.set_zlabel('Real Dielectric Constant', fontsize=16)

#fig = pyplot.figure(2)
#ax = Axes3D(fig)
#surf = ax.plot_surface(EEgrid, TTgrid, ImEPSILON, rstride=1, cstride=1, cmap=cm.coolwarm,linewidth=0, antialiased=False)
#fig.colorbar(surf, shrink=0.8)#, aspect=5)
#ax.set_xlabel('Energy[eV]', fontsize=16)
#ax.set_ylabel('Temperature[$^{\circ}$C]', fontsize=16)
#ax.set_zlabel('Imaginary Dielectric Constant', fontsize=16)

#fig = pyplot.figure(3)
#contour = pyplot.imshow(ReEPSILON, cmap=pyplot.get_cmap(cm.coolwarm), origin='lower', interpolation='none', extent = [Emin, Emax, T.min(), T.max()], aspect='auto')
#pyplot.xlabel('Energy[eV]', fontsize=16)
#pyplot.ylabel('Temperature[$^{\circ}$C]', fontsize=16)
#cbar = fig.colorbar(contour) #, shrink=0.5, aspect=5)
#cbar.set_label('Real Dielectric Constant', fontsize=16)

#fig = pyplot.figure(4)
#contour = pyplot.imshow(ImEPSILON, cmap=pyplot.get_cmap(cm.coolwarm), origin='lower', interpolation='none', extent = [Emin, Emax, T.min(), T.max()], aspect='auto')
#pyplot.xlabel('Energy[eV]', fontsize=16)
#pyplot.ylabel('Temperature[$^{\circ}$C]', fontsize=16)
#cbar = fig.colorbar(contour) #, shrink=0.5, aspect=5)
#cbar.set_label('Imaginary Dielectric Constant', fontsize=16)
#pyplot.show()


fig = pyplot.figure(1)
ax = fig.add_subplot(2, 2, 1, projection='3d')
surf = ax.plot_surface(EEgrid, TTgrid, ReEPSILON, rstride=1, cstride=1, cmap=cm.coolwarm,linewidth=0, antialiased=False)
cbar = fig.colorbar(surf, shrink=0.8)#, aspect=5)
cbar.set_label('Real Dielectric Constant', fontsize=16)
ax.set_xlabel('E [eV]', fontsize=16)
ax.set_ylabel('T [$^{\circ}$C]', fontsize=16)
#ax.set_xlabel('Energy[eV]', fontsize=16)
#ax.set_ylabel('Temperature[$^{\circ}$C]', fontsize=16)
ax.set_zlabel('Real $\epsilon$', fontsize=16)
pyplot.tick_params(axis='x', labelsize=14)
pyplot.tick_params(axis='y', labelsize=14)
pyplot.tick_params(axis='z', labelsize=14)


pyplot.subplot(222)
ax = fig.add_subplot(2, 2, 2, projection='3d')
surf = ax.plot_surface(EEgrid, TTgrid, ImEPSILON, rstride=1, cstride=1, cmap=cm.coolwarm,linewidth=0, antialiased=False)
cbar = fig.colorbar(surf, shrink=0.8)#, aspect=5)
cbar.set_label('Imaginary Dielectric Constant', fontsize=16)
#ax.set_xlabel('Energy[eV]', fontsize=16)
#ax.set_ylabel('Temperature[$^{\circ}$C]', fontsize=16)
ax.set_xlabel('E [eV]', fontsize=16)
ax.set_ylabel('T [$^{\circ}$C]', fontsize=16)
ax.set_zlabel('Imaginary $\epsilon$', fontsize=16)
pyplot.tick_params(axis='x', labelsize=14)
pyplot.tick_params(axis='y', labelsize=14)
pyplot.tick_params(axis='z', labelsize=14)

pyplot.subplot(223)
contour = pyplot.imshow(ReEPSILON, cmap=pyplot.get_cmap(cm.coolwarm), origin='lower', interpolation='none', extent = [Emin, Emax, T.min(), T.max()], aspect='auto')
pyplot.xlabel('Energy[eV]', fontsize=16)
pyplot.ylabel('Temperature[$^{\circ}$C]', fontsize=16)
cbar = fig.colorbar(contour) #, shrink=0.5, aspect=5)
cbar.set_label('Real Dielectric Constant', fontsize=16)
pyplot.tick_params(axis='x', labelsize=16)
pyplot.tick_params(axis='y', labelsize=16)

pyplot.subplot(224)
contour = pyplot.imshow(ImEPSILON, cmap=pyplot.get_cmap(cm.coolwarm), origin='lower', interpolation='none', extent = [Emin, Emax, T.min(), T.max()], aspect='auto')
pyplot.xlabel('Energy[eV]', fontsize=16)
pyplot.ylabel('Temperature[$^{\circ}$C]', fontsize=16)
cbar = fig.colorbar(contour) #, shrink=0.5, aspect=5)
cbar.set_label('Imaginary Dielectric Constant', fontsize=16)
pyplot.tick_params(axis='x', labelsize=16)
pyplot.tick_params(axis='y', labelsize=16)

pyplot.show()



## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## # # # # # # # #    MAKING NEW CURVES BASED ON INTERPOLATED DATA   #  
## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
## convert from permittivity to refractive index n and absorbtion coeff k:
#AbsEPSILON = numpy.sqrt( ReEPSILON**2 + ImEPSILON**2)
#n = numpy.sqrt( (AbsEPSILON + ReEPSILON)/2.0)
#k = numpy.sqrt( (AbsEPSILON - ReEPSILON)/2.0)



## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## # # # # # # # #        WRITING TO THE OUTPUT FILE       #  
## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#unit = 1
#x_min = Emin
#x_max = Emax
#dataPoints = len(EE) # All interpolated temperatures are given for energies in EE

#firstLine = str(unit)+'\t'+str(x_min)+'\t'+str(x_max)+'\t'+str(dataPoints)  

##for t in numpy.linspace(0,len(TT),10,endpoint=True):
#for t in numpy.linspace(T.min(),T.max(),20,endpoint=True):
    #tindex = numpy.searchsorted(TT,t)
    #nT = n[tindex,:]
    #kT = k[tindex,:]
    #relativePath = 'InterpolatedDataBase/'
    #fileName = 'vo2_'+str( int(t+0.5) )+'C.nk' #use rounded-off temperature as filename
    ## Write to file
    ##numpy.savetxt(relativePath+fileName,numpy.column_stack((nT,kT)),delimiter='\t',header=firstLine,comments='')

    ##print fileName
    ##print 't=',t
    ##print 'tindex=',tindex
    ##print 'TT[tindex]=',TT[tindex]
    ##print ''
    ##print 'len(nT)=', len( nT )
    ##print 'len(kT)=', len( kT )
    ##print ''
    ##print ''

    #pyplot.figure(3)
    #pyplot.plot(EE,nT,EE,kT)
#pyplot.show()
