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

#print reData.shape
#print inData.shape

T = numpy.array([23., 40., 60.,65.,67.,69.,72.,75.,80.])

E1 = reData[:,0]
#Eps1 = reData[:,1:] 

E2 = imData[:,0]
#Eps2 = imData[:,1:]


Emin= max( E1.min(), E2.min() )
Emax= min( E1.max(), E2.max() )
print E1.min(), E1.max()
print E2.min(), E2.max()
print Emin, Emax


# Wanted energy/temperature (chosen some random values)
energy = Emin + 0.1*(Emax-Emin)
TT     = T.min() + 0.24*(T.max()-T.min())


# Get dielectric functions for a given energy
N=T.shape[0]
reEpsilon=numpy.zeros([N])
imEpsilon=numpy.zeros([N])


# Interpolate in energy
for n in range(N):
    # --- real part
    reE = scipy.interpolate.interp1d(E1, reData[:,n+1] )
    reEpsilon[n] = reE(energy)
    # --- imag part
    imE = scipy.interpolate.interp1d(E2, imData[:,n+1] )
    imEpsilon[n] = imE(energy)


# Interpolate in Temperature
reE = scipy.interpolate.interp1d(T, reEpsilon )
imE = scipy.interpolate.interp1d(T, imEpsilon )

Epsilon = reE( TT ) + 1j* imE( TT )

print Epsilon




## Set up a regular grid of interpolation points:
#newT = numpy.linspace(T.min(),T.max(), 100) # temperature interpolation points
#newE1 = numpy.linspace(E1.min(),E1.max(), 100) # energy interpolation points for real values
#newE2 = numpy.linspace(E2.min(),E2.max(), 100) # energy interpolation points for imaginary values

#newEps1 = interpolator1(newE1, newT)
#newEps2 = interpolator2(newE2, newT)

## Meshgrid
#E1grid,T1grid = numpy.meshgrid(newE1,newT)
#E2grid,T2grid = numpy.meshgrid(newE2,newT)

#fig = pyplot.figure(1)
#ax = Axes3D(fig)
##fig.add_subplot(111, projection='3d')
#ax.plot_wireframe(E1grid, T1grid, newEps1)#, rstride=1, cstride=1, cmap=cm.jet)

#fig = pyplot.figure(2)
#ax = Axes3D(fig)
#ax.plot_wireframe(E2grid, T1grid, newEps2)#, rstride=1, cstride=1, cmap=cm.jet)
#pyplot.show()








# MY OLD CODE:
##!/usr/bin/python
#import numpy
#import matplotlib.pyplot as pyplot
#import scipy.interpolate

#import matplotlib
#from mpl_toolkits.mplot3d import Axes3D
#from matplotlib import cm

## Load dielectric constant data from files:
#reData = numpy.loadtxt('reEpsVO2.csv',skiprows=1,delimiter=',')
#imData = numpy.loadtxt('imEpsVO2.csv',skiprows=1,delimiter=',')

#T = numpy.array([23., 40., 60.,65.,67.,69.,72.,75.,80.])

#E1 = reData[:,0]
#Eps1 = reData[:,1:] 

#E2 = imData[:,0]
#Eps2 = imData[:,1:]

## structure:
##        9
##     T1 T2 T3
## E1 [        ]
## E2 [  Eps1  ]
## E3 [        ]
## E4

## Check lodaded data aafo Energy:
##pyplot.plot(E2 , Eps2[:,0])
##pyplot.plot(E2 , Eps2[:,1])
##pyplot.plot(E2 , Eps2[:,2])
##pyplot.plot(E2 , Eps2[:,3])
##pyplot.plot(E2 , Eps2[:,4])
##pyplot.plot(E2 , Eps2[:,5])
##pyplot.plot(E2 , Eps2[:,6])
##pyplot.plot(E2 , Eps2[:,7])
##pyplot.plot(E2 , Eps2[:,8])
##pyplot.show()

## Check lodaded data aafo temperare:
##pyplot.plot(T, Eps1[0,  :])
##pyplot.plot(T, Eps1[50, :])
##pyplot.plot(T, Eps1[100,:])
##pyplot.plot(T, Eps1[150,:])
##pyplot.plot(T, Eps1[200,:])
##pyplot.plot(T, Eps1[250,:])
##pyplot.plot(T, Eps1[300,:])
##pyplot.show()

## Set up a regular grid of interpolation points:
#newT = numpy.linspace(T.min(),T.max(), 100) # temperature interpolation points
#newE1 = numpy.linspace(E1.min(),E1.max(), 100) # energy interpolation points for real values
#newE2 = numpy.linspace(E2.min(),E2.max(), 100) # energy interpolation points for imaginary values

## Interpolate the data:
#interpolator1 = scipy.interpolate.interp2d(E1,T,Eps1,kind='linear')
#interpolator2 = scipy.interpolate.interp2d(E2,T,Eps2,kind='linear')

#newEps1 = interpolator1(newE1, newT)
#newEps2 = interpolator2(newE2, newT)

#print 'Interpolated imaginary dielectric constant:'
#print newEps2

## Meshgrid
#E1grid,T1grid = numpy.meshgrid(newE1,newT)
#E2grid,T2grid = numpy.meshgrid(newE2,newT)

#fig = pyplot.figure(1)
#ax = Axes3D(fig)
##fig.add_subplot(111, projection='3d')
#ax.plot_wireframe(E1grid, T1grid, newEps1)#, rstride=1, cstride=1, cmap=cm.jet)

#fig = pyplot.figure(2)
#ax = Axes3D(fig)
#ax.plot_wireframe(E2grid, T1grid, newEps2)#, rstride=1, cstride=1, cmap=cm.jet)
#pyplot.show()

##pyplot.imshow(newEps1, vmin=Eps1.min(), vmax=Eps1.max(), origin='lower', extent = [T.min(), T.max(), E1.min(), E1.max()])
##pyplot.show()

##pyplot.imshow(newEps2, vmin=Eps2.min(), vmax=Eps2.max(), origin='lower', extent = [T.min(), T.max(), E2.min(), E2.max()])
##pyplot.show()








#reE = reData[:,0]
#imE = imData[:,0]

#rePermittivity = reData[:,1:] 
#imPermittivity = imData[:,1:]

## Set up a regular grid of interpolation points:
#Ti = numpy.linspace(T.min(),T.max(), 100) # temperature interpolation points
#reEi = numpy.linspace(reE.min(),reE.max(), 100) # energy interpolation points for real values
#imEi = numpy.linspace(imE.min(),imE.max(), 100) # energy interpolation points for imaginary values

##reEgrid,reTgrid = numpy.meshgrid(Ti,reEi)
##imEgrid,imTgrid = numpy.meshgrid(Ti,imEi)
#reEgrid,reTgrid = numpy.meshgrid(T,reE)
#imEgrid,imTgrid = numpy.meshgrid(T,imE)

## Interpolate
## First attempt:
##rbf = scipy.interpolate.Rbf(reE,T,rePermittivity,function='linear')
##rePermittivityi = rbf(reEgrid,reTgrid)

##rbf = scipy.interpolate.Rbf(imE,T,imPermittivity,function='linear')
##imPermittivityi = rbf(imEigrid,imTgrid)

## Second attempt:
#re_interpolator = scipy.interpolate.interp2d(reE,T, rePermittivity, kind='linear', copy=True)
#re = re_interpolator(reEi,Ti)

#im_interpolator = scipy.interpolate.interp2d(imE,T, imPermittivity, kind='linear', copy=True)
#im = im_interpolator(imEi,Ti)

### Third attempt:
##re_interpolator = scipy.interpolate.interp2d.__call__(T, reE, rePermittivity, kind='linear', copy=True)
##im_interpolator = scipy.interpolate.interp2d.__call__(T, imE, imPermittivity, kind='linear', copy=True)

##re = re_interpolator(reTi,reEi)
##im = im_interpolator(imTi,imEi)


## Plot Interpolated data:
#pyplot.figure(1)
##pyplot.imshow(rePermittivityi, vmin=rePermittivity.min(), vmax=rePermittivity.max(), origin='lower', extent = [T.min(), T.max(), reE.min(), reE.max()])
#pyplot.imshow(re, vmin=rePermittivity.min(), vmax=rePermittivity.max(), origin='lower', extent = [T.min(), T.max(), reE.min(), reE.max()])
##pyplot.scatter(T,reE, c=rePermittivity)
#pyplot.colorbar()

#pyplot.figure(2)
##pyplot.imshow(imPermittivityi, vmin=imPermittivity.min(), vmax=imPermittivity.max(), origin='lower', extent = [T.min(), T.max(), imE.min(), imE.max()])
#pyplot.imshow(im, vmin=imPermittivity.min(), vmax=imPermittivity.max(), origin='lower', extent = [T.min(), T.max(), imE.min(), imE.max()])
##pyplot.scatter(T,imE, c=imPermittivity)
#pyplot.colorbar()

#pyplot.show()

