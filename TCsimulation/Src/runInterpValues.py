#!/usr/bin/python
import numpy
import matplotlib.pyplot as pyplot
import scipy.interpolate

import subprocess # To run allow running GranFilm from this python program

import matplotlib
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # #  The temperatures, found by interpolation, to be simulated  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#simulationTemperatures = numpy.array([ #Nice for phase and alpha,gamma,beta
                                      #25, 
                                      #43,46,
                                      #50,54,57,
                                      #60,61.5,63,65,67,
                                      #80
                                      #])   
#simulationTemperatures = numpy.array([ #nice for dR, R
                                      #25, 
                                      #45,48,
                                      #50,52,54,57,
                                      #60,62,65,66,67,
                                      #70,
                                      #80
                                      #])   
simulationTemperatures = numpy.array([  #Nice for dR's low E peak
                                      25, 
                                      45,
                                      50,57,
                                      60,62,  63,64,  65,67,
                                      70,
                                      80
                                      ])   


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # #   LOAD AND INTERPOLATE THE DATA FROM KANG ET AL 2012    #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Load dielectric constant data from files:
reData = numpy.loadtxt('../DataBase/VO2/reEpsVO2.csv',skiprows=1,delimiter=',')
imData = numpy.loadtxt('../DataBase/VO2/imEpsVO2.csv',skiprows=1,delimiter=',')

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



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # #    MAKING NEW CURVES BASED ON INTERPOLATED DATA   #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# convert from permittivity to refractive index n and absorbtion coeff k:
AbsEPSILON = numpy.sqrt( ReEPSILON**2 + ImEPSILON**2)
n = numpy.sqrt( (AbsEPSILON + ReEPSILON)/2.0)
k = numpy.sqrt( (AbsEPSILON - ReEPSILON)/2.0)



# # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # #  WRITING TO '.nk'-file  #  
# # # # # # # # # # # # # # # # # # # # # # 
# '.nk'-file information to the first line of the file:
unit = 1
x_min = Emin
x_max = Emax
dataPoints = len(EE) # All interpolated temperatures are given for energies in EE

firstLine = str(unit)+'\t'+str(x_min)+'\t'+str(x_max)+'\t'+str(dataPoints)  
# BECAUSE WE'RE ONLY USING THE DATA FROM KANG AND IT'S INTERPOLATED IN THE SAME RANGES,
# THE FIRST LINE INFORMATION WILL BE THE SAME FOR ALL TEMPERATURES OF VO2.
# ====>  THE SIMULATION FOR THE TEMPERATURES STARTS HERE:





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # #  SIMULATE AND PLOT DATA FOR GIVEN TEMPERATURES  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
NT = len(simulationTemperatures) # Number of temperatures to be simulated

# Using 'coolwarm' color map to color the curves:
cmap = pyplot.get_cmap('coolwarm')

# Make dashed lines for the curves
dashes = [[0,0] for Nlists in range(NT)] #one dashed line for each temperature
for i in range(NT):
    dashes[i] = [10,0.01+2*i**2] #3 on, i*2 off
## To cycle through the given list of plot options like colors or linestyles, we need:
#from itertools import cycle
## Make cycler:
#dashcycler = cycle(dashes)

i=0 # To update color and dashes
for t in simulationTemperatures:

    # # # # # # # # # #
    # Update nk-File  #
    # # # # # # # # # #
    # Find temperature data:
    tindex = numpy.searchsorted(TT,t)
    nT = n[tindex,:]
    kT = k[tindex,:]

    ## Test nk output:
    #pyplot.figure(num=None)
    #pyplot.plot(EE,nT,EE,kT)

    # Write  nk-file:
    relativePath = '../DataBase/'
    nkfile = 'vo2.nk'
    numpy.savetxt(relativePath+nkfile, numpy.column_stack((nT,kT)),delimiter='\t',header=firstLine,comments='')


    # # # # # # # # #
    # Run GranFilm  #
    # # # # # # # # #
    # Run GranFilm with subprocess.call. shell=False because shell=True is a security hazard (shell injection).
    ifile = 'interpVO2SiO2.sif' # Input file, containing input parameters
    ofile = 'vo2sio2.dat' # Output file name
    subprocess.call(["./GranFilm", 
        "-py", 
        "-p","../Input/"+ifile,
        "-o","../Output/"+ofile], shell=False)

    # Load output files:
    data = numpy.loadtxt("../Output/"+ofile)
    data_RefTrans = numpy.loadtxt("../Output/"+ofile+"_RefTrans")


    # # # # # # #
    # Plot data #
    # # # # # # #

    ##   R_p:
    #pyplot.figure(num=1, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.plot(data_RefTrans[:,0], data_RefTrans[:,1]**2 + data_RefTrans[0,2]**2, label=str(t)+"$^{\circ}$C",
        #color=cmap(float(i)/(NT-1)), 
        ##dashes = dashes[i],
        #linewidth = 1.5)
    ##   R_s:
    #pyplot.figure(num=2, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.plot(data_RefTrans[:,0], data_RefTrans[:,5]**2 + data_RefTrans[:,6]**2, label=str(t)+"$^{\circ}$C",
        #color=cmap(float(i)/(NT-1)), 
        ##dashes = dashes[i],
        #linewidth = 1.5)


    ##   amplitude of dR/R:
    #pyplot.figure(num=3, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.plot(data[:,0], data[:,1], label=str(t)+"$^{\circ}$C",
        #color=cmap(float(i)/(NT-1)), 
        ##dashes = dashes[i],
        #linewidth = 1.5)

    ##   phase of dR/R:
    #pyplot.figure(num=4, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.plot(data[:,0], data[:,2], label=str(t)+"$^{\circ}$C",
        #color=cmap(float(i)/(NT-1)), 
        ##dashes = dashes[i],
        #linewidth = 1.5)


    #   Real \alpha_{\parallel}
    pyplot.figure(num=5, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    pyplot.plot(data[:,0],data[:,3], label=str(t)+"$^{\circ}$C",
        color=cmap(float(i)/(NT-1)), 
        #dashes = dashes[i],
        linewidth = 1.5)
    #   Imaginary \alpha_{\parallel}
    pyplot.figure(num=6, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    pyplot.plot(data[:,0],data[:,4], label=str(t)+"$^{\circ}$C",
        color=cmap(float(i)/(NT-1)), 
        #dashes = dashes[i],
        linewidth = 1.5)

    ##   Real \alpha_{\perp}
    #pyplot.figure(num=7, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.plot(data[:,0],data[:,5], label=str(t)+"$^{\circ}$C",
        #color=cmap(float(i)/(NT-1)),
        ##dashes = dashes[i],
        #linewidth = 1.5)
    ##   Imaginary \alpha_{\perp}
    #pyplot.figure(num=8, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.plot(data[:,0], data[:,6], label=str(t)+"$^{\circ}$C",
        #color=cmap(float(i)/(NT-1)),
        ##dashes = dashes[i],
        #linewidth = 1.5)

    ##   Real \gamma
    #pyplot.figure(num=9, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.plot(data[:,0],data[:,11], label=str(t)+"$^{\circ}$C",
        #color=cmap(float(i)/(NT-1)), 
        ##dashes = dashes[i],
        #linewidth = 1.5)
    ##   Imaginary \gamma
    #pyplot.figure(num=10, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.plot(data[:,0],data[:,12], label=str(t)+"$^{\circ}$C",
        #color=cmap(float(i)/(NT-1)), 
        ##dashes = dashes[i],
        #linewidth = 1.5)

    ##   Real \beta
    #pyplot.figure(num=11, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.plot(data[:,0],data[:,13], label=str(t)+"$^{\circ}$C",
        #color=cmap(float(i)/(NT-1)), 
        ##dashes = dashes[i],
        #linewidth = 1.5)
    ##   Imaginary \beta
    #pyplot.figure(num=12, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.plot(data[:,0],data[:,14], label=str(t)+"$^{\circ}$C",
        #color=cmap(float(i)/(NT-1)), 
        ##dashes = dashes[i],
        #linewidth = 1.5)

    i=i+1


#pyplot.ylim(0,6)
#pyplot.savefig('test.png', edgecolor='none')

# # # # # # # # # # # # # # # # #
# # # # # # # #  Figure Details #
# # # # # # # # # # # # # # # # #
#pyplot.figure(1)
#pyplot.tick_params(axis='x', labelsize=16)
#pyplot.tick_params(axis='y', labelsize=16)
#pyplot.xlabel('Energy [eV]', fontsize=16)
#pyplot.ylabel('$R_p$', fontsize=20)
#pyplot.legend(loc='best', fontsize=14)

#pyplot.figure(2)
#pyplot.tick_params(axis='x', labelsize=16)
#pyplot.tick_params(axis='y', labelsize=16)
#pyplot.xlabel('Energy [eV]', fontsize=16)
#pyplot.ylabel('$R_s$', fontsize=20)
#pyplot.legend(loc='best', fontsize=14)


#pyplot.figure(3)
#pyplot.tick_params(axis='x', labelsize=16)
#pyplot.tick_params(axis='y', labelsize=16)
#pyplot.xlabel('Energy [eV]', fontsize=16)
#pyplot.ylabel('$\Delta R / R$', fontsize=20)
#pyplot.legend(loc='best', fontsize=14)

#pyplot.figure(4)
#pyplot.tick_params(axis='x', labelsize=16)
#pyplot.tick_params(axis='y', labelsize=16)
#pyplot.xlabel('Energy [eV]', fontsize=16)
#pyplot.ylabel('Phase of $\Delta R / R$', fontsize=16)
#pyplot.legend(loc='best', fontsize=14)


pyplot.figure(5)
pyplot.tick_params(axis='x', labelsize=16)
pyplot.tick_params(axis='y', labelsize=16)
pyplot.xlabel('Energy [eV]', fontsize=16)
pyplot.ylabel(r'Re($\alpha_{\parallel}$)', fontsize=16)
pyplot.legend(loc='best', fontsize=14)
pyplot.figure(6)
pyplot.tick_params(axis='x', labelsize=16)
pyplot.tick_params(axis='y', labelsize=16)
pyplot.xlabel('Energy [eV]', fontsize=16)
pyplot.ylabel(r'Im($\alpha_{\parallel}$)', fontsize=16)
pyplot.legend(loc='best', fontsize=14)

#pyplot.figure(7)
#pyplot.tick_params(axis='x', labelsize=16)
#pyplot.tick_params(axis='y', labelsize=16)
#pyplot.xlabel('Energy [eV]', fontsize=16)
#pyplot.ylabel(r'Re($\alpha_{\perp}$)', fontsize=16)
#pyplot.legend(loc='best', fontsize=14)
#pyplot.figure(8)
#pyplot.tick_params(axis='x', labelsize=16)
#pyplot.tick_params(axis='y', labelsize=16)
#pyplot.xlabel('Energy [eV]', fontsize=16)
#pyplot.ylabel(r'Im($\alpha_{\perp}$)', fontsize=16)
#pyplot.legend(loc='best', fontsize=14)


#pyplot.figure(9)
#pyplot.tick_params(axis='x', labelsize=16)
#pyplot.tick_params(axis='y', labelsize=16)
#pyplot.xlabel('Energy [eV]', fontsize=16)
#pyplot.ylabel('Re($\gamma$)', fontsize=16)
#pyplot.legend(loc='best', fontsize=14)
#pyplot.figure(10)
#pyplot.tick_params(axis='x', labelsize=16)
#pyplot.tick_params(axis='y', labelsize=16)
#pyplot.xlabel('Energy [eV]', fontsize=16)
#pyplot.ylabel('Im($\gamma$)', fontsize=16)
#pyplot.legend(loc='best', fontsize=14)

#pyplot.figure(11)
#pyplot.tick_params(axis='x', labelsize=16)
#pyplot.tick_params(axis='y', labelsize=16)
#pyplot.xlabel('Energy [eV]', fontsize=16)
#pyplot.ylabel(r'Re($\beta$)', fontsize=16)
#pyplot.legend(loc='best', fontsize=14)
#pyplot.figure(12)
#pyplot.tick_params(axis='x', labelsize=16)
#pyplot.tick_params(axis='y', labelsize=16)
#pyplot.xlabel('Energy [eV]', fontsize=16)
#pyplot.ylabel(r'Im($\beta$)', fontsize=16)
#pyplot.legend(loc='best', fontsize=14)

pyplot.show()

