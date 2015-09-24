#!/usr/bin/python
#This program (2) is the same as the first one, but this one plots the values as a function of wavelength!
import numpy
import matplotlib.pyplot as pyplot
import scipy.interpolate

import subprocess # To run allow running GranFilm from this python program

import matplotlib
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm

# Plotting colors using ColorPy:
import colorpy.ciexyz
import colorpy.colormodels
import colorpy.plots
import pylab

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # #  The temperatures, found by interpolation, to be simulated  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#simulationTemperatures = numpy.array([25,80])   
simulationTemperatures = numpy.linspace(25,80,80-25+1,endpoint=True)
#simulationTemperatures = numpy.array([ #Nice for phase and alpha,gamma,beta
                                      #25, 
                                      #43,46,
                                      #50,54,
                                      #61,63,65,
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
#simulationTemperatures = numpy.array([  #Nice for dR's low E peak
                                      #25, 
                                      #45,
                                      #50,57,
                                      #60,62,  63,64,  65,67,
                                      #70,
                                      #80
                                      #])   


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

# To plot the height of the left peak dR/R as a function of temperature,
# in each iteration:
peakValues = numpy.zeros(NT)

## For storing the reflected color for each temperature:
#irgb = numpy.zeros((len(simulationTemperatures), 3))
# Storing the reflectance 
Rp = numpy.zeros((1024,len(simulationTemperatures))) #1024 is specified in the .sif file
Rs = numpy.zeros((1024,len(simulationTemperatures)))
Tp = numpy.zeros((1024,len(simulationTemperatures))) #1024 is specified in the .sif file
Ts = numpy.zeros((1024,len(simulationTemperatures)))
wl_R = numpy.zeros((1024,len(simulationTemperatures)))
#RpFlat = numpy.zeros((1024,len(simulationTemperatures))) #TO BE DELETED, only a check
#RsFlat = numpy.zeros((1024,len(simulationTemperatures))) #TO BE DELETED, only a check

#i=0 # To update color and dashes
#for t in simulationTemperatures:
for i,t in enumerate(simulationTemperatures):

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
    ## dR/R plot with spectrum and resulting color
    #dRspectrum = numpy.column_stack((data[:,-1], data[:,1]))
    #colorpy.plots.spectrum_plot(dRspectrum, 'Color','../Figures/ColorPlots/dRspec'+str(t)+'C')
    #pyplot.subplot(212)
    #pyplot.xlabel('Wavelength[nm]')
    #pyplot.ylabel('Reflectance')
    #
    ##   Reflectance and transmission:
    #pyplot.figure(num=1, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.subplot(121)
    #pyplot.plot(data_RefTrans[:,0], data_RefTrans[:,3]**2 + data_RefTrans[:,4]**2,
        #label=str(t)+"$^{\circ}$C Rp Flat",
        #linewidth = 1.5)
    #pyplot.plot(data_RefTrans[:,0], data_RefTrans[:,7]**2 + data_RefTrans[:,8]**2, 
        #label=str(t)+"$^{\circ}$C Rs Flat",
        #linewidth = 1.5)
    #pyplot.plot(data_RefTrans[:,0], data_RefTrans[:,11]**2 + data_RefTrans[:,12]**2,
        #label=str(t)+"$^{\circ}$C Tp Flat",
        #linewidth = 1.5)
    #pyplot.plot(data_RefTrans[:,0], data_RefTrans[:,15]**2 + data_RefTrans[:,16]**2, 
        #label=str(t)+"$^{\circ}$C Ts Flat",
        #linewidth = 1.5)
    #pyplot.subplot(122)
    #pyplot.plot(data_RefTrans[:,0], data_RefTrans[:,1]**2 + data_RefTrans[:,2]**2,
        #label=str(t)+"$^{\circ}$C Rp",
        #linewidth = 1.5)
    #pyplot.plot(data_RefTrans[:,0], data_RefTrans[:,5]**2 + data_RefTrans[:,6]**2, 
        #label=str(t)+"$^{\circ}$C Rs",
        #linewidth = 1.5)
    #pyplot.plot(data_RefTrans[:,0], data_RefTrans[:,9]**2 + data_RefTrans[:,10]**2,
        #label=str(t)+"$^{\circ}$C Tp",
        #linewidth = 1.5)
    #pyplot.plot(data_RefTrans[:,0], data_RefTrans[:,13]**2 + data_RefTrans[:,14]**2, 
        #label=str(t)+"$^{\circ}$C Ts",
        #linewidth = 1.5)


    ##   amplitude of dR/R:
    #pyplot.figure(num=2, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.plot(data[:,-1], data[:,1], label=str(t)+"$^{\circ}$C",
        #color=cmap(float(i)/(NT-1)), 
        #linewidth = 1.5)

    ##   phase of dR/R:
    #pyplot.figure(num=3, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.plot(data[:,-1], data[:,2], label=str(t)+"$^{\circ}$C",
        #color=cmap(float(i)/(NT-1)), 
        #linewidth = 1.5)
    ##   phase of dR/R:
    #pyplot.figure(num=4, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.plot(data[:,-1], numpy.log(abs(data[:,2])), label=str(t)+"$^{\circ}$C",
        #color=cmap(float(i)/(NT-1)), 
        #linewidth = 1.5)



    ##   Real \alpha_{\parallel}
    #pyplot.figure(num=5, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.plot(data[:,-1],data[:,3], label=str(t)+"$^{\circ}$c",
        #color=cmap(float(i)/(nt-1)), 
        ##dashes = dashes[i],
        #linewidth = 1.5)
    ##   Imaginary \alpha_{\parallel}
    #pyplot.figure(num=6, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.plot(data[:,-1],data[:,4], label=str(t)+"$^{\circ}$C",
        #color=cmap(float(i)/(NT-1)), 
        ##dashes = dashes[i],
        #linewidth = 1.5)


    ##   Real \alpha_{\perp}
    #pyplot.figure(num=7, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.plot(data[:,-1],data[:,5], label=str(t)+"$^{\circ}$C",
        #color=cmap(float(i)/(NT-1)),
        ##dashes = dashes[i],
        #linewidth = 1.5)
    ##   Imaginary \alpha_{\perp}
    #pyplot.figure(num=8, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.plot(data[:,-1], data[:,6], label=str(t)+"$^{\circ}$C",
        #color=cmap(float(i)/(NT-1)),
        ##dashes = dashes[i],
        #linewidth = 1.5)

    ##   Real \gamma
    #pyplot.figure(num=9, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.plot(data[:,-1],data[:,11], label=str(t)+"$^{\circ}$C",
        #color=cmap(float(i)/(NT-1)), 
        ##dashes = dashes[i],
        #linewidth = 1.5)
    ##   Imaginary \gamma
    #pyplot.figure(num=10, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.plot(data[:,-1],data[:,12], label=str(t)+"$^{\circ}$C",
        #color=cmap(float(i)/(NT-1)), 
        ##dashes = dashes[i],
        #linewidth = 1.5)

    ##   Real \beta
    #pyplot.figure(num=11, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.plot(data[:,-1],data[:,13], label=str(t)+"$^{\circ}$C",
        #color=cmap(float(i)/(NT-1)), 
        ##dashes = dashes[i],
        #linewidth = 1.5)
    ##   Imaginary \beta
    #pyplot.figure(num=12, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
    #pyplot.plot(data[:,-1],data[:,14], label=str(t)+"$^{\circ}$C",
        #color=cmap(float(i)/(NT-1)), 
        ##dashes = dashes[i],
        #linewidth = 1.5)


    # Get dR/R peak:
    peakValues[i] = numpy.amax(data[:40,1])

    # Get Rp and Rs
    Rp[:,i] = data_RefTrans[:,1]**2 + data_RefTrans[:,2]**2
    Rs[:,i] = data_RefTrans[:,5]**2 + data_RefTrans[:,6]**2
    Tp[:,i] = data_RefTrans[:,9]**2 + data_RefTrans[:,10]**2
    Ts[:,i] = data_RefTrans[:,13]**2 + data_RefTrans[:,14]**2
    wl_R[:,i] = 1000*1.243/data_RefTrans[:,0] # reflectance x-axis, wavelenght in nm
    # For Control
    #RpFlat[:,i] = data_RefTrans[:,3]**2 + data_RefTrans[:,4]**2 #can be deleted if check is okay
    #RsFlat[:,i] = data_RefTrans[:,7]**2 + data_RefTrans[:,8]**2 #can be deleted if chekc is okay

    #i=i+1



#pyplot.ylim(0,6)
#pyplot.savefig('test.png', edgecolor='none')

# # # # # # # # # # # # # # # # #
# # # # # # # #  Figure Details #
# # # # # # # # # # # # # # # # #
#pyplot.figure(1)
#pyplot.subplot(121)
#pyplot.tick_params(axis='x', labelsize=20)
#pyplot.tick_params(axis='y', labelsize=20)
#pyplot.xlabel('Energy[eV]', fontsize=20)
#pyplot.ylabel('$Flat R,T$', fontsize=20)
#pyplot.legend(loc='best', fontsize=14)
#pyplot.subplot(122)
#pyplot.tick_params(axis='x', labelsize=20)
#pyplot.tick_params(axis='y', labelsize=20)
#pyplot.xlabel('Energy[eV]', fontsize=20)
#pyplot.ylabel('$Granular R,T$', fontsize=20)
#pyplot.legend(loc='best', fontsize=14)
##pyplot.xlim(numpy.amin(data[:,-1]),800)
##pyplot.xlim(350,750)

#pyplot.figure(2)
#pyplot.tick_params(axis='x', labelsize=20)
#pyplot.tick_params(axis='y', labelsize=20)
#pyplot.xlabel('Wavelength [nm]', fontsize=20)
#pyplot.ylabel('$\Delta R / R$', fontsize=20)
#pyplot.legend(loc='best', fontsize=14)
##pyplot.xlim(numpy.amin(data[:,-1]),800)
##pyplot.xlim(350,750)

#pyplot.figure(3)
#pyplot.tick_params(axis='x', labelsize=20)
#pyplot.tick_params(axis='y', labelsize=20)
#pyplot.xlabel('Wavelength [nm]', fontsize=20)
#pyplot.ylabel('Phase of $\Delta R / R$', fontsize=16)
#pyplot.legend(loc='best', fontsize=14)
##pyplot.xlim(numpy.amin(data[:,-1]),800)
##pyplot.xlim(350,750)

#pyplot.figure(4)
#pyplot.tick_params(axis='x', labelsize=20)
#pyplot.tick_params(axis='y', labelsize=20)
#pyplot.xlabel('Wavelength [nm]', fontsize=20)
#pyplot.ylabel('log |phase of $\Delta R / R$|', fontsize=26)
#pyplot.legend(loc='best', fontsize=14)
##pyplot.xlim(numpy.amin(data[:,-1]),800)
##pyplot.xlim(350,750)

#pyplot.figure(5)
#pyplot.tick_params(axis='x', labelsize=20)
#pyplot.tick_params(axis='y', labelsize=20)
#pyplot.xlabel('Wavelength [nm]', fontsize=20)
#pyplot.ylabel(r'Re($\alpha_{\parallel}$)', fontsize=16)
#pyplot.legend(loc='best', fontsize=14)
#pyplot.figure(6)
#pyplot.tick_params(axis='x', labelsize=20)
#pyplot.tick_params(axis='y', labelsize=20)
#pyplot.xlabel('Wavelength [nm]', fontsize=20)
#pyplot.ylabel(r'Im($\alpha_{\parallel}$)', fontsize=16)
#pyplot.legend(loc='best', fontsize=14)

#pyplot.figure(7)
#pyplot.tick_params(axis='x', labelsize=20)
#pyplot.tick_params(axis='y', labelsize=20)
#pyplot.xlabel('Wavelength [nm]', fontsize=20)
#pyplot.ylabel(r'Re($\alpha_{\perp}$)', fontsize=16)
#pyplot.legend(loc='best', fontsize=14)
#pyplot.figure(8)
#pyplot.tick_params(axis='x', labelsize=20)
#pyplot.tick_params(axis='y', labelsize=20)
#pyplot.xlabel('Wavelength [nm]', fontsize=20)
#pyplot.ylabel(r'Im($\alpha_{\perp}$)', fontsize=16)
#pyplot.legend(loc='best', fontsize=14)


#pyplot.figure(9)
#pyplot.tick_params(axis='x', labelsize=20)
#pyplot.tick_params(axis='y', labelsize=20)
#pyplot.xlabel('Wavelength [nm]', fontsize=20)
#pyplot.ylabel('Re($\gamma$)', fontsize=16)
#pyplot.legend(loc='best', fontsize=14)
#pyplot.figure(10)
#pyplot.tick_params(axis='x', labelsize=20)
#pyplot.tick_params(axis='y', labelsize=20)
#pyplot.xlabel('Wavelength [nm]', fontsize=20)
#pyplot.ylabel('Im($\gamma$)', fontsize=16)
#pyplot.legend(loc='best', fontsize=14)

#pyplot.figure(11)
#pyplot.tick_params(axis='x', labelsize=20)
#pyplot.tick_params(axis='y', labelsize=20)
#pyplot.xlabel('Wavelength [nm]', fontsize=20)
#pyplot.ylabel(r'Re($\beta$)', fontsize=16)
#pyplot.legend(loc='best', fontsize=14)
#pyplot.figure(12)
#pyplot.tick_params(axis='x', labelsize=20)
#pyplot.tick_params(axis='y', labelsize=20)
#pyplot.xlabel('Wavelength [nm]', fontsize=20)
#pyplot.ylabel(r'Im($\beta$)', fontsize=16)
#pyplot.legend(loc='best', fontsize=14)


## Plot only the peak values
#pyplot.figure(num=13, figsize=(5, 4), dpi=120, facecolor='w', edgecolor='k')
#pyplot.plot(simulationTemperatures, peakValues, linewidth = 1.5, color='k')#, marker='^')
#pyplot.xlim(simulationTemperatures.min(),simulationTemperatures.max())
#pyplot.xlabel('Temperature [$^{\circ}C$]')#, fontsize=24)
#pyplot.ylabel('Peak Amplitude $\Delta R/R$')#, fontsize=26)
#pyplot.gcf().subplots_adjust(left=0.15) #prevent labels being cut off
#pyplot.gcf().subplots_adjust(bottom=0.15) #prevent labels being cut off
#pyplot.legend(loc='best', fontsize=14)

## Plot change in reflectance R_h-R_c:
#pyplot.figure(num=14, figsize=(5, 4), dpi=120, facecolor='w', edgecolor='k')
#pyplot.subplot(221)
#pyplot.plot(wl_R[:,0], Rp[:,0], '--', color='k', label='$25^{\circ}C$')
#pyplot.plot(wl_R[:,-1], Rp[:,-1],'-', color='k', label='$80^{\circ}C$')
##pyplot.xlabel('Wavelength[nm]')
#pyplot.ylabel('$R_p$', fontsize=18)
#pyplot.xlim(wl_R[:,0].min(),wl_R[:,0].max())
#pyplot.legend(loc='best', fontsize=14)
#pyplot.subplot(222)
#pyplot.plot(wl_R[:,0], Rs[:,0], '--', color='k', label='$25^{\circ}C$')
#pyplot.plot(wl_R[:,-1], Rs[:,-1],'-', color='k', label='$80^{\circ}C$')
##pyplot.xlabel('Wavelength[nm]')
#pyplot.ylabel('$R_s$', fontsize=18)
#pyplot.xlim(wl_R[:,0].min(),wl_R[:,0].max())
#pyplot.legend(loc='best', fontsize=14)
#if( numpy.array_equal(wl_R[:,0], wl_R[:,-1]) ): #Check if elements are equal
    #pyplot.subplot(223)
    #pyplot.plot(wl_R[:,0],  Rp[:,-1] - Rp[:,0] , linewidth=1.5, color='k')
    #pyplot.xlabel('Wavelength[nm]')
    #pyplot.ylabel('$R_{p_{80^{\circ}C}}-R_{p_{25^{\circ}C}}$', fontsize=18)
    #pyplot.xlim(wl_R[:,0].min(),wl_R[:,0].max())
    #pyplot.subplot(224)
    #pyplot.plot(wl_R[:,0],  Rs[:,-1] - Rs[:,0] , linewidth=1.5, color='k')
    #pyplot.xlabel('Wavelength[nm]')
    #pyplot.ylabel('$R_{s_{80^{\circ}C}}-R_{s_{25^{\circ}C}}$', fontsize=18)
    #pyplot.xlim(wl_R[:,0].min(),wl_R[:,0].max())

# Plot total reflectance in vis and IR:
dwl = ( wl_R[:,0].max()-wl_R[:,0].min() )/( len(wl_R[:,0]) - 1 )
def findIndex(value, array):
    minVal = 1e1000 #"inf"
    index = 0
    for i,e in enumerate(array):
        if( abs(e-value) < minVal):
            minVal = abs(e-value)
            index = i
    return index
ivis1 = findIndex(380, wl_R[:,0])
ivis2 = findIndex(780, wl_R[:,0])
dvis = 780-380
dIR = wl_R[:,0].max()-780
##For checking:
#RpFlat_visible = numpy.zeros( len(Rp[0,:]) ) #may be deleted
#RsFlat_visible = numpy.zeros( len(Rs[0,:]) ) #may be deleted
#RpFlat_IR = numpy.zeros( len(Rp[0,:]) )      #may be deleted
#RsFlat_IR = numpy.zeros( len(Rs[0,:]) )      #may be deleted
##
Rp_visible = numpy.zeros( len(Rp[0,:]) )
Rs_visible = numpy.zeros( len(Rs[0,:]) )
Rp_IR = numpy.zeros( len(Rp[0,:]) )
Rs_IR = numpy.zeros( len(Rs[0,:]) )
Tp_visible = numpy.zeros( len(Rp[0,:]) )
Ts_visible = numpy.zeros( len(Rs[0,:]) )
Tp_IR = numpy.zeros( len(Rp[0,:]) )
Ts_IR = numpy.zeros( len(Rs[0,:]) )
for i in range(len(Rp_visible)):
    Rp_visible[i] = (Rp[ivis2:ivis1,i]*dwl).sum()/dvis
    Rp_IR[i] = (Rp[:ivis2,i]*dwl).sum()/dIR
    Tp_visible[i] = (Tp[ivis2:ivis1,i]*dwl).sum()/dvis
    Tp_IR[i] = (Tp[:ivis2,i]*dwl).sum()/dIR
for i in range(len(Rs_visible)):
    Rs_visible[i] = (Rp[ivis2:ivis1,i]*dwl).sum()/dvis
    Rs_IR[i] = (Rp[:ivis2,i]*dwl).sum()/dIR
    Ts_visible[i] = (Tp[ivis2:ivis1,i]*dwl).sum()/dvis
    Ts_IR[i] = (Tp[:ivis2,i]*dwl).sum()/dIR
pyplot.figure(num=15, figsize=(2.5, 2), dpi=120, facecolor='w', edgecolor='k')
pyplot.subplot(221)
pyplot.plot(simulationTemperatures, Rp_visible, color='k')
#pyplot.xlabel('Temperature[$^{\circ}$C]')
pyplot.ylabel('$\int_{visible} R_p(\lambda) d\lambda /\Delta \lambda_{vis} $')#, fontsize=18)
pyplot.xlim(simulationTemperatures.min(), simulationTemperatures.max())
pyplot.subplot(222)
pyplot.plot(simulationTemperatures, Rs_visible, color='k')
#pyplot.xlabel('Temperature[$^{\circ}$C]')
pyplot.ylabel('$\int_{visible} R_s(\lambda) d\lambda /\Delta \lambda _{IR}$')#, fontsize=18)
pyplot.xlim(simulationTemperatures.min(), simulationTemperatures.max())
pyplot.subplot(223)
pyplot.plot(simulationTemperatures, Rp_IR, color='k')
pyplot.xlabel('Temperature[$^{\circ}$C]')
pyplot.ylabel('$\int_{IR} R_p(\lambda) d\lambda /\Delta \lambda _{vis}$')#, fontsize=18)
pyplot.xlim(simulationTemperatures.min(), simulationTemperatures.max())
pyplot.subplot(224)
pyplot.plot(simulationTemperatures, Rs_IR, color='k')
pyplot.xlabel('Temperature[$^{\circ}$C]')
pyplot.ylabel('$\int_{IR} R_s(\lambda) d\lambda /\Delta \lambda _{IR}$')#, fontsize=18)
pyplot.xlim(simulationTemperatures.min(), simulationTemperatures.max())
#
pyplot.figure(num=16, figsize=(2.5, 2), dpi=120, facecolor='w', edgecolor='k')
pyplot.subplot(221)
pyplot.plot(simulationTemperatures, Tp_visible, color='k')
#pyplot.xlabel('Temperature[$^{\circ}$C]')
pyplot.ylabel('$\int_{visible} T_p(\lambda) d\lambda /\Delta \lambda_{vis} $')#, fontsize=18)
pyplot.xlim(simulationTemperatures.min(), simulationTemperatures.max())
pyplot.subplot(222)
pyplot.plot(simulationTemperatures, Ts_visible, color='k')
#pyplot.xlabel('Temperature[$^{\circ}$C]')
pyplot.ylabel('$\int_{visible} T_s(\lambda) d\lambda /\Delta \lambda _{IR}$')#, fontsize=18)
pyplot.xlim(simulationTemperatures.min(), simulationTemperatures.max())
pyplot.subplot(223)
pyplot.plot(simulationTemperatures, Tp_IR, color='k')
pyplot.xlabel('Temperature[$^{\circ}$C]')
pyplot.ylabel('$\int_{IR} T_p(\lambda) d\lambda /\Delta \lambda _{vis}$')#, fontsize=18)
pyplot.xlim(simulationTemperatures.min(), simulationTemperatures.max())
pyplot.subplot(224)
pyplot.plot(simulationTemperatures, Ts_IR, color='k')
pyplot.xlabel('Temperature[$^{\circ}$C]')
pyplot.ylabel('$\int_{IR} T_s(\lambda) d\lambda /\Delta \lambda _{IR}$')#, fontsize=18)
pyplot.xlim(simulationTemperatures.min(), simulationTemperatures.max())

## PLOTTING REFLECTED COLOR:
## Get colors from spectrums:
#irgbRp = numpy.zeros((len(simulationTemperatures),3))
#irgbRs = numpy.zeros((len(simulationTemperatures),3))
#for i in range(len(wl_R[0,:])):
    #pSpectrum = numpy.column_stack((wl_R[:,i], Rp[:,i]))
    #sSpectrum = numpy.column_stack((wl_R[:,i], Rs[:,i]))

    #xyz_p = colorpy.ciexyz.xyz_from_spectrum(pSpectrum)
    #xyz_s = colorpy.ciexyz.xyz_from_spectrum(sSpectrum)

    #rgb_p = colorpy.colormodels.rgb_from_xyz(xyz_p)
    #rgb_s = colorpy.colormodels.rgb_from_xyz(xyz_s)

    #irgbRp[i,:] = colorpy.colormodels.irgb_from_rgb(rgb_p)
    #irgbRs[i,:] = colorpy.colormodels.irgb_from_rgb(rgb_s)
## Plot Reflected Color:
##pyplot.figure(num=16, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
#pyplot.figure(num=16, figsize=(5, 4), dpi=120, facecolor='w', edgecolor='k')
#colorpy.plots.color_vs_param_plot(simulationTemperatures, irgbRp,'p-pol color','../Figures/ColorPlots/RpColor',
    #tight=False, plotfunc=pyplot.plot, xlabel='Temperature', ylabel='RGB Color')
#pyplot.subplot(211)
#pyplot.xlim(simulationTemperatures.min(),simulationTemperatures.max())
#pyplot.tick_params(labelleft=False, left=False, top=False, right=False)
#pyplot.subplot(212)
#pyplot.xlim(simulationTemperatures.min(),simulationTemperatures.max())
##pyplot.figure(num=17, figsize=(10, 8), dpi=120, facecolor='w', edgecolor='k')
#pyplot.figure(num=17, figsize=(5, 4), dpi=120, facecolor='w', edgecolor='k')
#pyplot.xlim(simulationTemperatures.min(),simulationTemperatures.max())
#colorpy.plots.color_vs_param_plot(simulationTemperatures, irgbRs,'s-pol color','../Figures/ColorPlots/RsColor',
    #tight=False, plotfunc=pyplot.plot, xlabel='Temperature', ylabel='RGB Color')
#pyplot.subplot(211)
#pyplot.xlim(simulationTemperatures.min(),simulationTemperatures.max())
#pyplot.tick_params(labelleft=False, left=False, top=False, right=False)
#pyplot.subplot(212)
#pyplot.xlim(simulationTemperatures.min(),simulationTemperatures.max())

## Rp, Rs at given T, plot with spectrum and resulting color
#for i,t in enumerate(simulationTemperatures):
    #pyplot.figure(19+2*i-1)
    #spectrum = numpy.column_stack((wl_R[:,0], Rp[:,i]))
    #colorpy.plots.spectrum_plot(spectrum, 'Color','../Figures/ColorPlots/Rpcolor'+str(t)+'C')
    #pyplot.subplot(212)
    #pyplot.xlabel('Wavelength[nm]')
    #pyplot.ylabel('R_p')

    #pyplot.figure(19+i*2)
    #spectrum = numpy.column_stack((wl_R[:,0], Rs[:,i]))
    #colorpy.plots.spectrum_plot(spectrum, 'Color','../Figures/ColorPlots/Rscolor'+str(t)+'C')
    #pyplot.subplot(212)
    #pyplot.xlabel('Wavelength[nm]')
    #pyplot.ylabel('R_s')

pyplot.show()
