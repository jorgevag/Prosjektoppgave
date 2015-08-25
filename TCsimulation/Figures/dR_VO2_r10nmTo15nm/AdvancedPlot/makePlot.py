#!/usr/bin/env python
import numpy
import matplotlib.pyplot as pyplot

import sys, getopt
import numpy

#skipRows = 0 # Number of rows to skip in the files read. (input agrument to numpy.loadtxt())
#dataDelimiter=None # What separates the file values. (input arg to numpy.loadtxt(),default = any whitespace)

# Getting different line types
from itertools import cycle
lines = ["-","--","-.",":"]
linecycler = cycle(lines)

# Load the data r=10nm:
data10nmRT = numpy.loadtxt('Output_r10/vo2_RT.dat') # Load file data
data10nm60C = numpy.loadtxt('Output_r10/vo2_60C.dat') # Load file data
data10nm65C = numpy.loadtxt('Output_r10/vo2_65C.dat') # Load file data
data10nm80C = numpy.loadtxt('Output_r10/vo2_80C.dat') # Load file data

# Load the data r=15nm:
data15nmRT = numpy.loadtxt('Output_r15/vo2_RT.dat') # Load file data
data15nm60C = numpy.loadtxt('Output_r15/vo2_60C.dat') # Load file data
data15nm65C = numpy.loadtxt('Output_r15/vo2_65C.dat') # Load file data
data15nm80C = numpy.loadtxt('Output_r15/vo2_80C.dat') # Load file data


##pyplot.figure(1) 
#pyplot.figure(num=None, figsize=(8, 6), dpi=120, facecolor='w', edgecolor='k')
##pyplot.plot(data[:,0], data[:,1], next(linecycler), color='k' ,label='$\Delta R/R$ '+str(ifileNameTemperature))
#pyplot.plot( data10nmRT[:,0], data10nmRT[:,1] , linestyle='--',color='k' ,label='RT')
#pyplot.plot(data10nm60C[:,0], data10nm60C[:,1], linestyle='-' ,color='k' , label='60$^{\circ}$C')
##pyplot.plot(data10nm65C[:,0], data10nm65C[:,1], linestyle='-.',color='k' ,label='65$^{\circ}$C')
#pyplot.plot(data10nm80C[:,0], data10nm80C[:,1], linestyle=':' ,color='k' , label='80$^{\circ}$C')

#pyplot.plot( data15nmRT[:,0], data15nmRT[:,1] , linestyle='--',color='k' )
#pyplot.plot(data15nm60C[:,0], data15nm60C[:,1], linestyle='-', color='k' )
##pyplot.plot(data15nm65C[:,0], data15nm65C[:,1], linestyle='-.', color='k')
#pyplot.plot(data15nm80C[:,0], data15nm80C[:,1], linestyle=':', color='k' )
#pyplot.legend(loc='best')

##pyplot.text(2.5, 0.2, r'$r = 15$nm', fontsize=15)
##pyplot.text(3.5, 0.1, r'$r = 10$nm', fontsize=15)

#pyplot.tick_params(axis='x', labelsize=16)
#pyplot.tick_params(axis='y', labelsize=16)
#pyplot.xlabel('Energy[eV]', fontsize=16)
#pyplot.ylabel('$ \Delta R / R$', fontsize=16)
##pyplot.ylim(0,6)
##pyplot.savefig('test.png', edgecolor='none')
#pyplot.show()





#pyplot.figure(1) 
pyplot.figure(num=None, figsize=(8, 6), dpi=120, facecolor='w', edgecolor='k')
pyplot.subplot(221)
pyplot.text(2.5, 0.2, r'RT', fontsize=15)
pyplot.plot( data10nmRT[:,0], data10nmRT[:,1] , linestyle='-',color='k' , label='r = 10nm' )
pyplot.plot( data15nmRT[:,0], data15nmRT[:,1] , linestyle=':',color='k' , label='r = 15nm')
pyplot.tick_params(axis='x', labelsize=14)
pyplot.tick_params(axis='y', labelsize=14)
pyplot.xlabel('Energy[eV]', fontsize=14)
pyplot.ylabel('$ \Delta R / R$', fontsize=14)
pyplot.legend(loc='best')

pyplot.subplot(222)
pyplot.text(2.5, 2, r'60$^{\circ}$C', fontsize=15)
pyplot.plot(data10nm60C[:,0], data10nm60C[:,1], linestyle='-' ,color='k' , label='r = 10nm')
pyplot.plot(data15nm60C[:,0], data15nm60C[:,1], linestyle=':', color='k' , label='r = 15nm')
pyplot.tick_params(axis='x', labelsize=14)
pyplot.tick_params(axis='y', labelsize=14)
pyplot.xlabel('Energy[eV]', fontsize=14)
pyplot.ylabel('$ \Delta R / R$', fontsize=14)
pyplot.legend(loc='best')

pyplot.subplot(223)
pyplot.text(2.5, 3, r'65$^{\circ}$C', fontsize=15)
pyplot.plot(data10nm65C[:,0], data10nm65C[:,1], linestyle='-',color='k' , label='r = 10nm')
pyplot.plot(data15nm65C[:,0], data15nm65C[:,1], linestyle=':', color='k', label='r = 15nm')
pyplot.tick_params(axis='x', labelsize=14)
pyplot.tick_params(axis='y', labelsize=14)
pyplot.xlabel('Energy[eV]', fontsize=14)
pyplot.ylabel('$ \Delta R / R$', fontsize=14)
pyplot.legend(loc='best')

pyplot.subplot(224)
pyplot.text(2.5, 1, r'80$^{\circ}$C', fontsize=15)
pyplot.plot(data10nm80C[:,0], data10nm80C[:,1], linestyle='-' ,color='k' , label='r = 10nm')
pyplot.plot(data15nm80C[:,0], data15nm80C[:,1], linestyle=':', color='k' , label='r = 15nm')
pyplot.tick_params(axis='x', labelsize=14)
pyplot.tick_params(axis='y', labelsize=14)
pyplot.xlabel('Energy[eV]', fontsize=14)
pyplot.ylabel('$ \Delta R / R$', fontsize=14)
pyplot.legend(loc='best')


#pyplot.savefig('test.png', edgecolor='none')
pyplot.show()
