#!/usr/bin/env python
import numpy
import matplotlib.pyplot as pyplot

import sys, getopt
import numpy
import matplotlib.pyplot as pyplot

skipRows = 0 # Number of rows to skip in the files read. (input agrument to numpy.loadtxt())
dataDelimiter=None # What separates the file values. (input arg to numpy.loadtxt(),default = any whitespace)

print 'args',sys.argv

args = sys.argv[1:]
flags = 'hi:'
try:
    flags, args = getopt.getopt(args, flags)
except getopt.GetoptError:  
    print 'Error, mismatch between given input arguments and the input options!'          
    print 'Help: ./plotdR.py -h'          
    sys.exit(2)


#print sys.argv #DEBUG

# Loop Through Input (Flags,Arguments)
for flag, arg in flags:
    # If Help Flag -> Show Instructions: #
    if flag == '-h':
        print 'Format:                                     '
        print "     -i  <inputfile>,<inputfile>,...        " 
        print 'Run:                                        '
        print '     ./plotdR.py -i ifile1.txt,ifile2.dat'
        sys.exit()

    # Read input files:
    elif flag in ('-i','--ifile'):
        ifiles = arg.split(',') #input files

        # Getting different line types
        from itertools import cycle
        lines = ["-","--","-.",":"]
        linecycler = cycle(lines)

        #pyplot.figure(1) 
        pyplot.figure(num=None, figsize=(8, 6), dpi=120, facecolor='w', edgecolor='k')
        for ifile in ifiles:
            ifileNameTemperature = ifile.split('/')[-1].split('.')[0].split('_')[-1] #get data temperature
            data = numpy.loadtxt(ifile, skiprows=skipRows, delimiter = dataDelimiter) # Load file data
            #pyplot.plot(data[:,0], data[:,1], next(linecycler), color='k' ,label='$\Delta R/R$ '+str(ifileNameTemperature))
            pyplot.plot(data[:,0], data[:,1], label=str(ifileNameTemperature))
            pyplot.legend(loc='best')

pyplot.tick_params(axis='x', labelsize=16)
pyplot.tick_params(axis='y', labelsize=16)
pyplot.xlabel('Energy[eV]', fontsize=16)
pyplot.ylabel('$ \Delta R / R$', fontsize=16)
#pyplot.ylim(0,6)
#pyplot.savefig('test.png', edgecolor='none')
pyplot.show()
