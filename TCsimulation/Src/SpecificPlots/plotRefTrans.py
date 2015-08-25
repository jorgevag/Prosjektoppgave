#!/usr/bin/env python
import numpy
import matplotlib.pyplot as pyplot

import sys, getopt
import numpy
import matplotlib.pyplot as pyplot

skipRows = 0 # Number of rows to skip in the files read. (input agrument to numpy.loadtxt())
dataDelimiter=None # What separates the file values. (input arg to numpy.loadtxt(),default = any whitespace)

plotR = False
plotT = False
plotA = False

senkrecht = False
parallel = False

if len(sys.argv) < 1:
    print 'ERROR! Need at least One input file!        '
    print '                                            '
    print 'Flag options:                               '
    print '     -i              -R      -T        -A     -s -p '
    print '    input           refl    trans   absorb   polarization'
    print '    <files>         "-----------for plotting-------------"  '
    print 'Format:                                     '
    print "     -i  <inputfile>,<inputfile>,...        " 
    print 'Run:                                        '
    print '     ./plotGFRefl.py -i ifile1.txt,ifile2.dat'
    sys.exit()

args = sys.argv[1:]
flags = 'hi:RpTpApRsTsAs'
try:
    flags, args = getopt.getopt(args, flags)
except getopt.GetoptError:  
    print 'Error, mismatch between given input arguments and the input options!'          
    print 'Help: ./joiPlot.py -h'          
    sys.exit(2)


#print sys.argv #DEBUG

# Loop Through Input (Flags,Arguments)
for flag, arg in flags:
    # If Help Flag -> Show Instructions: #
    if flag == '-h':
        print 'HELP:'
        print 'ERROR! Need at least One input file!        '
        print '                                            '
        print 'Flag options:                               '
        print '     -i              -R      -T        -A     -s -p '
        print '    input           refl    trans   absorb   polarization'
        print '    <files>         "-----------for plotting-------------"  '
        print 'Format:                                     '
        print "     -i  <inputfile>,<inputfile>,...        " 
        print 'Run:                                        '
        print '     ./plotGFRefl.py -i ifile1.txt,ifile2.dat'
        sys.exit()

    # Read input files:
    elif flag in ('-i','--ifile'):
        ifiles = arg.split(',') #input files

    elif flag in ('-R','--refl'):
        plotR = True
    elif flag in ('-T','--trans'):
        plotT = True
    elif flag in ('-A','--abs'):
        plotA = True

    elif flag in ('-p','--ppol'):
        parallel = True
    elif flag in ('-s','--spol'):
        senkrecht = True
        #for i in range(len(subArgs)): # For all input files in input flag "-i":
            #data = numpy.loadtxt(subArgs[i], skiprows=skipRows, delimiter = dataDelimiter) # Load File Name #
            #pyplot.plot( data[:,int(subArgs[j])], data[:,int(subArgs[j+1])], label='y'+subArgs[j+1]+'(x'+subArgs[j]+'), '+ subArgs[i])

print 'ifiles:',ifiles
print 'senkrecht:',senkrecht
print 'parallel:',parallel

if(parallel):
    if(plotR):
        pyplot.figure(1) # Reflection figure
        for ifile in ifiles:
            ifileNameTemperature = ifile.split('/')[-1].split('.')[0].split('_')[-1] #get data temperature
            data = numpy.loadtxt(ifile, skiprows=skipRows, delimiter = dataDelimiter) # Load file data
            pyplot.plot(data[:,0], data[:,1]**2 + data[:,2]**2, label='$R_p$ '+str(ifileNameTemperature))
            pyplot.legend(loc='best')

    if(plotT):
        pyplot.figure(2) # Transmission figure
        for ifile in ifiles:
            ifileNameTemperature = ifile.split('/')[-1].split('.')[0].split('_')[-1] #get data temperature
            data = numpy.loadtxt(ifile, skiprows=skipRows, delimiter = dataDelimiter) # Load file data
            pyplot.plot(data[:,0], data[:,9]**2 + data[:,10]**2, label='$T_p$ '+str(ifileNameTemperature))
            pyplot.legend(loc='best')

    if(plotA):
        pyplot.figure(3) # Absorbtion figure ##IS THIS CORRECT?? IS 1-R-T = A???
        for ifile in ifiles:
            ifileNameTemperature = ifile.split('/')[-1].split('.')[0].split('_')[-1] #get data temperature
            data = numpy.loadtxt(ifile, skiprows=skipRows, delimiter = dataDelimiter) # Load file data
            pyplot.plot(data[:,0], 1-(data[:,9]**2 + data[:,10]**2)-(data[:,1]**2 + data[:,2]**2), label='1-$T_p$-$R_p$ '+str(ifileNameTemperature))
            pyplot.legend(loc='best')


if(senkrecht):
    if(plotR):
        pyplot.figure(4) # Reflection figure
        for ifile in ifiles:
            ifileNameTemperature = ifile.split('/')[-1].split('.')[0].split('_')[-1] #get data temperature
            data = numpy.loadtxt(ifile, skiprows=skipRows, delimiter = dataDelimiter) # Load file data
            pyplot.plot(data[:,0], (data[:,5]**2 + data[:,6]**2), label='$R_s$ '+str(ifileNameTemperature))
            pyplot.legend(loc='best')

    if(plotT):
        pyplot.figure(5) # Transmission figure
        for ifile in ifiles:
            ifileNameTemperature = ifile.split('/')[-1].split('.')[0].split('_')[-1] #get data temperature
            data = numpy.loadtxt(ifile, skiprows=skipRows, delimiter = dataDelimiter) # Load file data
            pyplot.plot(data[:,0], (data[:,13]**2 + data[:,14]**2), label='$T_s$ '+str(ifileNameTemperature))
            pyplot.legend(loc='best')

    if(plotA):
        pyplot.figure(6) # Absorbtion figure ##IS THIS CORRECT?? IS 1-R-T = A???
        for ifile in ifiles:
            ifileNameTemperature = ifile.split('/')[-1].split('.')[0].split('_')[-1] #get data temperature
            data = numpy.loadtxt(ifile, skiprows=skipRows, delimiter = dataDelimiter) # Load file data
            pyplot.plot(data[:,0], 1-(data[:,5]**2 + data[:,6]**2)-(data[:,13]**2 + data[:,14]**2), label='1-$T_s$-$R_s$ '+str(ifileNameTemperature))
            pyplot.legend(loc='best')


pyplot.xlabel('eV', fontsize=18)
pyplot.show()
