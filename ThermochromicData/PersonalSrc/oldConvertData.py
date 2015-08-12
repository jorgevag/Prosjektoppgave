#!/usr/bin/python
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ## # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Author: Jorgen Vagan                                                                                                   #
# convertData.py |  A program to convert files containing refractive index data (n,k) or dielectric                      #
#                   function (permittivity) data (Re{e}, Im{e}), with unmatched unequidistant data for                   #
#                   the real and imaginary data and writes it to a single file with with equidistanced,                  #
#                   matching values for the refractive index (n,k), using interpolation.                                 #
#                   The function assumes one or two files. Example of the required and optional input                    #
#                   is shown below.                                                                                      #
#                                                                                                                        #
# Format:                                                                                                                #
#           -i  <realDataFile>,<imaginaryDataFile>,(s=int),(d='char'),unit=string,isPermittivity=bool,X_Re,Y_Re,X_Im,Y_Im#
#            where y1(x1), y2(x2), etc...                                                                                #
# Examples:                                                                                                              #
#           ./convertData.py -i refile.txt,imfile.dat,s=2,d=.,0,1                                                        #
#           ./convertData.py -i refile.txt,s=2,d=.,0,1  -i imfile.dat,s=2,d=comma,0,4                                    #
# Help:                                                                                                                  #
#           ./convertData.y -h                                                                                           #
#                                                                                                                        #
# (change mode; script to executable: chmod +x convertData.py)                                                           #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ## # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# NOOOOTE: if delimiter is whitespace, then don't add any spesific delimiter. leave it out!!

#!!!!!!! Due to that ',' splits the different input values for each input flat -i and that 
#   the delimiter input option has ',' as input, the special case has been formed: that 'comma' marks the delimiter!
#   e.g. s=1,d=comma,X1,Y1,etc...

import sys, getopt
import numpy
import scipy.interpolate
import matplotlib.pyplot as pyplot



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # #             TAKING INPUT FROM THE COMMAND LINE AND READING DATA                     # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

skipRows = 0 #number of rows to skip in the files read. (input agrument to numpy.loadtxt())
dataDelimiter=None #what separates the values in the file. (input arg to numpy.loadtxt(),default = any whitespace)
unit = 0

# The data has not yet been read: #
loadedRealData = False
loadedImaginaryData = False


if len(sys.argv) < 2:
    print 'convertData.py |  A program to convert files containing refractive index data (n,k) or dielectric                      '
    print '                  function (permittivity) data (Re{e}, Im{e}), with unmatched unequidistant data for                   '
    print '                  the real and imaginary data and writes it to a single file with with equidistanced,                  '
    print '                  matching values for the refractive index (n,k), using interpolation.                                 '
    print '                  The function assumes one or two files. Example of the required and optional input                    '
    print '                  is shown below.                                                                                      '
    print '                                                                                                                       '
    print 'Format:                                                                                                                '
    print "          -i  <realDataFile>,<imaginaryDataFile>,(s=int),(d='char'),unit=string, isPermittivity=bool,X1,Y1,X2,Y2,etc..."
    print '           where y1(x1), y2(x2), etc...                                                                                '
    print 'Examples:                                                                                                              '
    print '          ./convertData.py -i refile.txt,imfile.dat,s=2,d=.,0,1                                                        '
    print '          ./convertData.py -i refile.txt,s=2,d=.,0,1  -i imfile.dat,s=2,d=comma,0,4                                    '
    print 'Help:                                                                                                                  '
    print '          ./convertData.y -h                                                                                           '
    sys.exit()

args = sys.argv[1:]
flags = 'hi:'
try:
    flags, args = getopt.getopt(args, flags)
except getopt.GetoptError:  
    print 'Error, mismatch between given input arguments and the input options!'          
    print 'Help: ./joiPlot.py -h'          
    sys.exit(2)

# Loop Through Input (Flags,Arguments)
for flag, arg in flags:
    # If Help Flag -> Show Instructions: #
    if flag == '-h':
        print 'HELP:'
        print 'convertData.py |  A program to convert files containing refractive index data (n,k) or dielectric                      '
        print '                  function (permittivity) data (Re{e}, Im{e}), with unmatched unequidistant data for                   '
        print '                  the real and imaginary data and writes it to a single file with with equidistanced,                  '
        print '                  matching values for the refractive index (n,k), using interpolation.                                 '
        print '                  The function assumes one or two files. Example of the required and optional input                    '
        print '                  is shown below.                                                                                      '
        print '                                                                                                                       '
        print 'Format:                                                                                                                '
        print "          -i  <realDataFile>,<imaginaryDataFile>,(s=int),(d='char'),unit=string, isPermittivity=bool,X1,Y1,X2,Y2,etc..."
        print '           where y1(x1), y2(x2), etc...                                                                                '
        print 'Examples:                                                                                                              '
        print '          ./convertData.py -i refile.txt,imfile.dat,s=2,d=.,0,1                                                        '
        print '          ./convertData.py -i refile.txt,s=2,d=.,0,1  -i imfile.dat,s=2,d=comma,0,4                                    '
        print 'Help:                                                                                                                  '
        print '          ./convertData.y -h                                                                                           '
        sys.exit()

    # If Input Flag -> Check and Sort InputArguments and Plot Data According to Instructions #
    elif flag in ('-i','--ifile'):
        # Get subArguments, they should be separated by ","
        subArgs = arg.split(',')
        # Input Check:
        if subArgs[0].isdigit() and subArgs[0] == 0:
            print 'Error, First argument for input flag "-i" must be a filename!'          
            sys.exit(2)
        # Find First Intruction Index, So We Know Where The Filenamess And Plot-Instructions Are: #
        iFirstInstruction = 1
        #  ...at the same check if a skip row value or delimiter has been specified: #
        skipSpecified = 0
        delimSpecified = 0
        unitSpecified = 0
        permittivitySpecified = 0
        for i in range(len(subArgs)):
            if( subArgs[i][0:2] == 's='):
                skipSpecified = 1 # skiprows argument given
                # extract the integer:
                extracted = False;
                for char in subArgs[i]:
                    if(char.isdigit()):
                        skipRows = int(char)
                        extracted = True
                if(not extracted):
                    print 'skip rows argument not correctly given! write e.g.: s=1     ...and use int, not double!'
                    sys.exit()

            if( subArgs[i][0:2] == 'd='):
                delimSpecified = 1 # delimiter argument given
                # extract the delimiter:
                if(len(subArgs[i]) < 7): # if not special case below
                    dataDelimiter = subArgs[i][2:len(subArgs[i])]
                # SPECIAL CASE FOR COMMA UNTIL BETTER SOLUTION IS FOUND (DUE TO arg.split(',') ABOVE!)
                elif(len(subArgs[i]) == 7):
                    if(subArgs[i][2:7] == 'comma'):
                        dataDelimiter = ','

            # Read unit:
            if( subArgs[i][0:5] == 'unit='):
                unitSpecified = 1 # unit argument given
                #if( subArgs[i][5:].isalpha() ):
                    #unit = subArgs[i][5:]
                if( subArgs[i][5:] == 'energy'       or   subArgs[i][5:] == 'eV' ):
                    unit = 1
                elif( subArgs[i][5:] == 'micrometers'  or   subArgs[i][5:] == 'microm' or subArgs[i][5:] == 'mum'):
                    unit = 2
                elif( subArgs[i][5:] == 'nanometers'   or   subArgs[i][5:] == 'nm' ):
                    unit = 3
                elif( subArgs[i][5:] == 'meters'       or   subArgs[i][5:] == 'm' ):
                    unit = 4
                else:
                    print "The unit must be spesified by a string of characters: "
                    print "  unit='energy'/'eV'/'nanometers'/'nm'/'micrometers'/'microm'/'mum'/'meters'/'m'"
                    sys.exit()

            # Read if permittivity
            if( subArgs[i][0:15] == 'isPermittivity='):
                permittivitySpecified = 1 # permittivity argument given
                if( subArgs[i][15:] == '1' or subArgs[i][15:] == 'true' or subArgs[i][15:] == 'True' or subArgs[i][15:] == 'TRUE'):
                    isPermittivity = 1
                elif( subArgs[i][15:] == '0' or subArgs[i][15:] == 'false' or subArgs[i][15:] == 'False' or subArgs[i][15:] == 'FALSE'):
                    isPermittivity = 0
                else:
                    print "'isPermittivity='-parameter is not correctly stated, see below for correct input:"
                    print "  isPermittivity='1'/'0'/'true'/'false'/'True'/'False'/'TRUE'/'FALSE'"
                    sys.exit()

            # Continue in Finding The First Intruction Index... #
            if subArgs[i].isdigit():
                iFirstInstruction = i   
                break
        #If the number of instructions is not an even number: #
        if len(subArgs[iFirstInstruction:])%2 != 0: 
            print 'Error, The plot instructions ",X1,Y1,X2,Y2,..." must be an even number! '
            print '-> every X needs a Y: e.g. <inputfile>,X1,Y1,X2,Y2'
            sys.exit(2)


        numberOfFiles = iFirstInstruction-skipSpecified-delimSpecified-unitSpecified-permittivitySpecified
        #if(numberOfFiles > 2):
            #print 'Error, There can maximally be two files! One for the real values and one for the imaginary values:'
            #print '-i <n-datafile>,<k-datafile>,...,X,Y       or         -i <n-datafile>,...,X1,Y1  -i <k-datafile>,...,X2,Y2'
            #sys.exit(2)

        ##DEBUG STUFF:
        #print 'unit='+str(unit)
        #print 'isPermittivity='+str(isPermittivity)
        #print '# of files='+str(numberOfFiles)

        for i in range(numberOfFiles): # For all input files in input flag "-i" #   
            if( not loadedRealData ):
                realData = numpy.loadtxt(subArgs[i], skiprows=skipRows, delimiter = dataDelimiter) # Load File Name #
                x_re = subArgs[iFirstInstruction+i*2] # Get index of x-value data in the realData file
                y_re = subArgs[iFirstInstruction+1+i*2] # Get index of y-value data in the realData file
                loadedRealData = True

                realUnit = unit
                realIsPermittivity = isPermittivity

                #iFirstInstruction = iFirstInstruction + 2 # shift instruction if both real and imag data is included in one file

            if( loadedRealData and not loadedImaginaryData):
                imaginaryData = numpy.loadtxt(subArgs[i], skiprows=skipRows, delimiter = dataDelimiter) # Load File Name #
                x_im = subArgs[iFirstInstruction+i*2] # Get index of x-value data in the realData file
                y_im = subArgs[iFirstInstruction+1+i*2] # Get index of y-value data in the realData file
                loadedRealData = True

                imaginaryUnit = unit
                imaginaryIsPermittivity = isPermittivity

## DEBUG STUFF:
#print 'x_re='+str(x_re)
#print 'y_re='+str(y_re)
#print 'x_im='+str(x_im)
#print 'y_im='+str(y_im)
#print 'realUnit='+str(realUnit)
#print 'imaginaryUnit='+str(imaginaryUnit)
#print 'realIsPermittivity='+str(realIsPermittivity)
#print 'imaginaryIsPermittivity='+str(imaginaryIsPermittivity)


if(realUnit != imaginaryUnit):
    print 'Error, Unit missmatch! '
    print 'As to now, no solution for this has been made, though it should be possible to'
    print 'convert the units before interpolation. These seemed however, at least at first '
    print 'thought, as a cumbersome thing to do and not very likely to occur...'
    print 'Try to fix if necessary!'
    print "For now, I'll just assume that the units must be equal..."
    print '     . . . '
    sys.exit(2)
else:
    unit = realUnit

if( realIsPermittivity != imaginaryIsPermittivity):
    print 'Error, Accoarding to your input parameters the one dataset is refraktive index'
    print "while the other is permittivity! This is not handled by the program!"
    print "Add the adjustment if necessary, if not use the same optical sizes!"
    print "Either       n and k        or          Re{e} and Im{e}"
    sys.exit(2)
else:
    isPermittivity = realIsPermittivity

# ...................................................................
# The datafiles have now been read and it is time to convert the data...




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # #                    EQUIDISTANCING DATA THROUGH INTERPOLATION                        # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# find range 
x_min =  numpy.max([ numpy.min(realData[:,x_re]),numpy.min(imaginaryData[:,x_im]) ])
x_max =  numpy.min([ numpy.max(realData[:,x_re]),numpy.max(imaginaryData[:,x_im]) ])

#Not sure about the number of data points so I just use the lowest of the two:
dataPoints = numpy.min( [len(realData[:,x_re]), len(imaginaryData[:,x_im])] ) 
# I don't know how many points I shoul have.. 
# to prevent that any information is lost, I'll at least double the number of points:
dataPoints = 2*dataPoints

# Interpolate the data such that we get the n,k values for the same equidistant x-values:
x = numpy.linspace(x_min,x_max, num=dataPoints, endpoint=True) #equidistant x-values

# interpolation 
re_interpolator = scipy.interpolate.interp1d(realData[:,x_re], realData[:,y_re])
im_interpolator = scipy.interpolate.interp1d(imaginaryData[:,x_im], imaginaryData[:,y_im])

re = re_interpolator(x)
im = im_interpolator(x)





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # #                    CONVERTING DATA  AND  CHANING UNITS                              # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# If the input data is the permittivity/dielectric function, 
# we have to convert to the refractive indicies n,k:
if( isPermittivity ): # data is permittivity
    import scipy.constants
    epsilon0 = scipy.constants.epsilon_0 # CHECK IF THIS IS THE CORRECT UNITS?! SHOULD THIS BE CONTROLLED? DOES IT VARY FROM DATA TO DATA?
    # convert from permittivity to refractive index n and absorbtion coeff k:
    absEpsilon = numpy.sqrt( re**2 + im**2)
    n = numpy.sqrt( (absEpsilon + re)/2.0*epsilon0 )
    k = numpy.sqrt( (absEpsilon - re)/2.0*epsilon0 )
else: # the real data should be n, and the imaginary data should be k:
    n = re
    k = im


# convert the x-values to the correct output units: unit=1->x[eV] or unit=2->x[micrometers]
if( unit == 1 ): # eV
    pass # Do nothing
elif(unit == 2): # micrometers
    pass # Do nothing
elif(unit == 3): # nm
    x_min = x_min*(10**(-3))
    x_max = x_max*(10**(-3))
    unit = 2
elif(unit == 4): # m
    x_min = x_min*(10**(6))
    x_max = x_max*(10**(6))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # #                            WRITING TO THE OUTPUT FILE                               # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#write to file
firstLine = str(unit)+'\t'+str(x_min)+'\t'+str(x_max)+'\t'+str(dataPoints)  
numpy.savetxt('testouput.nk', numpy.column_stack((n,k)), delimiter='\t', header=firstLine, comments='')





## Debug
#print ' Found the following range '
#print '    x_min : %e %e %e' % (x_min, numpy.min(nData[:,0]),numpy.min(kData[:,0]) ) 
#print '    x_max : %e %e %e' % (x_max, numpy.max(nData[:,0]),numpy.max(kData[:,0]) )  

import matplotlib.pyplot as pyplot
pyplot.plot(x,n,x,k)
pyplot.show()

