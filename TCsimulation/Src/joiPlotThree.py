#!/usr/bin/python
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ## # # # # # # # # # # # # # # # # # # # # # #
# Author: Jorgen Vagan                                                                                     #
# joiPlot.py | Joined data plot: A simple generic plot function, which takes multiple file input and       #
#                                and plots it according to instructions into a single figure. Assumes      #
#                                inputfile with data in columns: column0,column1,column2,colum3,..etc.     #
#                                Plots Y as a function of X, where X and Y are data-columns.               #
#                                                                                                          #
# Format:                                                                                                  #
#           -i  <inputfile>,<inputfile>,(s=int),(d='char'),X1,Y1,X2,Y2,etc..., where y1(x1), y2(x2), etc...#
# Run:                                                                                                     #
#           ./joiPlot.py -i ifile1.txt,ifile2.dat,s=2,d=.,0,1 -i ifile2.dat,2,3,4,5                        #
# Help:                                                                                                    #
#           ./joiPlot.py -h                                                                                #
#                                                                                                          #
# (change mode; script to executable: chmod +x joiPlot.py)                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ## # # # # # # # # # # # # # # # # # # # # # #

# NOOOOTE: if delimiter is whitespace, then don't add any spesific delimiter. leave it out!!

#!!!!!!! Due to that ',' splits the different input values for each input flat -i and that 
#   the delimiter input option has ',' as input, the special case has been formed: that 'comma' marks the delimiter!
#   e.g. s=1,d=comma,X1,Y1,etc...

import sys, getopt
import numpy
import matplotlib.pyplot as pyplot

skipRows = 0 #number of rows to skip in the files read. (input agrument to numpy.loadtxt())
dataDelimiter=None #what separates the values in the file. (input arg to numpy.loadtxt(),default = any whitespace)

if len(sys.argv) < 2:
    print 'ERROR! Too few arguments!                                                                      '
    print 'joiPlot.py | Joined data plot: A simple generic plot function, which takes multiple file input and'
    print '                               and plots it according to instructions into a single figure. Assumes '
    print '                               inputfile with data in columns: column0,column1,column2,colum3,..etc.'
    print '                               Plots Y as a function of X, where X and Y are data-columns.          '
    print '                                                                                                    '
    print 'Format:                                                                                             '
    print "     -i  <inputfile>,<inputfile>,(s=int),(d='char'),X1,Y1,X2,Y2,etc..., where y1(x1), y2(x2), etc..."
    print 'Run:                                                                                                '
    print '     ./joiPlot.py -i ifile1.txt,ifile2.dat,s=2,d=.,0,1 -i ifile2.dat,2,3,4,5                   '
    sys.exit()

args = sys.argv[1:]
flags = 'hi:'
try:
    flags, args = getopt.getopt(args, flags)
except getopt.GetoptError:  
    print 'Error, mismatch between given input arguments and the input options!'          
    print 'Help: ./joiPlot.py -h'          
    sys.exit(2)

# Make Figure #
pyplot.figure(1)

# Loop Through Input (Flags,Arguments)
for flag, arg in flags:
    # If Help Flag -> Show Instructions: #
    if flag == '-h':
        print 'HELP:'
        print 'joiPlot.py | A simple generic plot function, which takes multiple file input and'
        print '             and plots it according to instructions into a single figure. Assumes '
        print '             inputfile with data in columns: column0,column1,column2,colum3,..etc.'
        print '             Plots Y as a function of X, where X and Y are data-columns.          '
        print '                                                        '
        print 'Format:                                                                         '
        print "  -i  <inputfile>,<inputfile>,(s=int),(d='char'),X1,Y1,X2,Y2,etc..., where y1(x1), y2(x2), etc..."
        print 'Run:                                                                '
        print '  ./joiPlot.py -i ifile1.txt,ifile2.dat,s=2,d=.,0,1 -i ifile2.dat,2,3,4,5                   '
        print '  '
        print '        (arguments for input-flag "-i" must be separated by commas and contain no spaces!)'
        print '  '
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
        for i in range(len(subArgs)):
            print i
            if( subArgs[i][0:2] == 's='):
                skipSpecified = 1
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
                delimSpecified = 1
                # extract the delimiter:
                #extracted = False;
                if(len(subArgs[i]) < 7): # if not special case below
                    dataDelimiter = subArgs[i][2:len(subArgs[i])]
                    #extracted = True
                # SPECIAL CASE FOR COMMA UNTIL BETTER SOLUTION IS FOUND (DUE TO arg.split(',') ABOVE!)
                elif(len(subArgs[i]) == 7):
                    if(subArgs[i][2:7] == 'comma'):
                        dataDelimiter = ','
                        #extracted = True
                #if(not extracted):
                    #print 'delimiter argument not correctly given! write e.g.: d=,'          
                    #sys.exit()

            # Continue in Finding The First Intruction Index... #
            if subArgs[i].isdigit():
                iFirstInstruction = i   
                break
        #If the number of instructions is not an even number: #
        if len(subArgs[iFirstInstruction:])%2 != 0: 
            print 'Error, The plot instructions ",X1,Y1,X2,Y2,..." must be an even number! '
            print '-> every X needs a Y: e.g. <inputfile>,X1,Y1,X2,Y2'
            sys.exit(2)

        # Plot Data in "-i arguments": #
        for i in range(iFirstInstruction-skipSpecified-delimSpecified): # For all input files in input flag "-i" #   
                                                                        # (Last file is at position iFirstIntruction-skipSpecified-delimSpecified 
            data = numpy.loadtxt(subArgs[i], skiprows=skipRows, delimiter = dataDelimiter) # Load File Name #
            for j in range(iFirstInstruction, len(subArgs), 2): # Plot File Data According to Instructions: #
                pyplot.plot( data[:,int(subArgs[j])], data[:,int(subArgs[j+1])], label='y'+subArgs[j+1]+'(x'+subArgs[j]+'), '+ subArgs[i])

# Set Axis Labels, Set Legend Labels, Show Plots: #
pyplot.xlabel("x", fontsize=18)
pyplot.ylabel("y", fontsize=18)
pyplot.legend(loc='best')
pyplot.show()

