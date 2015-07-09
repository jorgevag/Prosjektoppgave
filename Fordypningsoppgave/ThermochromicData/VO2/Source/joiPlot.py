#!/usr/bin/python
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ## # # # # # # # # # # # # # # # # # # # # 
# Author: Jorgen Vagan                                                                                 #
# joiPlot.py | Joined data plot: A simple generic plot function, which takes multiple file input and   #
#                                and plots it according to instructions into a single figure. Assumes  #
#                                inputfile with data in columns: column0,column1,column2,colum3,..etc. #
#                                Plots Y as a function of X, where X and Y are data-columns.           #
#                                                                                                      #
# Format:                                                                                              #
#           -i  <inputfile>,<inputfile>,X1,Y1,X2,Y2,etc...    , where y1(x1), y2(x2), etc...           #
# Run:                                                                                                 #
#           ./joiPlot.py -i ifile1.txt,ifile2.dat,0,1 -i ifile2.dat,2,3,4,5                            #
# Help:                                                                                                #
#           ./joiPlot.py -h                                                                            #
#                                                                                                      #
# (change mode; script to executable: chmod +x joiPlot.py)                                             #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ## # # # # # # # # # # # # # # # # # # # # 

import sys, getopt
import numpy
import matplotlib.pyplot as pyplot

skipRows = 1 #number of rows to skip in the files read. (input agrument to numpy.loadtxt())
#valueDelimiter=None #what separates the values in the file. (input arg to numpy.loadtxt(),default = any whitespace)
valueDelimiter = ","

if len(sys.argv) < 2:
    print 'joiPlot.py | Joined Data Plot:'
    print 'Format:'           
    print '   -i  <inputfile>,<inputfile>,X1,Y1,X2,Y2,etc... , where y1(x1), y2(x2), etc...'           
    print 'Run Example:'           
    print '   ./joiPlot.py -i ifile1.txt,ifile2.dat,0,1 -i ifile2.dat,2,3,4,5                 '           
    print ' Help:              '      
    print '   ./joiPlot.py -h  '
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
        print '  Format should be on the form: -i <inputfile1>,<inputfile2>,X1,Y1,X2,Y2 '
        print '  Where figure = plot(X1-file1,Y1-file1), plot(X1-file2,Y1-file2), '
        print '   plot(X2-file1,Y2-file1), plot(X2-file2,Y2,-file2)'
        print '  '
        print '  Run Program with for example:'
        print '        ./getoptTest2.py -i <inputfile1>,<inputfile2>,3,6 etc..'
        print '        ./getoptTest2.py -i <inputfile1>,3,4 -i <inputfile2>,1,2,3,4  etc..'
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
        for i in range(len(subArgs)):
            print i
            if subArgs[i].isdigit():
                iFirstInstruction = i 
                break
        #If the number of instructions is not an even number: #
        if len(subArgs[iFirstInstruction:])%2 != 0: 
            print 'Error, The plot instructions ",X1,Y1,X2,Y2,..." must be an even number! '
            print '-> every X needs a Y: e.g. <inputfile>,X1,Y1,X2,Y2'
            sys.exit(2)
        # Plot Data in "-i arguments": #
        for i in range(iFirstInstruction): # For all input files in input flag "-i" #
            data = numpy.loadtxt(subArgs[i], skiprows=skipRows, delimiter = valueDelimiter) # Load File Name #
            for j in range(iFirstInstruction, len(subArgs), 2): # Plot File Data According to Instructions: #
                pyplot.plot( data[:,int(subArgs[j])], data[:,int(subArgs[j+1])], label='y'+subArgs[j+1]+'(x'+subArgs[j]+'), '+ subArgs[i])

# Set Axis Labels, Set Legend Labels, Show Plots: #
pyplot.xlabel("x")
pyplot.ylabel("y")
pyplot.legend(loc='best')
pyplot.show()
