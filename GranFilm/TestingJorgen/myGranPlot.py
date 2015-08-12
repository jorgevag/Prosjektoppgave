#!/usr/bin/env python
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ## # # # # # # # # # # # # # # # # # # # # 
# Author: Jorgen Vagan                                                                                 #
# mypyplot.py: an attempt for a simple generic plot function.                                          #
# This function plots a file given as an argument:                                                     #
# The file should have the data in columns on the form: (X, Y1, Y2,...) where y1(x), y2(x), etc...     #
# Input Arguments:                                                            argv-index:              #
#   1.  number of files: e.g.    2                                                1                    #
#   2.  inputfiles:   e.g. file1.dat file2.dat                                2,...,2+Nfiles-1         #
#   3.  plotTyoe:    '-s' (separate), '-t' (together)                           2+Nfiles               #
#   3.  What to plot:    'all' or column spesification: '3' '5'                 3+Nfiles,...           #
#                                                  plots column 3 and 5                                #
# Example 1: ./mypyplot file1.dat file2.dat file3.dat -t -all                                          #
# Example 2: ./mypyplot file.dat -t 2 4                                                                #
# (change mode; script to executable: chmod +x mypyplot.py)                                               #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ## # # # # # # # # # # # # # # # # # # # # 
import os,sys
#from scipy import stats #DO I NEED THIS ????????????????????????????????????????????????????????????
import numpy as np
import matplotlib.pyplot as plt


# # # # # # # # # # # # # # # #
# Begin Input Parameter Check #
argc = len(sys.argv) # sys.argv is a list/array of the input parameters/arguments from command line/ terminal

# Check If Number Of files are Spesified #
if (sys.argv[1].isdigit()):
    Nfiles = int(sys.argv[1])
else: 
    print 'ERROR, WRONG INPUT PARAMETERS!'
    print 'Input parametersMust be on the form:' 
    print '\t 1. an int: "3"'
    print '\t 2. "file.dat" or "file.txt"'
    print '\t 3. "-a" or "-t"'
    print '\t 4. "-all" or spesify columns to plot, e.g. "2 5 8"'
    print 'Example1: ./mypyplot file.dat -t -all \nExample2: ./mypyplot file.txt -t 2 4'
    exit(1)

# Check Number Of Arguments #
if (argc < 5):
    print 'ERROR: Need at least 3 input parameters:'
    print '\t 1. "number of files" '
    print '\t 2. "files" '
    print '\t 3."a"/"t" '
    print '\t 4. "all" or spesify columns to plot, e.g. "2 5 8"'
    exit(1)

# Check Arguments #
if (( sys.argv[2+Nfiles]!='-s' and  sys.argv[2+Nfiles]!='-t'  )or(  sys.argv[3+Nfiles]!='-all' and not(sys.argv[3+Nfiles].isdigit())  )):
    print 'ERROR, WRONG INPUT PARAMETERS!'
    print 'Input parametersMust be on the form:' 
    print '\t 1. an int: "3"'
    print '\t 2. "file.dat" or "file.txt"'
    print '\t 3. "-a" or "-t"'
    print '\t 4. "-all" or spesify columns to plot, e.g. "2 5 8"'
    print 'Example1: ./mypyplot file.dat -t -all \nExample2: ./mypyplot file.txt -t 2 4'
    exit(1)
# End Input Parameter Check #
# # # # # # # # # # # # # # #


# # # # # # # # # # # # # 
# Begin Read File Loop  #
isDeclared = 0
for file_i in range(Nfiles):
    # Read Data From Input File i #
    f=open(sys.argv[2+file_i], 'r')
    lines = f.readlines()
    f.close()

    # If this is the first run, our "tools" aren't declared, and must be declared #
    if(isDeclared == 0):
        # Get Data Dimensions #
        Nrows = len(lines)
        Ncols = len(lines[35].split())
        data = np.zeros((Nrows,Ncols,Nfiles))
        isDeclared = 1
        

    # Organize Data #
    for i in range(35,Nrows):
        line=lines[i].split()
        for j in range(Ncols):
            data[i,j,file_i] = line[j]
# End Read File Loop  #'
# # # # # # # # # # # # 


# # # # # # # # # # # # # # # # # # # # #
# Plot Data According To Spesifications #
if (sys.argv[3+Nfiles] =='-all'):
    if (sys.argv[2+Nfiles] == '-t'): # t->together, plot each column in same figure
        fig = plt.figure(1)
        ax = fig.add_subplot(111)
        ax.set_xlabel("x")
        ax.set_ylabel("y")
        for file_i in range(Nfiles):
            for i in range(1,Ncols):
                ax.plot(data[:,0,file_i], data[:,i,file_i], label='y'+str(i)+', '+sys.argv[2+file_i])
        leg = ax.legend()
        plt.show()
    if (sys.argv[2+Nfiles] == '-s'): # s->separate, plot the columns in separate figures
        for i in range(1,Ncols):
            fig = plt.figure(i)
            ax = fig.add_subplot(111)
            ax.set_xlabel("x")
            ax.set_ylabel("y")
            for file_i in range(Nfiles):
                ax.plot(data[:,0,file_i], data[:,i,file_i], label='y'+str(i)+', '+sys.argv[2+file_i])
            leg = ax.legend()
        plt.show()
else:
    if (sys.argv[2+Nfiles] == '-t'): # t->together, plot each column in same figure
        fig = plt.figure(1)
        ax = fig.add_subplot(111)
        ax.set_xlabel("x")
        ax.set_ylabel("y")
        for file_i in range(Nfiles):
            for i in range(3+Nfiles,argc):
                #print'argv=', sys.argv, '\nfile_i=',file_i, ',i=', i, 'sys.argv[i] =', sys.argv[i], 'int(sys.argv[i]) =', int(sys.argv[i]) # DEBUG
                ax.plot(data[:,0,file_i], data[:,int(sys.argv[i]),file_i], label='y'+ sys.argv[i] + ', '+sys.argv[2+file_i])
        leg = ax.legend()
        plt.show()
    if (sys.argv[2+Nfiles] == '-s'): # s->separate, plot the columns in separate figures
        for i in range(3+Nfiles,argc):
            fig = plt.figure(i)
            ax = fig.add_subplot(111)
            ax.set_xlabel("x")
            ax.set_ylabel("y")
            for file_i in range(Nfiles):
                ax.plot(data[:,0,file_i], data[:,int(sys.argv[i]),file_i], label='y'+ sys.argv[i] + ', '+sys.argv[2+file_i])
            leg = ax.legend()
        plt.show()
