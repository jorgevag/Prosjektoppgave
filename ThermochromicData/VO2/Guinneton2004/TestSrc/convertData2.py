#!/usr/bin/python
import matplotlib.pyplot as pyplot #just for testing program
import sys
import numpy
import scipy.interpolate

# Load data files
skipRows = 1 #input for numpy.loadtxt(). Skips the first row with the top information
valueDelimiter = ',' # input for numpy.loadtxt(). Tells which symbol that separates the data
nData = numpy.loadtxt('../reOpticalIndexVO2.csv', skiprows=skipRows, delimiter=valueDelimiter)
kData = numpy.loadtxt('../imOpticalIndexVO2.csv', skiprows=skipRows, delimiter=valueDelimiter)

unit = 2 # data unit is wavelength THIS SHOULD BE TAKIN IN THROUGH COMMAND LINE

temperatures = len(nData[0,:])
if( temperatures != len(kData[0,:]) ):
    print 'The number of columns in the n-datafile is different from the number of columns in the k-datafile'
    sys.exit()

# find range 
x_min =  numpy.max( numpy.min(nData[:,0]),numpy.min(kData[:,0]) )
x_max =  numpy.min( numpy.max(nData[:,0]),numpy.max(kData[:,0]) )

#Not sure about the number of data points so I just use the lowest of the two:
#dataPoints = numpy.min( [len(nData[:,0]), len(kData[:,0])] ) 
dataPoints = numpy.min( [len(nData[:,0]), len(kData[:,0])] ) 


# Interpolate the data such that we get the n,k values for the same equidistant x-values:
x = numpy.linspace(x_min,x_max, num=dataPoints, endpoint=True) #equidistant x-values


for T in xrange(1,temperatures):
    print T
    # interpolation 
    n_interpolator = scipy.interpolate.interp1d(nData[:,0], nData[:,T])
    k_interpolator = scipy.interpolate.interp1d(kData[:,0], kData[:,T])
    
    n = n_interpolator(x)
    k = k_interpolator(x)
    
    # write to file (note that the x-values are converted to micrometers)
    firstLine = str(unit)+'\t'+str(x_min*10**6)+'\t'+str(x_max*10**6)+'\t'+str(dataPoints)  
    numpy.savetxt('InputOutput/vo2_T'+str(T)+'.nk', numpy.column_stack((n,k)), delimiter='\t', header=firstLine, comments='')

    pyplot.figure(1)
    pyplot.plot(x,n,x,k)
    pyplot.show()
    
    
## Debug
#print ' Found the following range '
#print '    x_min : %e %e %e' % (x_min, numpy.min(nData[:,0]),numpy.min(kData[:,0]) ) 
#print '    x_max : %e %e %e' % (x_max, numpy.max(nData[:,0]),numpy.max(kData[:,0]) )  


