#!/usr/bin/python
import numpy
import scipy.interpolate


skipRows = 1 #input for numpy.loadtxt(). Skips the first row with the top information
valueDelimiter = ',' # input for numpy.loadtxt(). Tells which symbol that separates the data


# Trying to first convert the 300 K temperature data for both n and k:
unit = 2 #OUTPUT-LINE-1
nData = numpy.loadtxt('../reOpticalIndexVO2.csv', skiprows=skipRows, delimiter=valueDelimiter)
kData = numpy.loadtxt('../imOpticalIndexVO2.csv', skiprows=skipRows, delimiter=valueDelimiter)

# find range 
x_min =  numpy.max( numpy.min(nData[:,0]),numpy.min(kData[:,0]) )
x_max =  numpy.min( numpy.max(nData[:,0]),numpy.max(kData[:,0]) )

#Not sure about the number of data points so I just use the lowest of the two:
dataPoints = numpy.min( [len(nData[:,0]), len(kData[:,0])] ) 


# Interpolate the data such that we get the n,k values for the same equidistant x-values:
x = numpy.linspace(x_min,x_max, num=dataPoints, endpoint=True) #equidistant x-values


# interpolation 
n_interpolator = scipy.interpolate.interp1d(nData[:,0], nData[:,1])
k_interpolator = scipy.interpolate.interp1d(kData[:,0], kData[:,1])

n = n_interpolator(x)
k = k_interpolator(x)

# convert data to micrometers:
x_min = x_min*10**6
x_max = x_max*10**6

#write to file
firstLine = str(unit)+'\t'+str(x_min)+'\t'+str(x_max)+'\t'+str(dataPoints)  
numpy.savetxt('InputOutput/vo2_T1.nk', numpy.column_stack((n,k)), delimiter='\t', header=firstLine, comments='')


## Debug
#print ' Found the following range '
#print '    x_min : %e %e %e' % (x_min, numpy.min(nData[:,0]),numpy.min(kData[:,0]) ) 
#print '    x_max : %e %e %e' % (x_max, numpy.max(nData[:,0]),numpy.max(kData[:,0]) )  


import matplotlib.pyplot as pyplot
pyplot.plot(x,n,x,k)
pyplot.show()
