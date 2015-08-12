#!/usr/bin/python
import numpy
from scipy.interpolate import interp1d 
from scipy.interpolate import griddata

skipRows = 1 #input for numpy.loadtxt(). Skips the first row with the top information
valueDelimiter = ',' # input for numpy.loadtxt(). Tells which symbol that separates the data


def find_nearest(array,value):
    i = (numpy.abs(array-value)).argmin()
    return i

#Of the largest min-values in the two arrays, this functions finds the index in the other array
# corresponding to this value:
def find_min_x(x1, x2): 
    if( x1[0] < x2[0] ):
        imin = find_nearest(x1, x2[0])
        if( x1[imin] < x2[0] ):
            imin = imin + 1
            return x1[imin]
        else: 
            return x2[0]
    else:
        imin = find_nearest(x2, x1[0])
        if( x2[imin] < x1[0]):
            imin = imin + 1
            return x2[imin]
        else:
            return x1[0]


def find_max_x(x1, x2): 
    if( x1[-1] < x2[-1] ):
        imax = find_nearest(x2, x1[-1])
        if( x1[-1] < x2[imax] ):
            imax = imax - 1
            return x2[imax]
        else: 
            return x1[-1]
    else:
        imax = find_nearest(x1, x2[-1])
        if( x2[-1] < x1[imax] ):
            imax = imax - 1
            return x1[imax]
        else: 
            return x2[-1]




## TESTING THE FIND FUNCTIONS:
#import matplotlib.pyplot as pyplot
#x1 = numpy.linspace(0.33,8.5,num=20)
#x2 = numpy.linspace(0.12,9.9,num=24)

#print 'nearest to 4.9 in the arrays x1 and x2 is:' 
#nearest1 = find_nearest(x1,4.9)
#nearest2 = find_nearest(x2,4.9)
#print nearest1
#print nearest2
#print 'respectively.\n'

#print 'The lowest x-value is:'
#mini = find_min_x(x1,x2)
#print mini
#print 'The largest x-value is:'
#maxi = find_max_x(x1,x2)
#print maxi
#n = 30+ x1+ 3 - x1**2
#k = x2**2 - x2**3

#pyplot.figure(1)
#pyplot.axvline(x2[0], color='g')
#pyplot.axvline(x1[-1],color='g')
#pyplot.plot(x1,n)
#pyplot.axvline(mini, color='r')
#pyplot.plot(x2,k)
#pyplot.axvline(maxi, color='r')
#pyplot.show()

# Trying to first convert the 300 K temperature data for both n and k:

unit = 2 #OUTPUT-LINE-1
nData = numpy.loadtxt('reOpticalIndexVO2.csv', skiprows=skipRows, delimiter=valueDelimiter)
kData = numpy.loadtxt('imOpticalIndexVO2.csv', skiprows=skipRows, delimiter=valueDelimiter)

# Find lowest x-value which are in both datasets:
x1 = find_min_x(nData[0], kData[0])

# Find largest -value which are in both datasets:
x2 = find_max_x(nData[0], kData[0])

print 'number of file data points:'
print len(nData[:,0])
print len(kData[:,0])

#Not sure about the number of data points so I just use the lowest of the two:
dataPoints = 150 #OUTPUT-LINE-1

firstLine = str(unit)+'\t'+str(x1)+'\t'+str(x2)+'\t'+str(dataPoints)  


# Interpolate the data such that we get the n,k values for the same equidistant x-values:
x = numpy.linspace(x1,x2, num=dataPoints, endpoint=True) #equidistant x-values
#try 1:
#n = interp1d(x,nData[:,1])
#k = interp1d(x,kData[:,1])
#try 2:
n = griddata(nData[:,0], nData[:,1], x, method='linear')
k = griddata(kData[:,0], kData[:,1], x, method='linear')

print 'n-data:'
print len(nData[:,0]) 
print len(nData[:,1])
print '\nk-data:'
print len(kData[:,0])
print len(kData[:,1])

#write to file
numpy.savetxt('savetxtOutput.nk', numpy.column_stack((n,k)), delimiter='\t', header=firstLine, comments='')
#comments='' because if not, then the header would be a comment and contain the #-symbol


