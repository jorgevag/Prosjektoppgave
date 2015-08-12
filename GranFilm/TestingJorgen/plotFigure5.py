#!/usr/bin/env python
import numpy
import matplotlib.pyplot as pyplot

data5 = numpy.loadtxt('Figure5/5.dat')
data5Ref = numpy.loadtxt('Figure5/5.dat_RefTrans')

fig = pyplot.figure(1)
subfig = fig.add_subplot(221)
subfig.set_xlabel("x")
subfig.set_ylabel("y")
#subfig.plot(data5[:,0], data5[:,1], label='dR/R')
subfig.plot(data5Ref[:,0], numpy.sqrt(data5Ref[:,1]**2 + data5Ref[:,2]**2), label='R_p')
subfig.plot(data5Ref[:,0], numpy.sqrt(data5Ref[:,9]**2 + data5Ref[:,10]**2), label='T_p')
subfig.plot(data5Ref[:,0], 1-numpy.sqrt(data5Ref[:,9]**2 + data5Ref[:,10]**2)-numpy.sqrt(data5Ref[:,1]**2 + data5Ref[:,2]**2), label='1-T_p-R_p')
leg = subfig.legend(loc='best')
subfig = fig.add_subplot(222)
subfig.set_xlabel("x")
subfig.set_ylabel("y")
subfig.plot(data5Ref[:,0], numpy.sqrt(data5Ref[:,5]**2 + data5Ref[:,6]**2), label='R_s')
subfig.plot(data5Ref[:,0], numpy.sqrt(data5Ref[:,13]**2 + data5Ref[:,14]**2), label='T_s')
subfig.plot(data5Ref[:,0], 1-numpy.sqrt(data5Ref[:,5]**2 + data5Ref[:,6]**2)-numpy.sqrt(data5Ref[:,13]**2 + data5Ref[:,14]**2), label='1-T_s-R_s')
leg = subfig.legend(loc='best')

pyplot.show()

