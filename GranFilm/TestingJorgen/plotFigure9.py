#!/usr/bin/env python
import numpy
import matplotlib.pyplot as pyplot

data9a = numpy.loadtxt('Figure9/9a.dat')
data9aRef = numpy.loadtxt('Figure9/9a.dat_RefTrans')
data9b = numpy.loadtxt('Figure9/9b.dat')
data9bRef = numpy.loadtxt('Figure9/9b.dat_RefTrans')
data9c = numpy.loadtxt('Figure9/9c.dat')
data9cRef = numpy.loadtxt('Figure9/9c.dat_RefTrans')

fig = pyplot.figure(1)
subfig = fig.add_subplot(231)
subfig.set_xlabel("x")
subfig.set_ylabel("y")
subfig.plot(data9a[:,0], data9a[:,1], label='dR/R')
leg = subfig.legend(loc='best')
subfig = fig.add_subplot(234)
subfig.set_xlabel("x")
subfig.set_ylabel("y")
subfig.plot(data9aRef[:,0], data9aRef[:,1]**2 + data9aRef[:,2]**2, label='r_p')
subfig.plot(data9aRef[:,0], data9aRef[:,3]**2 + data9aRef[:,4]**2, label='r_p fresnel')
leg = subfig.legend(loc='best')

subfig = fig.add_subplot(232)
subfig.set_xlabel("x")
subfig.set_ylabel("y")
subfig.plot(data9b[:,0], data9b[:,1], label='dR/R')
leg = subfig.legend(loc='best')
subfig = fig.add_subplot(235)
subfig.set_xlabel("x")
subfig.set_ylabel("y")
subfig.plot(data9bRef[:,0], data9bRef[:,1]**2 + data9bRef[:,2]**2, label='r_p')
subfig.plot(data9bRef[:,0], data9bRef[:,3]**2 + data9bRef[:,4]**2, label='r_p fresnel')
leg = subfig.legend(loc='best')

subfig = fig.add_subplot(233)
subfig.set_xlabel("x")
subfig.set_ylabel("y")
subfig.plot(data9c[:,0], data9c[:,1], label='dR/R')
leg = subfig.legend(loc='best')
subfig = fig.add_subplot(236)
subfig.set_xlabel("x")
subfig.set_ylabel("y")
subfig.plot(data9cRef[:,0], data9cRef[:,1]**2 + data9cRef[:,2]**2, label='r_p')
subfig.plot(data9cRef[:,0], data9cRef[:,3]**2 + data9cRef[:,4]**2, label='r_p fresnel')
leg = subfig.legend(loc='best')


pyplot.show()

