import numpy
import matplotlib.pyplot as pyplot

w0 = 10
eta = 1
eopt = 1

w = numpy.linspace(0,20,1000)
er = (w0**2 - w**2)/( (w0**2 - w**2)**2 + eta**2*w**2 ) + eopt
ei = (eta*w)/( (w0**2 - w**2)**2 + eta**2*w**2 )

abs_e = numpy.sqrt(er**2 + ei**2)
n = numpy.sqrt( (abs_e + er)/2.0 )
k = numpy.sqrt( (abs_e - er)/2.0 )


pyplot.figure(1)
pyplot.subplot(121)
pyplot.plot(w,er,color='k')
pyplot.plot(w,n,color='r')
pyplot.xlabel('                          $\omega_0$                       $\omega$', fontsize = 26)
pyplot.ylabel('Real Dielectric Function', fontsize = 20)
pyplot.tick_params(
    axis='both',          # changes apply to the x-axis
    which='both',      # both major and minor ticks are affected
    bottom='off',      # ticks along the bottom edge are off
    top='off',         # ticks along the top edge are off
    left='off',
    right='off',
    labelbottom='off', # labels along the bottom edge are off
    labelleft='off')
#pyplot.tick_params(
    #axis='x',          # changes apply to the x-axis
    #which='both',      # both major and minor ticks are affected
    #bottom='off',      # ticks along the bottom edge are off
    #top='off',         # ticks along the top edge are off
    #labelbottom='off') # labels along the bottom edge are off
#pyplot.tick_params(axis='x', labelsize=16)


pyplot.subplot(122)
pyplot.plot(w,ei,color='k')
pyplot.plot(w,k,color='b')
pyplot.xlabel('                          $\omega_0$                       $\omega$', fontsize = 26)
pyplot.ylabel('Imaginary Dielectric Function', fontsize = 20)
pyplot.tick_params(
    axis='both',          # changes apply to the x-axis
    which='both',      # both major and minor ticks are affected
    bottom='off',      # ticks along the bottom edge are off
    top='off',         # ticks along the top edge are off
    left='off',
    right='off',
    labelbottom='off', # labels along the bottom edge are off
    labelleft='off')

pyplot.show()

#Plot n and k
#pyplot.figure(1)
#pyplot.subplot(121)
#pyplot.plot(w,n,color='k')
#pyplot.xlabel('                          $\omega_0$                       $\omega$', fontsize = 26)
#pyplot.ylabel('n', fontsize = 20)
#pyplot.tick_params(
    #axis='both',          # changes apply to the x-axis
    #which='both',      # both major and minor ticks are affected
    #bottom='off',      # ticks along the bottom edge are off
    #top='off',         # ticks along the top edge are off
    #left='off',
    #right='off',
    #labelbottom='off', # labels along the bottom edge are off
    #labelleft='off')
##pyplot.tick_params(
    ##axis='x',          # changes apply to the x-axis
    ##which='both',      # both major and minor ticks are affected
    ##bottom='off',      # ticks along the bottom edge are off
    ##top='off',         # ticks along the top edge are off
    ##labelbottom='off') # labels along the bottom edge are off
##pyplot.tick_params(axis='x', labelsize=16)

#pyplot.subplot(122)
#pyplot.plot(w,k,color='k')
#pyplot.xlabel('                          $\omega_0$                       $\omega$', fontsize = 26)
#pyplot.ylabel('k', fontsize = 20)
#pyplot.tick_params(
    #axis='both',          # changes apply to the x-axis
    #which='both',      # both major and minor ticks are affected
    #bottom='off',      # ticks along the bottom edge are off
    #top='off',         # ticks along the top edge are off
    #left='off',
    #right='off',
    #labelbottom='off', # labels along the bottom edge are off
    #labelleft='off')

pyplot.show()
