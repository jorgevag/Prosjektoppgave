import numpy
import scipy.constants as sc

pi = sc.pi
d = 80.0 #nm

# Assume some initial conditions
a = 1.0 # nm
r = ( (6*d*a**2)/(4*pi) )**(1.0/3)


while (a <= 2*r):
    a = a + 1.0
    r = ( (6*d*a**2)/(4*pi) )**(1.0/3)
    if(a > 1000):
        break

print 'For d = ',d,':'
print 'a = ',a
print 'r = ',r
print ''
print 'Keeping a constant, we get...'

d = d-10
while(d > 0):
    r = ( (6*d*a**2)/(4*pi) )**(1.0/3)
    print 'For d = ',d,':'
    print '(a = ',a,')'
    print 'r = ',r
    print ''
    d = d-10
