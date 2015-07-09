import os,sys
from scipy import stats
import numpy as np
import matplotlib.pyplot as plt

f=open('9a.dat', 'r')
lines = f.readlines()
f.close()
N = len(lines)
x = np.zeros(N)
y1 = np.zeros(N)
y2 = np.zeros(N)
y3 = np.zeros(N)
y4 = np.zeros(N)
y5 = np.zeros(N)
y6 = np.zeros(N)
y7 = np.zeros(N)
y8 = np.zeros(N)
y9 = np.zeros(N)
y10 = np.zeros(N)
y11 = np.zeros(N)
y12 = np.zeros(N)
y13 = np.zeros(N)
y14 = np.zeros(N)
y15= np.zeros(N)
y16= np.zeros(N)
y17= np.zeros(N)
y18 = np.zeros(N)
y19 = np.zeros(N)
y20 = np.zeros(N)
for i in range(35,N):
    w=lines[i].split()
    x[i]= w[0]
    y1[i] = w[1]
    y2[i] = w[2]
    y3[i] = w[3]
    y4[i] = w[4]
    y5[i] = w[5]
    y6[i] = w[6]
    y7[i] = w[7]
    y8[i] = w[8]
    y9[i] = w[9]
    y10[i] = w[10]
    y11[i] = w[11]
    y12[i] = w[12]
    y13[i] = w[13]
    y14[i] = w[14]
    y15[i] = w[15]
    y16[i] = w[16]
    y17[i] = w[17]
    y18[i] = w[18]
    y19[i] = w[19]

    #try:
        #x=[float(j) for j in w[0]]
        #potential=[float(j) for j in w[1]]
    #except ValueError,e:
        #print "error",e,"on line",i


fig = plt.figure(1)
ax1 = fig.add_subplot(111)
ax1.set_xlabel("x")
ax1.set_ylabel("y")
#ax1.axis([-0.5,0.5,-10, 10])
ax1.plot(x, y1, label='energy')
ax1.plot(x, y4, label='alpha||,Re')
ax1.plot(x, y5, label='alpha||,Im')
ax1.plot(x, y6, label='alpha_|_,Re')
ax1.plot(x, y7, label='alpha_|_,Im')
ax1.plot(x, y8, label='alpha||^10,Re')
ax1.plot(x, y9, label='alpha||^10,Im')
ax1.plot(x, y10, label='alpha_|_^10,Re')
ax1.plot(x, y11, label='alpha_|_^10,Im')
ax1.plot(x, y12, label='gamma, Re')
ax1.plot(x, y13, label='gamma, Im')
ax1.plot(x, y14, label='beta, Re')
ax1.plot(x, y15, label='beta, Im')
ax1.plot(x, y16, label='delta, Re')
ax1.plot(x, y17, label='delta, Im')
ax1.plot(x, y18, label='tau, Re')
ax1.plot(x, y19, label='tau, Im')
leg = ax1.legend()

fig2 = plt.figure(2)
ax2 = fig2.add_subplot(111)
ax2.set_xlabel("x")
ax2.set_ylabel("y")
ax2.plot(x, y2, label='dR/R, A')
ax2.plot(x, y3, label='dR/R, phase')
leg = ax1.legend()

plt.show()


