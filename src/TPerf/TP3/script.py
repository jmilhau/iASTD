#!/usr/bin/env python


for i in range(10000):
       print ("Acquire("+str(i)+",\"UdS\");\nJoin("+str(i)+",\"UdS\");")
for i in range(5000):
       print ("Reserve("+str(2*i)+","+str(10000-(2*i+1))+",\"UdS\");\nTake("+str(2*i+1)+","+str(10000-(2*i+2))+",\"UdS\");")
for i in range(9999):
       print ("Return("+str(i)+","+str(10000-(i+1))+",\"UdS\");")
print ("Return("+str(9999)+","+str(0)+",\"UdS\")")
