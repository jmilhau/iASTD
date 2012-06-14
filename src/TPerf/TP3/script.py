#!/usr/bin/env python


for i in range(2001):
       print ("Acquire("+str(i)+",\"UdS\");\nJoin("+str(i)+",\"UdS\");")
for i in range(1001):
       print ("Reserve("+str(i)+","+str(1000-i)+",\"UdS\");\nTake("+str(i)+","+str(1000-i)+",\"UdS\");")
for i in range(1000):
       print ("Return("+str(i)+","+str(1000-i)+",\"UdS\");")
for i in range(499):
       print ("Sale("+str(1001+i)+","+str(2000-i)+",\"UdS\");")
print ("Sale("+str(2000)+","+str(1001)+",\"UdS\")")
