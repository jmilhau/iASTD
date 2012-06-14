#!/usr/bin/env python


#for i in range(10000):
#       print ("Acquire("+str(i)+",\"UdS\");\nJoin("+str(i)+",\"UdS\");")
#for i in range(5000):
#       print ("Reserve("+str(2*i)+","+str(10000-(2*i+1))+",\"UdS\");\nTake("+str(2*i+1)+","+str(10000-(2*i+2))+",\"UdS\");")
#for i in range(9999):
#       print ("Return("+str(i)+","+str(10000-(i+1))+",\"UdS\");")
#print ("Return("+str(9999)+","+str(0)+",\"UdS\")")


def Main(i,j):
	for e in range(i):
		for o in range(j):
	       		print ("CreateE"+str(e+1)+"("+str(o+1)+")")
	for e in range(i-1):
		for o in range(j):
			for m in range(1):
		       		print ("CreateA"+str(e+1)+"("+str((((o+1)+(m+1))%j)+1)+","+str(o+1)+")")
	for e in range(i-1):
		for o in range(j):
			for m in range(1):
		       		print ("DelA"+str(e+1)+"("+str((((o+1)+(m+1))%j)+1)+")")
	for e in range(i):
		for o in range(j):
	       		print ("DelE"+str(e+1)+"("+str(o+1)+")")

#for i = 1 to k do
#  CreateE1(i)
#  ...
#  CreateEn(i)
#// creation des associations: Ei+1 est associe a 10 Ei
#for i = 1 to k do
#  for j = 1 to 10 do
#      CreateA1(((i+j) mod k)+1,i)
#      ...
#      CreateAn-1(((i+j) mod k)+1,i)
#// suppression des associations
#for i = 1 to k do
#  for j = 1 to 10 do
#      DelA1(((i+j) mod k)+1,i)
#      ...
#      DelAn-1(((i+j) mod k)+1,i)
#// suppression des entites
#for i = 1 to k do
#  DelE1(i)
#  ...
#  DelEn(i)

Main(100,1)
