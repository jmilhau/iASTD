#!/usr/bin/env python







#Ai
def printAi(i,s1,s2,j):
	print ("     ( A"+str(i)+""+str(j)+",<  ");
	print ("             aut;  ");
	print ("             { (SA"+str(i)+""+str(j)+"1->elem),  ");
	print ("               (SA"+str(i)+""+str(j)+"2->elem),  ");
	print ("               (SA"+str(i)+""+str(j)+"3->elem)  ");
	print ("             };  ");
	print ("             { ((local,SA"+str(i)+""+str(j)+"1,SA"+str(i)+""+str(j)+"2),CreateA"+str(i)+"("+s1+","+s2+"),{},False),  ");
	print ("               ((local,SA"+str(i)+""+str(j)+"2,SA"+str(i)+""+str(j)+"3),DelA"+str(i)+"("+s1+"),{},False)  ");
	print ("             };  ");
	print ("             {   ");
	print ("               SA"+str(i)+""+str(j)+"3  ");
	print ("             };  ");
	print ("             {   ");
	print ("             };  ");
	print ("             SA"+str(i)+""+str(j)+"1  ");
	print ("           > ) ");





#E1
def printE1():

	print ("(E1,<|[]|:;");
	print ("       x1;");
	print ("       [1,100000];");
	print ("       {};");
	print ("    (E1KLEENE,<*;");
	print ("      (E1AUT,<  ");
	print ("             aut;  ");
	print ("             { (SE11->elem),  ");
	print ("               (SE12->");



	print ("               <*;");
	print ("                          (A1QCHOICE,<|:;");
	print ("                                     x2;");
	print ("                                     [1,100000];");
	printAi(1,"x1","x2",1);
	print ("                                    >)");
	print ("                         >");




	print ("               ),  ");
	print ("               (SE13->elem)  ");
	print ("             };  ");
	print ("             { ((local,SE11,SE12),CreateE1(x1),{},False),  ");
	print ("               ((local,SE12,SE13),DelE1(x1),{},False)  ");
	print ("             };  ");
	print ("             {   ");
	print ("               SE13  ");
	print ("             };  ");
	print ("             {   ");
	print ("             };  ");
	print ("             SE11  ");
	print ("           >  ");
	print ("           )  ");
	print ("        >)");
	print ("  >)")


#En
def printEn(n):
	print ("(E"+str(n)+",<|[]|:;");
	print ("       x"+str(n)+";");
	print ("       [1,100000];");
	print ("       {};");
	print ("    (E"+str(n)+"KLEENE,<*;");
	print ("      (E"+str(n)+"AUT,<  ");
	print ("             aut;  ");
	print ("             { (SE"+str(n)+"1->elem),  ");
	print ("               (SE"+str(n)+"2->");



	print ("               <|[]|:;");
	print ("                x"+str(n-1)+";");
	print ("                [1,100000];");
	print ("                {};");
	print ("                (ANKLEENE,<*;");
	printAi((n-1),("x"+str(n-1)),("x"+str(n)),n);
	print ("                         >)");
	print ("               >");



	print ("               ),  ");
	print ("               (SE"+str(n)+"3->elem)  ");
	print ("             };  ");
	print ("             { ((local,SE"+str(n)+"1,SE"+str(n)+"2),CreateE"+str(n)+"(x"+str(n)+"),{},False),  ");
	print ("               ((local,SE"+str(n)+"2,SE"+str(n)+"3),DelE"+str(n)+"(x"+str(n)+"),{},False)  ");
	print ("             };  ");
	print ("             {   ");
	print ("               SE"+str(n)+"3  ");
	print ("             };  ");
	print ("             {   ");
	print ("             };  ");
	print ("             SE"+str(n)+"1  ");
	print ("           >  ");
	print ("           )  ");
	print ("        >)");
	print ("  >)")




#Ei
def printEi(i):
	print ("(E"+str(i)+",<|[]|:;");
	print ("       x"+str(i)+";");
	print ("       [1,100000];");
	print ("       {};");
	print ("    (E"+str(i)+"KLEENE,<*;");
	print ("      (E"+str(i)+"AUT,<  ");
	print ("             aut;  ");
	print ("             { (SE"+str(i)+"1->elem),  ");
	print ("               (SE"+str(i)+"2->");


	print ("                      <|[]|;");
	print ("                      {};");
	print ("                      (ASTD"+str(i)+"1,");



	print ("               <|[]|:;");
	print ("                x"+str(i-1)+";");
	print ("                [1,100000];");
	print ("                {};");
	print ("                (AIM1"+str(i)+"KLEENE,<*;");
	printAi((i-1),("x"+str(i-1)),("x"+str(i)),i);
	print ("                         >)");
	print ("               >");
	print ("                      );");



	print ("                      (ASTD"+str(i)+"2,");
	print ("               <*;");
	print ("                          (AI"+str(i)+"QCHOICE,<|:;");
	print ("                                     x"+str(i+1)+";");
	print ("                                     [1,100000];");
	printAi(i,("x"+str(i)),("x"+str(i+1)),i);
	print ("                                    >)");
	print ("                         >");
	print ("                      )");
	print ("                     >");



	print ("               ),  ");
	print ("               (SE"+str(i)+"3->elem)  ");
	print ("             };  ");
	print ("             { ((local,SE"+str(i)+"1,SE"+str(i)+"2),CreateE"+str(i)+"(x"+str(i)+"),{},False),  ");
	print ("               ((local,SE"+str(i)+"2,SE"+str(i)+"3),DelE"+str(i)+"(x"+str(i)+"),{},False)  ");
	print ("             };  ");
	print ("             {   ");
	print ("               SE"+str(i)+"3  ");
	print ("             };  ");
	print ("             {   ");
	print ("             };  ");
	print ("             SE"+str(i)+"1  ");
	print ("           >  ");
	print ("           )  ");
	print ("        >)");
	print ("  >)")


#Synch


def Synch (syn,i):

	if syn==0 : printEn(i)

	else :  
		print ("(SYNCH"+str(i)+",");
		print ("     <|[]|;");
		print ("      {CreateA"+str(i)+",DelA"+str(i)+"};");
		printEi(i);
		print ("      ;");
		Synch(syn-1,i+1);
		print (">)")




#Main

def Main(i):
	if i==1 : 
		print ("(MAIN,<|[]|:;");
		print ("       x;");
		print ("       [1,100000];");
		print ("       {};");
		print ("    (E1KLEENE,<*;");
		print ("      (E1AUT,<  ");
		print ("             aut;  ");
		print ("             { (SE11->elem),  ");
		print ("               (SE12->");



		print ("               <*;");
		print ("                          (A1QCHOICE,<|:;");
		print ("                                     y;");
		print ("                                     [1,100000];");
		printAi(1,"x","y",1);
		print ("                                    >)");
		print ("                         >");




		print ("               ),  ");
		print ("               (SE13->elem)  ");
		print ("             };  ");
		print ("             { ((local,SE11,SE12),CreateE1(x),{},False),  ");
		print ("               ((local,SE12,SE13),DelE1(x),{},False)  ");
		print ("             };  ");
		print ("             {   ");
		print ("               SE13  ");
		print ("             };  ");
		print ("             {   ");
		print ("             };  ");
		print ("             SE11  ");
		print ("           >  ");
		print ("           )  ");
		print ("        >)");
		print ("  >)")
	
	elif i==0 : print " 0 n est pas une valeur valide pour la generation de la structure "

	else : 
		print ("(MAIN,");
		print ("     <|[]|;");
		print ("      {CreateA1,DelA1};");
		printE1();
		print ("      ;");
		Synch(i-2,2);#il en reste i-2 synchro a creer, et on doit commencer par i=2 pour l astd
		print (">)")


Main(100)














#for i in range(10000):
#       print ("Acquire("+str(i)+",\"UdS\");\nJoin("+str(i)+",\"UdS\");")
#for i in range(5000):
#       print ("Reserve("+str(2*i)+","+str(10000-(2*i+1))+",\"UdS\");\nTake("+str(2*i+1)+","+str(10000-(2*i+2))+",\"UdS\");")
#for i in range(9999):
#       print ("Return("+str(i)+","+str(10000-(i+1))+",\"UdS\");")
#print ("Return("+str(9999)+","+str(0)+",\"UdS\")")
