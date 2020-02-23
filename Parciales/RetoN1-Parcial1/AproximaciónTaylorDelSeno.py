#Hecho por Sofia Moreno y Anggie Correa

import numpy as np
import sympy as sym
import math

def politaylor(funcionx,x0,n):
    k = 0
    polinomio = 0
    while (k <= n):
        derivada   = funcionx.diff(x,k)
        derivadax0 = derivada.subs(x,x0)
        divisor   = np.math.factorial(k)
        terminok  = (derivadax0/divisor)*(x-x0)**k
        polinomio = polinomio + terminok
        k = k + 1
    return(polinomio)

x = sym.Symbol('x')
funcionx = sym.sin(x) 

x0 = 0          
n  = 10         # Grado polinomio Taylor
a  = -math.pi/64 ; b = math.pi/64  # x entre [a,b]
muestras = 51

op=0
px_tabla = []
for grado in range(0,n,1):
    polinomio = politaylor(funcionx,x0,grado)
    px_tabla.append(polinomio)
    op=op+1

for grado in range(0,n,1):
    px = px_tabla[grado]
    print("grado :"+str(grado)+ ' polinomio simplificado: '+str(px))
    
    print('polinomio: ')
    sym.pprint(px)
    print()
print ("El número de operaciones es de: "+str(op*10))