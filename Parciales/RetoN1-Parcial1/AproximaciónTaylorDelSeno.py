#Hecho por Sofia Moreno y Anggie Correa
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

def error (x, aproximacion):
  if math.sin(x) == 0:
    return(0)
  else: 
    errorAbsoluto = math.sin(x) - aproximacion
    errorRelativo = (errorAbsoluto/math.sin(x))*100
    return (errorRelativo)
  x=x+x

def operacionesNecesarias(poliniomio):
  op=0
  for i in range(poliniomio):  
    op = op + 1
  return (op)


x = sym.Symbol('x')
funcionx = sym.sin(x) 
errorTota=0
operaciones=0
x0 = 0          
n  = 10         # Grado polinomio Taylor
a  = -math.pi/64 ; b = math.pi/64  # x entre [a,b]


op=0
px_tabla = []
datos=[]
for grado in range(0,n,1):
    polinomio = politaylor(funcionx,x0,grado)
    px_tabla.append(polinomio)
    op=op+1
g=(math.pi/64 + math.pi/64) /n
for i in range (0,n,1):
  datos.append(sym.sin(g))
  g=g+g


for grado in range(0,n,1):
    px = px_tabla[grado]
    print("grado :"+str(grado)+ ' polinomio simplificado: '+str(px))
    
    print('polinomio: ')
    sym.pprint(px)
    print()
    errorTota = errorTota + error(g, datos[grado]) 
    operaciones = operaciones + 5
    print("El error toal es:"+str(errorTota))
print ("El número de operaciones es de: "+str(operaciones))
