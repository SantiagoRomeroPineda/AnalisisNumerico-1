#Interpolación
# Objetos definidos
ls()
# Eliminar todos los objetos
rm(list= ls())
exp(-30)
options(scipen=999)# desactivar la nomenclatura cientifica 
options(digits = 16)
# --- Cálculo con 256 bits
# install.packages(Rmpfr)
# Requiere instalar previamente
# 1. MPFR (MP Floating-Point Reliable Library), http://mpfr.org/
# 2. GMP (GNU Multiple Precision library), http://gmplib.org/
require(Rmpfr)

##############Interpolación con la libreria PoLyfom
install.packages("PolynomF")#instalar paquete 
help(PolynomF)
library(PolynomF)
x0 <- 80:89
length(x0)
y0 <- c(487, 370, 361, 313, 246, 234, 173, 128, 88, 83)
length(y0)

plot(x0,y0, pch=19, xlim = c(80,89),cex=1, col = "red") #ver grafica 
plot(x0,y0, pch=19,cex=1, col = "red", asp=1) #utilizar escala igual
p <- poly_calc(x0, y0)        ## conduce a una falla numérica catastrófica! al utilizar Poly
p                             ## imprime el polinomio
poly
curve(p,add=T)                ## La curva parece indicar que tiene ajuste!!!
range(p(x0) - y0)             ## estos deben estar "cerca de cero"!lo cual no se cumple
p1 <- poly_calc(x0 - 84, y0)  ## Propone cambiar el origen soluciona el problema??
p1
range(p1(x0 - 84) - y0)       ## Estos estan cercanos a cero.
plot(p1, xlim = c(80, 89) - 84, xlab = "x0 - 84")
points(x0 - 84, y0, col = "red")
## ¿Podemos ahora escribir el polinomio ?
p0 <- change_origin(p1, -84)  ## intentando cambiar el origen a cero
p0
## conduce a graves problemas numéricos de nuevo!!!!!!!!!!!!
plot(p0, xlim = c(80, 89))
points(x0, y0, col = "red")   ## Mayores errores debido a la  precision (finita)
#### En resumen tenemos valores que oscilan
xi=c(0,.5,1,2,3,4)
xi
yi=c(0,.93,1,1.1,1.15,1.2)
p3 = poly.calc(xi,yi)
p3
range(p3(xi) - yi) 
#polyAjuste
plot(xi,yi, pch = 19, cex=1, col= "red")
curve(p3,add=T,lty=3, lwd=2)

#####Lagrange
xi=c(0,.5,1,2,3,4)
xi
yi=c(0,.93,1,1.1,1.15,1.2)
n = length(xi)
X = matrix(rep(xi, times=n), n, n, byrow=T)
X
a=2
mN = a - X ;diag(mN)= 1
mN
mD = X - t(X);  diag(mD)=1
mD
INTlagrange = function(xi,yi,a){
  n = length(xi)
  if(a < min(xi) || max(xi) < a) stop("No está interpolando")
  X = matrix(rep(xi, times=n), n, n, byrow=T)
  mN = a - X; diag(mN) = 1
  mD = X - t(X); diag(mD) = 1
  Lnk = apply(mN, 1, prod)/apply(mD, 2, prod)
  sum(yi*Lnk)
}
xi = c( 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
yi = c(0.31, 0.32, 0.33, 0.34, 0.45, 0.46, 0.47, 0.48, 0.49, 0.5)
INTlagrange(xi[2:5],yi[2:5], 0.35)
### Variaciones Lagrange
install.packages("pracma")
library(pracma)
xi[c(2:6)]; yi[c(2:6)]

barylag(xi[c(2:6)], yi[c(2:6)], c(0.55, 1.51, 2.1))#evaluado en nodos del rango xi
## Neville
neville(xi[2:5],yi[2:5], 0.35)
#####Splines
f = function(x) 1/(1+25*x^2)
n = 50
x = seq(-1,1, by=2/n)
y = f(x)
splinesRunge = splinefun(x,y, method="natural")
plot(x,y)
curve(splinesRunge(x), add=TRUE, col=2, n=1001)

###Comparacion
require(PolynomF)
xi=c(0,.5,1,2,3,4)
yi=c(0,.93,1,1.1,1.15,1.2)
polyAjuste = poly_calc(xi,yi)
#polyAjuste
plot(xi,yi, pch = 19, cex=1.5, col= "red")
curve(polyAjuste,add=T,lty=2, lwd=4) # lwd = grosor, lty = tipo
# Usando trazadores cúbicos
splineAjuste=splinefun(xi,yi) # frontera natural
curve(splineAjuste,add=T,lty=1,lwd=4, col= "blue")
# splineAjuste.mono = splinefun(xi,yi,method="mono")#curva monótona
# curve(splineAjuste.mono,add=T,lty=1)
legend("bottomright",legend=c("Interpolante","Trazador"), lty=c(2:1),bty="n")























