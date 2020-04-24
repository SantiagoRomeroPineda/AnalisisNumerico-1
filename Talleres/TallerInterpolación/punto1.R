
require(PolynomF)


options (digits = 5)
#Funcion para interpolar con el polinomio de Lagrange:
lagrange = function(x,y,a){
  n = length(x)
  if(a < min(x) || max(x) < a) stop("No está interpolando")
  X = matrix(rep(x, times=n), n, n, byrow=T)
  mN = a - X; diag(mN) = 1
  mD = X - t(X); diag(mD) = 1
  Lnk = apply(mN, 1, prod)/apply(mD, 2, prod)
  sum(y*Lnk)
}

#Puntos:
x = c( 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0) 
y = c(0.31, 0.32, 0.33, 0.34, 0.45, 0.46, 0.47, 0.48, 0.49, 0.5)
lagrange(x[2:5],y[2:5], 0.35)



xdatos = x[2:5]; ydatos = y[2:5]
polinomioAjuste = poly_calc(x,y)
polinomioAjuste
#cat (polinomioAjuste, "\n")
#Puntos
plot(xdatos,ydatos, pch=19, cex=1, col = "blue", asp=1, xlab= "x", ylab="y") 
curve(polinomioAjuste,add=T) # Curva de ajuste (polinomio interpolante)