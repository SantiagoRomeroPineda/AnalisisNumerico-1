#install.packages("Matrix")#instalar paquete
#library(Matrix)
#install.packages("PolynomF")#instalar paquete
#library(PolynomF)

rm()
#A
T = c(200,300,400,500,600)
B = c(-35,-4.2,-9,16.9,21.3)

resultado = poly.calc(T,B)
resultado
#B
cat("->en 450",resultado(450),"\n")
#C
tablaInterpolada = data.frame(seq(200, 600, by=100),resultado(seq(200, 600, by=100)))
tablaInterpolada
tablaDatos = data.frame(T,B)
tablaDatos
plot(tablaDatos,pch=15, cex=2, col = "red", xlab="X",ylab="Y")
par(new = TRUE)
plot(tablaInterpolada, pch=20, cex=2, col = "blue",xlab="X", ylab="Y", main="Diagrama ")
par(new = TRUE)
#curve(resultado,add=T,from =200, to = 600,xlab="X", ylab="Y", main="Interpolación")

#Lagrange
#D
f <- function(x)
{             
  mapply(sin, x) 
  
}
oneLagrange_pol <- function(dataX, index, x)
{
 
  L_i <- 1.                   
  for(i in 1:length(dataX))
  {
    if(i != index)
    {
      #evaluamos el coeficiente de 
      L_i <- L_i*( (x - dataX[i] )/(dataX[index] - dataX[i]) )
    }
  }
  
  return(L_i)  #regresamos el coeficiente index-esimo 
}
Eval_pLagrange <- function(dataX, dataY, x)
{
 
  for(i in 1:length(dataX))
  {
    #calculamos iterativamente el polinomio de Laprange
    f_aprox <- f_aprox + dataY[i]*oneLagrange_pol(dataX, i, x)
  }
  return(f_aprox)  #regresamos el valor del polinomio en el punto
}  
Lagrange <- function(dataX, dataY, m, a, b )
{
  
  soporte <- seq(a, b, length = m)                                                              
  f_soporte <- soporte*0                                                                           
  for(i in 1:length(soporte))
  {
    #para cada punto en el soporte se evalua el polinomio
    f_soporte[i] <- Eval_pLagrange(dataX, dataY , soporte[i]  )
    
  }
  return(f_soporte)      #regresamos el vector con los valores interpolados
}

tablaLagrange = data.frame(seq(200,600,50),Lagrange(T,B,9,200,600))
tablaLagrange

resultadoLagrange = c(-35.0,-4.1,9.2,17.2,21.7)
tablaLagrangePoly = data.frame(T,resultadoLagrange)
tablaLagrangePoly
plot(tablaLagrangePoly, pch=18, cex=2, col = "Green",xlab="X", ylab="Y", main="Diagrama ")

datosReales = B
resultadosInterpolación = resultado(seq(200, 600, by=100))
tablaComparativa = data.frame(T, datosReales, resultadosInterpolación, resultadoLagrange)
tablaComparativa