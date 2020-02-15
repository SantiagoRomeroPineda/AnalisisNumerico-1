horner <- function(coeficiente, x){
  resultado <- coeficiente[1]
  iteraciones <-0
  
  for(k in coeficiente[2:length(coeficiente)]){
    resultado <- x*resultado + k
    iteraciones <- iteraciones + 2
  }
  cat("El resultado es: ", resultado, "\nEl numero minimo de operaciones es: ", iteraciones,"\nSon ",iteraciones/2," multiplicaciones y ",iteraciones/2, " sumas")
  return(resultado)
}
x0 <- 1.0001
coeficiente <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
resul1=horner(coeficiente,x0)

q1<-function(x) (x*51-1)/x-1
resul2=q1(1.0001)

r=abs(resul2-resul1)
err=(r/resul1)*100
cat("El resultado de p(x) es: ", resul1, ", el resultado de q(x) es: ", resul2, " el error encontrado de los polinominos evaluados es: ", err)
