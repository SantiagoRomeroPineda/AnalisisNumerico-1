
puntofijo=function(g,x0,tol=(10^-8),maxIteraciones=100){
  #contador de iteraciones k
  k=0
  listaIteraciones = c()
  listaErrores = c()
  cat("----------------------------------------------------------\n")
  cat(formatC( c("Iteracion","xInicial","xActual","Error"), width = -15, format = "f", flag = " "), "\n")
  cat("----------------------------------------------------------\n")
  repeat{
    x1=g(x0)
    dx=abs((x1-x0)/x1)
    if(k!=0){
      listaIteraciones = c(listaIteraciones, k)
      listaErrores= c(listaErrores,dx)
      cat(formatC( c(k,x0,x1,dx), digits=10, width = -15, format = "f", flag = " "), "\n")
    }
    x0=x1
    k=k+1
    #si k supera el maximo de iteraciones o dx es menor a la tolerancia limite, se detiene
    if (dx<tol||k>maxIteraciones)break;
  }
  #si dx es mayor a la tolerancia limite
  if (dx>tol){
    cat("no hubo convergencia   ")
    
  }else {
    cat("x* es aproximadamente ", x1, " con error relativo de ", dx)
  }
  points(listaIteraciones, listaErrores, col="red")
  lines(listaIteraciones, listaErrores, col = "blue")
}

g = function(x) ((x^3 -1)/-5);
#formar el lienzo
plot(g, xlim=c(1,6), ylim=c(0,0.009), col = "white")
#linea en 0
abline(h=0,col="black")
puntofijo(g,1)
