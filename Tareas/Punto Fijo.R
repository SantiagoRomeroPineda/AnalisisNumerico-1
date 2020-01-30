puntofijo =function(g, x0, tol=1e-8, maxIteraciones=100){
  k = 1
  repeat{
    x1 = g(x0)
    dx = abs(x1 - x0)
    x0 = x1
    #Imprimir estado
    cat("x_", k, "= ", x1, "\n")
    k = k+1
    #until
    if(dx< tol|| k > maxIteraciones) break;
  }
  # Mensaje de salida
  if( dx > tol ){
    cat("No hubo convergencia ")
    #return(NULL)
  } else{
    cat("x* es aproximadamente ", x1, " con error menor que ", tol)
  }
}

g = function(x) (x^3-1)/-5
puntofijo(g, 0, 1e-8)
g = function(x) ((1)-(5*x))^(1/3)
puntofijo(g, 0, 1e-8)