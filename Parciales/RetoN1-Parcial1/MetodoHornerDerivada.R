#Hecho por Sofia Moreno y Anggie Correa
horner <- function(Px, x){
  y<-Px[1]
  z<-Px[1]
  operaciones <-0
  lon<-length(Px)-1
  operaciones<-operaciones+1
  
  for(k in Px[2:lon]){
    y <- x*y + k
    z <- x*z + y
    operaciones <- operaciones + 2
  }
  
  y <- x*y + Px[length(Px)]
  operaciones <- operaciones + 1
  
  cat("Horner de p(x): ", y, "\nResultado de p'(X) es: ",round(z,3) ,"\nEl numero minimo de operaciones es: ", operaciones,"\nSon ",operaciones/2," multiplicaciones y ",operaciones/2, " sumas")
}

x0 <- -2
Px <- c(2,0,-3,3,-4)
horner(Px,x0)

