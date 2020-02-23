#Hecho por Sofia Moreno y Anggie Correa
horner <- function(Px, x){
  
  y<-Px[1]

  operaciones <-0
  lon<-length(Px)
  operaciones<-operaciones+1
  
  for(k in Px[2:lon]){
    y <- x*y + k
    operaciones <- operaciones + 1
  }
  
  cat("Horner de p(x): ",round(y,3),"\nEl numero minimo de operaciones es: ", operaciones,"\nSon ",operaciones/2," multiplicaciones y ",operaciones/2, " sumas")
}

x0 <- 6+1.3i
Px <- c(1+0i,0+8i,5+0i,-2+0i)
horner(Px,x0)