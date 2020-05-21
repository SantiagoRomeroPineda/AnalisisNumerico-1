library (deSolve)
library (phaseR)
library (pracma)
#Tiempo simulación
tf = 20
#estado inicial de los compartimentos
init <- c(S = 400 , # Suceptibles ->>  input$suceptiblesInicialesSI
          I = 2,
          R=64)  # Infectados ->>  input$infectadosInicialesSI

#parametros del modelo (coeficientes de las variables)
param <- c( beta = 0.02 , # Tasa de infeccion ( contagiados por unidad de tiempo) input$bethaSI
            gamma = 0.04 ) # Probabilidad de transmitir la infeccion  input$gammaSI
#crear la funcion con las ODE
sir <- function(times, init, param) 
{
  with(as.list(c(init, param)), 
       {
         #ecuaciones diferenciales   
         dS <- - beta*S*I
         dI <- + beta*S*I-gamma*I
         dR <- gamma*I
         #resultados de las tasas de cambio   
         return(list(c(dS, dI,dR)))
       })
}

#intervalo de tiempo y resolucion
times <- seq(0, tf, by = 1)
#Mediante la funcion "ode" resolvemos el sistema de ecuaciones diferenciales y generamos un data frame 
simulacionM1SI.sir <- as.data.frame(ode(y=init, times=times, func=sir,parms=param, method = "euler"))
attach(simulacionM1SI.sir)
#Calculamos el tamanio de la poblacion
N <- sum(init)
#Representamos graficamente los resultados obtenidos
plot(times, S, type="l", col="blue", ylim=c(0,sum(init)), xlab="Tiempo (en horas)", ylab="Numero de Personas",main = "Metodo 1: Runge Kutta 4")
lines(times, I, type="l", col="red")
lines(times, R, type="l", col="green")
simulacionM1SI.sir <- as.data.frame(ode(y=init, times=times, func=sir,parms=param,method = "adams"))
attach(simulacionM1SI.sir)
#Calculamos el tamanio de la poblacion
N <- sum(init)
#Representamos graficamente los resultados obtenidos
plot(times, S, type="l", col="blue", ylim=c(0,sum(init)), xlab="Tiempo (en horas)", ylab="Numero de Personas",main = "Metodo 2: Adams")
lines(times, I, type="l", col="red")
lines(times, R, type="l", col="green")
legend(x = "topright", legend=c("Susceptibles", "Infectados"), col=c("blue", "red"), lty=rep(1, 2))
legend(x = "topright", legend=c("Susceptibles", "Infectados"), col=c("blue", "red"), lty=rep(1, 2))


scopeField <- function(t, y, parameters){
  x <- y[1]
  y <- y[2]
  r <- y[3]
  betha <- parameters[1]
  gamma <- parameters[2]
  dy <- numeric(3)
  dy[1] <- -betha*x*y
  dy[2] <- betha*x*y - gamma*y
  dy[3] <- gamma*y
  list(dy)
}
scopeField.flowField <- flowField(scopeField, xlim = c(0,tf),
                                  ylim = c(0,init[1]), parameters = c(0.02,0.04),
                                  add = FALSE, xlab = "Tiempo (en horas)", ylab = "Numero de personas", 
                                  main = "Campo de pendientes")