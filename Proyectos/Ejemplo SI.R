library (deSolve)
library (phaseR)
library (pracma)
#Tiempo simulación
tf = 110

#estado inicial de los compartimentos
init <- c(S = 300 , # Suceptibles ->>  input$suceptiblesInicialesSI
          I = 2)  # Infectados ->>  input$infectadosInicialesSI

#parametros del modelo (coeficientes de las variables)
param <- c( beta = 0.49 , # Tasa de infeccion ( contagiados por unidad de tiempo) input$bethaSI
            gamma = 0.59 ) # Tasa de recuperados
#crear la funcion con las ODE
si <- function(times, init, param) 
{
  with(as.list(c(init, param)), 
       {
         #ecuaciones diferenciales   
         dS <- - gamma*beta*S*I/(S+I)
         dI <- + gamma*beta*S*I/(S+I)
         #resultados de las tasas de cambio    
         return(list(c(dS, dI)))
       })
}

#intervalo de tiempo y resolucion
times <- seq(0, tf, by = 1)
#Mediante la funcion "ode" resolvemos el sistema de ecuaciones diferenciales y generamos un data frame 
simulacionM1SI.si <- as.data.frame(ode(y=init, times=times, func=si,parms=param,method = "rk4"))
attach(simulacionM1SI.si)
#Calculamos el tamanio de la poblacion
N <- sum(init)
#Representamos graficamente los resultados obtenidos
plot(times, S, type="l", col="blue", ylim=c(0,sum(init)), xlab="Tiempo (en horas)", ylab="Numero de Personas",main = "Metodo 1: Runge Kutta 4")
lines(times, I, type="l", col="red")
simulacionM1SI.si <- as.data.frame(ode(y=init, times=times, func=si,parms=param,method = "adams"))
attach(simulacionM1SI.si)
#Calculamos el tamanio de la poblacion
N <- sum(init)
#Representamos graficamente los resultados obtenidos
plot(times, S, type="l", col="blue", ylim=c(0,sum(init)), xlab="Tiempo (en horas)", ylab="Numero de Personas",main = "Metodo 2: Adams")
lines(times, I, type="l", col="red")
legend(x = "topright", legend=c("Susceptibles", "Infectados"), col=c("blue", "red"), lty=rep(1, 2))
legend(x = "topright", legend=c("Susceptibles", "Infectados"), col=c("blue", "red"), lty=rep(1, 2))
scopeField <- function(t, p, parameters){
  k <- parameters[1]
  n <- parameters[2]
  dp <- k*(p*(n-p))
  list(dp)
}
scopeField.flowField <- flowField(scopeField, xlim = c(0,tf),
                                  ylim = c(0,init[1]), parameters = param,
                                  system = "one.dim",
                                  add = FALSE, xlab = "Tiempo (en horas)", ylab = "Numero de personas", 
                                  main = "Campo de pendientes")
