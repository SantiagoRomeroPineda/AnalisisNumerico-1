library (deSolve)
library (phaseR)
library (pracma)
N = 2000
#Tiempo simulación
tf = 107

#estado inicial de los compartimentos
init <- c(S = 5 , # Suceptibles ->>  input$suceptiblesInicialesSI
          I = 2)  # Infectados ->>  input$infectadosInicialesSI

#parametros del modelo (coeficientes de las variables)
param <- c( beta = 0.49 , # Tasa de infeccion ( contagiados por unidad de tiempo) 
            gamma = 0.34 ,
            miu = 0.20) # Tasa de recuperados
#crear la funcion con las ODE
sis <- function(times, init, param) 
{
  with(as.list(c(init, param)), 
       {
         #ecuaciones diferenciales   
         dS <- -gamma*S*I/ (N+miu*I)
         dI <- gamma*S*I / (N-miu*I)
         #resultados de las tasas de cambio   
         return(list(c(dS, dI)))
       })
}

#intervalo de tiempo y resolucion
times <- seq(0, tf, by = 1)
#Mediante la funcion "ode" resolvemos el sistema de ecuaciones diferenciales y generamos un data frame 
simulacionM1SI.sis <- as.data.frame(ode(y=init, times=times, func=sis,parms=param,method = "rk4"))
attach(simulacionM1SI.sis)
#Calculamos el tamanio de la poblacion
N <- sum(init)
#Representamos graficamente los resultados obtenidos
plot(times, S, type="l", col="blue", ylim=c(0,sum(init)), xlab="Tiempo (en horas)", ylab="Numero de Personas",main = "Metodo 1: Runge Kutta 4")
lines(times, I, type="l", col="red")
simulacionM1SI.sis <- as.data.frame(ode(y=init, times=times, func=SIS(),parms=param,method = "euler"))
attach(simulacionM1SI.sis)
#Calculamos el tamanio de la poblacion
N <- sum(init)
#Representamos graficamente los resultados obtenidos
plot(times, S, type="l", col="blue", ylim=c(0,sum(init)), xlab="Tiempo (en horas)", ylab="Numero de Personas",main = "Metodo 2: EULER")
lines(times, I, type="l", col="red")
legend(x = "topright", legend=c("Susceptibles", "Infectados"), col=c("blue", "red"), lty=rep(1, 2))
legend(x = "topright", legend=c("Susceptibles", "Infectados"), col=c("blue", "red"), lty=rep(1, 2))