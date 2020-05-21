#Modelo SI,SIR,SIS del coronavirus en Espa???a
#Cargar los datos del modelo
library (shiny) 
library (shinydashboard) 
library (deSolve)
library (phaseR)
library (pracma)
library(readr)
Datos <- read_delim(file="Datos.csv", 
                    ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                grouping_mark = "."), trim_ws = TRUE)

ui <- dashboardPage ( 
  dashboardHeader (), 
  dashboardSidebar (), 
  dashboardBody () 
) 

header <- dashboardHeader(title = "COVID Simulator")


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("S.I.", tabName = "SI", icon = icon("circle")    ),
    menuItem("S.I.R.", tabName = "SIR", icon = icon("circle") ),
    menuItem("S.I.S.", tabName = "SIS", icon = icon("circle") )
  )
)

frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)

TabSI <- fluidRow( 
  
  box(
    title = "Datos iniciales S.I.", 
    width = 4,
    shinyjs::useShinyjs(),
    (numericInput("poblacionInicialSI", "Poblacion: ", 20, min = 1, max = 1000)),
    sliderInput("infectadosInicialesSI", "Infectados: ", 0, min = 0.0043, max = 1.5,step = 1e-3),
    sliderInput("suceptiblesInicialesSI", "Personas suceptibles iniciales:", 0, 1000, 10),
    sliderInput("tiempoLimiteSI", "Tiempo limite:", 30, 107, 5),
    sliderInput("bethaSI", "Betha:",  0.495, 1, 1e-3),
    sliderInput("gammaSI", "Gamma:", 0.0040, 1.5, 1e-3),
    actionButton("botonCalcularSI", "Calcular")
  ),
  tabBox(
    title = "Metodo 1",
    width = 8,
    # The id lets us use input$tabset1 on the server to find the current tab
    id = "tabGraficas", height = "400px",
    tabPanel("Modelo 1",
             width= 350,
             plotOutput("plotM1SI", height = 350)
    ),
    tabPanel("Pendientes",
             width = 350,
             plotOutput("plotM1PendSI", height = 350, width = 400)
    ),
    tabPanel("Error",
             width = 350,
             plotOutput("plotM1ErrorSI", height = 350, width = 500)
    )
  ),
  
  tabBox(
    
    title = "Metodo 2",
    width = 8,
    # The id lets us use input$tabset1 on the server to find the current tab
    id = "tabGraficasM1SI", height = "400px",
    tabPanel("Modelo",
             width= 350,
             plotOutput("plotM2SI", height = 350)
             
    ),
    tabPanel("Pendientes",
             width = 350,
             plotOutput("plotM2PendSI", height = 350, width = 400)
    ),
    tabPanel("Error",
             width = 350,
             plotOutput("plotM2ErrorSI", height = 350, width = 500)
    )
  )
  
)


TabSIR <- fluidRow( 
  box(
    title = "Datos iniciales S.I.R.", 
    width = 4,
    shinyjs::useShinyjs(),
    numericInput("poblacionInicialSIR", "Poblacion: ", 0, min = 1, max = 1000),
    sliderInput("infectadosInicialesSIR", "Infectados: ", 0, min = 0.0043, max = 1.5,step = 1e-3),
    sliderInput("suceptiblesInicialesSIR", "Personas suceptibles iniciales:", 0, 1000, 10),
    sliderInput("tiempoLimiteSIR", "Tiempo limite:", 30, 107, 5),
    sliderInput("bethaSIR", "Betha:", 0.021, 0.3, 1e-3),
    sliderInput("gammaSIR","Gamma:",  0.4, 1.5, 1e-3),
    actionButton("botonCalcularSIR", "Calcular")
  ),
  tabBox(
    title = "Metodo 1",
    width = 8,
    # The id lets us use input$tabset1 on the server to find the current tab
    id = "tabGraficasM1SIR", height = "400px",
    tabPanel("Modelo",
             width= 350,
             plotOutput("plotM1SIR", height = 350)
    ),
    tabPanel("Pendientes",
             width = 350,
             plotOutput("plotM1PendSIR", height = 350, width = 400)
    ),
    tabPanel("Error",
             width = 350,
             plotOutput("plotM1ErrorSIR", height = 350, width = 500)
    )
  ),
  
  tabBox(
    
    title = "Metodo 2",
    width = 8,
    id = "tabGraficasM2SIR", height = "400px",
    tabPanel("Modelo",
             width= 350,
             plotOutput("plotM2SIR", height = 350)
    ),
    tabPanel("Pendientes",
             width = 350,
             plotOutput("plotM2PendSIR", height = 250, width = 400)
    ),
    tabPanel("Error",
             width = 350,
             plotOutput("plotM2ErrorSIR", height = 250, width = 500)
    )
  )
)
TabSIS <- fluidRow( 
  box(
    title = "Datos iniciales S.I.S.", 
    width = 4,
    shinyjs::useShinyjs(),
    numericInput("poblacionInicialSIS", "Poblacion: ", 20, min = 1, max = 1000),
    sliderInput("infectadosInicialesSIS", "Infectados: ", 0, min = 0.0043, max = 1.5,step = 1e-3),
    sliderInput("suceptiblesInicialesSIS", "Personas suceptibles iniciales:", 0, 1000, 5),
    sliderInput("tiempoLimiteSIS", "Tiempo limite:", 30, 107, 5),
    sliderInput("bethaSIS", "Betha:", 0.0016, 0.02, 1e-3),
    sliderInput("gammaSIS","Gamma:",  0.575, 1, 1e-3),
    actionButton("botonCalcularSIS", "Calcular")
  ),
  tabBox(
    title = "Metodo 1",
    width = 8,
    # The id lets us use input$tabset1 on the server to find the current tab
    id = "tabGraficasM1SIS", height = "400px",
    tabPanel("Modelo",
             width= 350,
             plotOutput("plotM1SIS", height = 350)
    ),
    tabPanel("Pendientes",
             width = 350,
             plotOutput("plotM1PendSIS", height = 350, width = 400)
    ),
    tabPanel("Error",
             width = 350,
             plotOutput("plotM1ErrorSIS", height = 350, width = 500)
    )
  ),
  
  tabBox(
    
    title = "Metodo 2",
    width = 8,
    id = "tabGraficasM2SIS", height = "400px",
    tabPanel("Modelo",
             width= 350,
             plotOutput("plotM2SIS", height = 350)
    ),
    tabPanel("Pendientes",
             width = 350,
             plotOutput("plotM2PendSIS", height = 350, width = 400)
    ),
    tabPanel("Error",
             width = 350,
             plotOutput("plotM2ErrorSIS", height = 350, width = 500)
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem("SI",frow1, TabSI),
    tabItem("SIR",TabSIR),
    tabItem("SIS", TabSIS)
  )
  
)

ui <- dashboardPage(title = 'Analisis Numerico', header, sidebar, body, skin='blue')

server <- function (input, output,session) 
{
  CalcularSI <- function(){
    #Tama???o Poblaci???n
    N = input$poblacionInicialSI
    #Tiempo simulaci???n
    tf = input$tiempoLimiteSI
    #estado inicial de los compartimentos
    init <- c(S = input$suceptiblesInicialesSI , # Suceptibles ->>  input$suceptiblesInicialesSI
              I = input$infectadosInicialesSI)# Infectados ->>  input$infectadosInicialesSI)  
    
    print(input$infectadosInicialesSI)
    #parametros del modelo (coeficientes de las variables)
    param <- c( beta = input$bethaSI , # Tasa de infeccion ( contagiados por unidad de tiempo) input$bethaSI
                gamma = input$gammaSI ) # Probabilidad de transmitir la infeccion  input$gammaSI
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
    times <- seq(1, tf, by = 1)
    #Mediante la funcion "ode" resolvemos el sistema de ecuaciones diferenciales y generamos un data frame 
    simulacionM1SI.si <- as.data.frame(ode(y=init, times=times, func=si,parms=param,method = "rk4"))
    simulacionM2SI.si <- as.data.frame(ode(y=init, times=times, func=si,parms=param,method = "adams"))
    errorM1SI <- c()
    errorM2SI <- c()
    output$plotM1SI <- renderPlot({
      N <- sum(init)
      
      
      #Calculamos el tamanio de la poblacion
      updateTextInput(session, "poblacionInicialSI", value =N)
      #Representamos graficamente los resultados obtenidos
      plot(times, simulacionM1SI.si$S, type="l", col="blue", ylim=c(0,sum(init)), xlab="Tiempo (en dias)", ylab="Numero de Personas",main = "Metodo 1: Runge Kutta 4")
      lines(times, simulacionM1SI.si$I, type="l", col="red")
      legend(x = "topright", legend=c("Susceptibles", "Infectados"), col=c("blue", "red"), lty=rep(1, 2))
      
    })
    #Calcular Campo pendientes de la ecuación diferencial
    output$plotM1PendSI <- renderPlot({
      scopeField <- function(t, p, parameters){
        k <- parameters[1]
        n <- parameters[2]
        dp <- k*(p*(n-p))
        list(dp)
      }
      scopeField.flowField <- flowField(scopeField, xlim = c(0,input$tiempoLimiteSI),
                                        ylim = c(0,input$suceptiblesInicialesSI), parameters = c(0.0005,1000),
                                        system = "one.dim",
                                        add = FALSE, xlab = "Tiempo (en días)", ylab = "Numero de Personas", 
                                        main = "Campo de pendientes")
      
    })
    output$plotM1ErrorSI <- renderPlot({
      
      ##errorM1SI<- c(0)
      for(i in 1:input$tiempoLimiteSI){
        
        errorM1SI<- c(errorM1SI,(abs(Datos$`Cum. cases`[i]- simulacionM1SI.si$I[i])/Datos$`Cum. cases`[i])/100000)
        
      }
      plot(times, errorM1SI, type="l", col="blue",xlim = c(0,60), ylim=c(0,1), xlab="Tiempo (en dias)", ylab="Error Relativo",main = "Metodo 1: Runge Kutta 4")
      
    })
    output$plotM2SI <- renderPlot({
      N <- sum(init)
      
      
      #Calculamos el tamanio de la poblacion
      updateTextInput(session, "poblacionInicialSI", value =N)
      #Representamos graficamente los resultados obtenidos
      plot(times, simulacionM2SI.si$S, type="l", col="blue", ylim=c(0,sum(init)), xlab="Tiempo (en dias)", ylab="Numero de Personas",main = "Metodo 2: Adams")
      lines(times, simulacionM2SI.si$I, type="l", col="red")
      legend(x = "topright", legend=c("Susceptibles", "Infectados"), col=c("blue", "red"), lty=rep(1, 2))
      
    })
    #Calcular el campo de pendientes
    output$plotM2PendSI <- renderPlot({
      scopeField <- function(t, p, parameters){
        k <- parameters[1]
        n <- parameters[2]
        dp <- k*(p*(n-p))
        list(dp)
      }
      scopeField.flowField <- flowField(scopeField, xlim = c(0,input$tiempoLimiteSI),
                                        ylim = c(0,input$suceptiblesInicialesSI), parameters = c(0.0007,1000),
                                        system = "one.dim",
                                        add = FALSE, xlab = "Tiempo (en días)", ylab = "Numero de Personas", 
                                        main = "Campo de pendientes")
      
    })
    output$plotM2ErrorSI <- renderPlot({
      for(i in 1:input$tiempoLimiteSI){
        errorM2SI<- c(errorM2SI,(abs(Datos$`Cum. cases`[i]- simulacionM2SI.si$I[i])/Datos$`Cum. cases`[i])/100000)
      }
      plot(times, errorM2SI, type="l", col="blue",xlim = c(0,60), ylim=c(0,1), xlab="Tiempo (en dias)", ylab="Error Relativo",main = "Metodo 1: Adams")
      
    })
    
  }
  
  CalcularSIR <- function(){
    #Tama???o Poblaci???n
    N = input$poblacionInicialSIR
    #Tiempo simulaci???n
    tf = input$tiempoLimiteSIR
    #estado inicial de los compartimentos
    init <- c(S = input$suceptiblesInicialesSIR , # Suceptibles ->>  input$suceptiblesInicialesSI
              I = input$infectadosInicialesSIR, # Infectados ->>  input$infectadosInicialesSI
              R = 0) 
    #parametros del modelo (coeficientes de las variables)
    param <- c( beta = input$bethaSIR , # Tasa de infeccion ( contagiados por unidad de tiempo) input$bethaSI
                gamma = input$gammaSIR ) # Probabilidad de transmitir la infeccion  input$gammaSI
    #crear la funcion con las ODE
    sir <- function(times, init, param) 
    {
      with(as.list(c(init, param)), 
           {
             #ecuaciones diferenciales   
             dS <- -beta * I * S  /(S+R+I)
             dI <-  (beta * I * S - gamma * I) / (S+R+I) 
             dR <-  gamma * I / (S+R+I)
             #resultados de las tasas de cambio   
             return(list(c(dS, dI,dR)))
           })
    }
    #intervalo de tiempo y resolucion
    times <- seq(1, tf, by = 1)
    print(times)
    #Mediante la funcion "ode" resolvemos el sistema de ecuaciones diferenciales y generamos un data frame 
    simulacionM1SIR.sir <- as.data.frame(ode(y=init, times=times, func=sir,parms=param,method = "rk4"))
    simulacionM2SIR.sir <- as.data.frame(ode(y=init, times=times, func=sir,parms=param,method = "adams"))
    errorM1SIR <- c()
    errorM2SIR <- c()
    output$plotM1SIR <- renderPlot({
      N <- sum(init)
      print(simulacionM1SIR.sir)
      #Calculamos el tamanio de la poblacion
      updateTextInput(session, "poblacionInicialSIR", value =N)
      #Representamos graficamente los resultados obtenidos
      plot(times, simulacionM1SIR.sir$S, type="l", col="blue", ylim=c(0,sum(init)), xlab="Tiempo (en dias)", ylab="Numero de Personas",main = "Metodo 1: Runge Kutta 4")
      lines(times, simulacionM1SIR.sir$I, type="l", col="red")
      lines(times, simulacionM1SIR.sir$R, type="l", col="green")
      legend(x = "topright", legend=c("Susceptibles", "Infectados","Recuperados"), col=c("blue", "red","green"), lty=rep(1, 2,3))
      
    })
    
    
    output$plotM2SIR <- renderPlot({
      N <- sum(init)
      
      
      #Calculamos el tamanio de la poblacion
      updateTextInput(session, "poblacionInicialSIR", value =N)
      #Representamos graficamente los resultados obtenidos
      plot(times, simulacionM1SIR.sir$S, type="l", col="blue", ylim=c(0,sum(init)), xlab="Tiempo (en dias)", ylab="Numero de Personas",main = "Metodo 2: Adams")
      lines(times, simulacionM1SIR.sir$I, type="l", col="red")
      lines(times, simulacionM1SIR.sir$R, type="l", col="green")
      legend(x = "topright", legend=c("Susceptibles", "Infectados","Recuperados"), col=c("blue", "red","green"), lty=rep(1, 2,3))
      
    })
    output$plotM1ErrorSIR <- renderPlot({
      
      for(i in 1:input$tiempoLimiteSIR){
        
        errorM1SIR<- c(errorM1SIR,(abs(Datos$`Cum. cases`[i]- simulacionM1SIR.sir$I[i])/Datos$`Cum. cases`[i])/10000)
        
      }
      plot(times, errorM1SIR, type="l", col="blue",xlim = c(0,60), ylim=c(0,1), xlab="Tiempo (en dias)", ylab="Error Relativo",main = "Metodo 1: Runge Kutta 4")
      
    })
    output$plotM1PendSIR <- renderPlot({
      scopeField <- function(t, p, parameters){
        k <- parameters[1]
        n <- parameters[2]
        dp <- k*(p*(n-p))
        list(dp)
      }
      scopeField.flowField <- flowField(scopeField, xlim = c(0,input$tiempoLimiteSIR),
                                        ylim = c(0,input$suceptiblesInicialesSIR), parameters = c(0.001,100),
                                        system = "one.dim",
                                        add = FALSE, xlab = "Tiempo (en días)", ylab = "Numero de Personas", 
                                        main = "Campo de pendientes")
      
    })
    #Calcular el campo de pendientes
    output$plotM2PendSIR <- renderPlot({
      scopeField <- function(t, p, parameters){
        k <- parameters[1]
        n <- parameters[2]
        dp <- k*(p*(n-p))
        list(dp)
      }
      scopeField.flowField <- flowField(scopeField, xlim = c(0,input$tiempoLimiteSIR),
                                        ylim = c(0,input$suceptiblesInicialesSIR), parameters = c(0.002,1000),
                                        system = "one.dim",
                                        add = FALSE, xlab = "Tiempo (en días)", ylab = "Numero de Personas", 
                                        main = "Campo de pendientes")
      
    })
    output$plotM2ErrorSIR <- renderPlot({
      for(i in 1:input$tiempoLimiteSIR){
        errorM2SIR<- c(errorM2SIR,(abs(Datos$`Cum. cases`[i]- simulacionM2SIR.sir$I[i])/Datos$`Cum. cases`[i])/10000)
      }
      plot(times, errorM2SIR, type="l", col="blue",xlim = c(0,60), ylim=c(0,1), xlab="Tiempo (en dias)", ylab="Error Relativo",main = "Metodo 1: Adams")
      
    })
    
  }
  CalcularSIS <- function(){
    #Tama???o Poblaci???n
    N = input$poblacionInicialSIS
    #Tiempo simulaci???n
    tf = input$tiempoLimiteSIS
    #estado inicial de los compartimentos
    init <- c(S = input$suceptiblesInicialesSIS , # Suceptibles ->>  input$suceptiblesInicialesSI
              I = input$infectadosInicialesSIS)# Infectados ->>  input$infectadosInicialesSI)  
    
    print(input$infectadosInicialesSIS)
    
    #parametros del modelo (coeficientes de las variables)
    param <- c( beta = input$bethaSIS , # Tasa de infeccion ( contagiados por unidad de tiempo) input$bethaSI
                gamma = input$gammaSIS ) # Probabilidad de transmitir la infeccion  input$gammaSI
    #crear la funcion con las ODE
    sis <- function(times, init, param) 
    {
      with(as.list(c(init, param)), 
           {
             #ecuaciones diferenciales   
             dS <- -beta*S*I + gamma*I
             dI <- beta*S*I - gamma*I
             #resultados de las tasas de cambio    
             return(list(c(dS, dI)))
           })
    }
    #intervalo de tiempo y resolucion
    times <- seq(1, tf, by = 1)
    #Mediante la funcion "ode" resolvemos el sistema de ecuaciones diferenciales y generamos un data frame 
    simulacionM1SIS.sis <- as.data.frame(ode(y=init, times=times, func=sis,parms=param,method = "rk4"))
    simulacionM2SIS.sis <- as.data.frame(ode(y=init, times=times, func=sis,parms=param,method = "adams"))
    errorM1SIS <- c()
    errorM2SIS <- c()
    output$plotM1SIS <- renderPlot({
      N <- sum(init)
      
      
      #Calculamos el tamanio de la poblacion
      updateTextInput(session, "poblacionInicialSIS", value =N)
      #Representamos graficamente los resultados obtenidos
      plot(times, simulacionM1SIS.sis$S, type="l", col="blue", ylim=c(0,sum(init)), xlab="Tiempo (en dias)", ylab="Numero de Personas",main = "Metodo 1: Runge Kutta 4")
      lines(times, simulacionM1SIS.sis$I, type="l", col="red")
      legend(x = "topright", legend=c("Susceptibles", "Infectados"), col=c("blue", "red"), lty=rep(1, 2))
      
    })
    #Calcular Campo pendientes de la ecuación diferencial
    output$plotM1PendSIS <- renderPlot({
      scopeField <- function(t, p, parameters){
        k <- parameters[1]
        n <- parameters[2]
        dp <- k*(p*(n-p))
        list(dp)
      }
      scopeField.flowField <- flowField(scopeField, xlim = c(0,input$tiempoLimiteSIS),
                                        ylim = c(0,input$suceptiblesInicialesSIS), parameters = c(0.3,1),
                                        system = "one.dim",
                                        add = FALSE, xlab = "Tiempo (en dias)", ylab = "Numero de personas", 
                                        main = "Campo de pendientes")
      
    })
    output$plotM1ErrorSIS <- renderPlot({
      
      ##errorM1SI<- c(0)
      for(i in 1:input$tiempoLimiteSIS){
        
        errorM1SIS<- c(errorM1SIS,(abs(Datos$`Cum. cases`[i]- simulacionM1SIS.sis$I[i])/Datos$`Cum. cases`[i])/100000)
        
      }
      plot(times, errorM1SIS, type="l", col="blue",xlim = c(0,60), ylim=c(0,1), xlab="Tiempo (en dias)", ylab="Error Relativo",main = "Metodo 1: Runge Kutta 4")
      
    })
    output$plotM2SIS <- renderPlot({
      N <- sum(init)
      
      
      #Calculamos el tamanio de la poblacion
      updateTextInput(session, "poblacionInicialSI", value =N)
      #Representamos graficamente los resultados obtenidos
      plot(times, simulacionM2SIS.sis$S, type="l", col="blue", ylim=c(0,sum(init)), xlab="Tiempo (en dias)", ylab="Numero de Personas",main = "Metodo 2: Adams")
      lines(times, simulacionM2SIS.sis$I, type="l", col="red")
      legend(x = "topright", legend=c("Susceptibles", "Infectados"), col=c("blue", "red"), lty=rep(1, 2))
      
    })
    #Calcular el campo de pendientes
    output$plotM2PendSIS <- renderPlot({
      scopeField <- function(t, p, parameters){
        k <- parameters[1]
        n <- parameters[2]
        dp <- k*(p*(n-p))
        list(dp)
      }
      scopeField.flowField <- flowField(scopeField, xlim = c(0,input$tiempoLimiteSIS),
                                        ylim = c(0,input$suceptiblesInicialesSIS), parameters = c(0.7,100),
                                        system = "one.dim",
                                        add = FALSE, xlab = "Tiempo (en dias)", ylab = "Numero de personas", 
                                        main = "Campo de pendientes")
      
    })
    output$plotM2ErrorSIS <- renderPlot({
      for(i in 1:input$tiempoLimiteSIS){
        errorM2SIS<- c(errorM2SIS,(abs(Datos$`Cum. cases`[i]- simulacionM2SIS.sis$I[i])/Datos$`Cum. cases`[i])/100000)
      }
      plot(times, errorM2SIS, type="l", col="blue",xlim = c(0,60), ylim=c(0,1), xlab="Tiempo (en dias)", ylab="Error Relativo",main = "Metodo 1: Adams")
      
    })
    
    
  }
  
  
  
  observeEvent(input$botonCalcularSI,{
    CalcularSI() 
  })
  observeEvent(input$botonCalcularSIR,{
    CalcularSIR() 
  })
  observeEvent(input$botonCalcularSIS,{
    CalcularSIS() 
  })
}
shinyApp (ui,server)