#package.skeleton(name="tsmodfor",force = TRUE, code_files="tsmodfor_01.R")
#R CMD build tsmodfor
#R CMD INSTALL tsmodfor_1.0.tar.gz
#install.packages('/home/andraxas/Data_Science_Reference/07_Projects/05_Shiny/Demo_TS01/tsmodfor_1.0.tar.gz', repos = NULL, type="source")
#install_local()

#library(forecast)
#library(tseries)
#library(shiny)
#library(dygraphs)
#library(tsmodfor)

#' Modelar la tendencia de la serie de tiempo
#'
#' Modela una tendencia Lineal, Cuadrático o Cúbica, devuelve una serie de tiempo.
#'residual que puede ser modelada en estacionalidad
#'
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
TsTrans <- function(mtyp, xts)#recibe TS.trans, devuelve TS.tend
{
  Time<- as.vector(time(xts))
  switch(mtyp,
         {
           # case Ninguno here...1
           print('Ninguna transformación')
           xts.serie<-xts          #No hay serie modelada

         },
         {
           # case Diff   ...2
           print('Transformación Diferencial')
           xts.serie<-na.omit(diff(xts))
           },
         {
           # case Log ...3
           print('Transformación Log')
           xts.serie<-na.omit(log(xts))
         },

         {
           print('Ts Transf default')
         }
  )
  print("Transformación finalizada")
  return(xts.serie)
}

#_____________________________________________________________________________________

#' Modelar la tendencia de la serie de tiempo
#'
#' Modela una tendencia Lineal, Cuadrático o Cúbica, devuelve una serie de tiempo.
#'residual que puede ser modelada en estacionalidad
#'
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
TsTendModel <- function(mtyp, xts)#recibe TS.trans, devuelve TS.tend
{

  #subxts<-end(xts)[1]+end(xts)[2]*deltat(xts)-2*deltat(xts)
  #xts<-window(xts ,end=subxts)
  Time<- as.vector(time(xts))
  switch(mtyp,
         {
           # case Ninguno here...1
           print('Ningún Modelo')
           xts.mod<-""            #No hay serie modelada
           xts.serie<-xts*0          #No hay serie modelada
           xts.resid<-xts

           },
         {
           # case LINEAL here...2
           print('Modelo lineal')
           xts.mod<-lm(xts ~ Time)
           xts.serie<-ts(xts.mod$fitted.values,freq=frequency(xts),start=start(xts),deltat=deltat(xts))
           xts.resid<-ts(xts.mod$residuals,freq=frequency(xts),start=start(xts),deltat=deltat(xts))


         },
         {
           # case CUADRATICA here...5
           print('Modelo cuadrático')
           Timex2=Time*Time
           xts.mod<- lm(xts ~ Time+Timex2)
           xts.serie<-ts(xts.mod$fitted.values,freq=frequency(xts),start=start(xts),deltat=deltat(xts))
           xts.resid<-ts(xts.mod$residuals,freq=frequency(xts),start=start(xts),deltat=deltat(xts))
         },
         {
           # case CUBICO here...6
           print('Modelo Cúbico')
           Timex2=Time*Time
           Timex3=Time*Time*Time
           xts.mod <- lm(xts ~ Time+Timex2+Timex3)
           xts.serie<-ts(xts.mod$fitted.values,freq=frequency(xts),start=start(xts),deltat=deltat(xts))
           xts.resid<-ts(xts.mod$residuals,freq=frequency(xts),start=start(xts),deltat=deltat(xts))
         },

         {
           print('Ts Tend default')
         }
  )

  print("Tendencia finaliza modelado")
  resultados <- list("modelo" = xts.mod, "serie" = xts.serie, "residuo"=xts.resid)
  return(resultados)
}

#' Modelar la estacionalidad de la serie de tiempo
#'
#' Modela una tendencia estacional, devuelve una serie de tiempo.
#'residual que puede ser modelada en AR, MA, ARMA, ARIMA, Holt-Winters
#'
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
TsSeaModel <- function(mtyp,xts)
{
  #Time<- as.vector(time(xts))-as.vector(time(xts))[1]
#  subxts<-end(xts)[1]+end(xts)[2]*deltat(xts)-2*deltat(xts)
#  xts<-window(xts , end=subxts)
  Time<- as.vector(time(xts))
  switch(mtyp,
         {
           # case Ninguno here...1
           print('Ningún Modelo')
           xts.mod<-""            #No hay serie modelada
           xts.serie<-xts*0          #No hay serie modelada
           xts.resid<-xts

         },
         {
           # case Estacionalidad
           It <- forecast::seasonaldummy(xts)
           print('Estacionalidad inicia Modelo')
           xts.mod<- lm(xts ~ Time+It)
           xts.serie<-ts(xts.mod$fitted.values,freq=frequency(xts),start=start(xts),deltat=deltat(xts))
           xts.resid<-ts(xts.mod$residuals,freq=frequency(xts),start=start(xts),deltat=deltat(xts))
          },

         {
           print('Ts Sea default')
         }
  )
  print("Estacionalidad finaliza modelo")
  resultados <- list("modelo" = xts.mod, "serie" = xts.serie, "residuo"=xts.resid)
  return(resultados)
}

#' Modelar la serie de tiempo como AR, MA, ARMA, ARIMA, Holt-Winters
#'
#' Modela la serie de tiempo, devuelve una serie de tiempo
#'residual.
#'
#'
#' @param mtyp, xts, Time
#' @return A matrix of the infile
#' @export
TsModel <- function(mtyp, xts)#recibe TS.sea, devuelve TS.res
{
  #Time<- as.vector(time(xts))-as.vector(time(xts))[1]
  Time<- as.vector(time(xts))
#  subxts<-end(xts)[1]+end(xts)[2]*deltat(xts)-2*deltat(xts)
#  xts<-window(xts , end=subxts)
  switch(mtyp,
         {
           # case Ninguno here...1
           print('Ningún Modelo')
           xts.mod<-""
           xts.serie<-xts
           xts.resid<-xts

         },
         {
           # case GN...2
           print('Ruido Gaussiano')
           xts.mod<-xts.mod<-forecast::auto.arima(xts,d=0, D=0, max.p=0, max.q=0,max.P=0, max.Q=0, max.order=10, max.d=0, max.D=0)
           xts.serie<-ts(rep(mean(xts),length(xts)),freq=frequency(xts),start=start(xts),deltat=deltat(xts))
           xts.resid<-xts-xts.serie

         },
         {
           # case AR...3
           print('Modelo AR')
           xts.mod<-xts.mod<-forecast::auto.arima(xts,d=0, D=0, max.p=5, max.q=0,max.P=0, max.Q=0, max.order=10, max.d=0, max.D=0)
           xts.serie<-xts.mod$fitted
           xts.resid<-xts.mod$residuals

         },
         {
           # case MA...4
           print('Modelo MA')
           xts.mod<-xts.mod<-forecast::auto.arima(xts,d=0, D=0, max.p=0, max.q=5,max.P=0, max.Q=0, max.order=10, max.d=0, max.D=0)
           xts.serie<-xts.mod$fitted
           xts.resid<-xts.mod$residuals
         },
         {
           # case ARMA...5
           print('Modelo ARMA')
           xts.mod<-xts.mod<-forecast::auto.arima(xts,d=0, D=0, max.p=5, max.q=5,max.P=0, max.Q=0, max.order=10, max.d=0, max.D=0)
           xts.serie<-xts.mod$fitted
           xts.resid<-xts-xts.serie
         },
         {
           # case ARIMA...6
           print('Modelo ARIMA')
           xts.mod<-forecast::auto.arima(xts)
           xts.serie<-xts.mod$fitted
           xts.resid<-xts.mod$residuals
         },

         {
           # case Holt-Winters...7
           print('Modelo Holt-Winters')

           xts.mod<-stats::HoltWinters(xts, alpha = NULL, beta = NULL, gamma = NULL,
                                seasonal = "additive")
           xts.serie<-xts.mod$fitted
           xts.resid<-xts-xts.serie

         },

         {
           print('Ts Model default')
         }
  )

  #print("TS Modelo Datos")
  #print(str(xts.mod))
  #print("TS Modelo Summary")
  #print(summary(xts.mod))
  #print("TS Modelo Finaliza")

  resultados <- list("modelo" = xts.mod, "serie" = xts.serie, "residuo"=xts.resid)
  return(resultados)
}

#' Consolidar series de tiempo ajustadas AR, MA, ARMA, ARIMA, Holt-Winters
#'
#' Modela la serie de tiempo, devuelve una serie de tiempo
#'residual.
#'
#'
#' @param mtyp, xts, Time
#' @return A matrix of the infile
#' @export
GroupTs <- function(TsTrans,TsTend,TsSea,TsMod,TendRad,SeaRad,ModRad)#recibe TS.sea, devuelve TS.res
{


  RTrans<-TsTrans

  if(as.numeric(TendRad)==1){
    RTend<-RTrans*0
  }
  else{
    RTend<-fitted(TsTend[[1]])
  }

  if(as.numeric(SeaRad)==1){
    RSea<-RTrans*0
  }
  else
  {
    RSea<-fitted(TsSea[[1]])
  }

  if(as.numeric(ModRad)==1){
    RMod<-RTrans*0
  }
  else
  {
    RMod<-fitted(TsMod[[1]])
  }

  tsas<-ts.intersect(RTrans,RTend,RSea,RMod)

  Trans<-tsas[,1]
  Tend<-tsas[,2]
  Sea<-tsas[,3]
  Mod<-tsas[,4]
  Cons<-Tend+Sea+Mod
  result<-cbind (Trans,Cons)

  return(result)
}



#' Grafica el desempeño del modelado de la serie de tiempo como AR, MA, ARMA, ARIMA, Holt-Winters
#'
#' Grafica el desempeño del modelado de la serie
#'
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
grtsmod <- function(mod, xts, Time)
{
  options(repr.plot.width=6, repr.plot.height=6)
  plot(Time, xts, type = "o", lwd = 2)
  lines(mod$fitted.values, col = "red", lwd = 2)
  legend( "topleft",                              # posicion
          c("real","modelo"),                 # texto
          lwd = c(2, 2),                          # grosor lineas
          col = c('black','red'),                 # color lineas
          bty = "n")                              # sin caja alrededor de la leyenda
  grid()
}

#' Consolidar series de tiempo ajustadas AR, MA, ARMA, ARIMA, Holt-Winters
#'
#' Modela la serie de tiempo, devuelve una serie de tiempo
#'residual.
#'
#'
#' @param mtyp, xts, Time
#' @return A matrix of the infile
#' @export
PronGroupTs <- function(TsTrans,TsTend,TsSea,TsMod,TendRad,SeaRad,ModRad,ciclos,CI)#recibe TS.sea, devuelve TS.res
{
  #OffsetTime<- end(TsTrans)[1]+end(TsTrans)[2]*deltat(TsTrans)
  #OffsetVal<-tail(as.vector(TsTrans),1)

  ForcTsIni<-end(TsTrans)[1]+end(TsTrans)[2]*deltat(TsTrans)-1*deltat(TsTrans)
  ForcTsEnd<-end(TsTrans)[1]+end(TsTrans)[2]*deltat(TsTrans)+ciclos*deltat(TsTrans)
  ForcN<-ciclos*deltat(TsTrans)
  Ltrans<-data.frame(Time=seq(from = ForcTsIni-deltat(TsTrans), to = ForcTsEnd, by=deltat(TsTrans)))
  Ltransx2<-data.frame(Timex2=seq(from = ForcTsIni-deltat(TsTrans), to = ForcTsEnd, by=deltat(TsTrans))^2)
  Ltransx3<-data.frame(Timex3=seq(from = ForcTsIni-deltat(TsTrans), to = ForcTsEnd, by=deltat(TsTrans))^3)
  Dtransx2<-cbind(Ltrans,Ltransx2)
  Dtransx3<-cbind(Ltrans,Ltransx2,Ltransx3)
  dummyts<-ts(Ltrans,freq=frequency(TsTrans),start=ForcTsIni,deltat=deltat(TsTrans))
  dummytsdf<-data.frame(It=I(forecast::seasonaldummy(dummyts)) )
  seadf<-cbind(Ltrans,dummytsdf)


  if(as.numeric(TendRad)==2){#Reg lineal
   #pronosticar tendencia#ver que pasa con los corrimientos de tiempo.
    PTend <- predict(TsTend[[1]],Ltrans,
                     se.fit = TRUE,
                level = as.numeric(CI))
    FTend<-ts(PTend$fit,freq=frequency(TsTrans),start=ForcTsIni, deltat=deltat(TsTrans))
  }
  else{
    if(as.numeric(TendRad)==3){ #Reg. cuadrática
      #pronosticar tendencia#ver que pasa con los corrimientos de tiempo.
      PTend <- predict(TsTend[[1]],Dtransx2,
                       se.fit = TRUE,
                       level = as.numeric(CI))
      FTend<-ts(PTend$fit,freq=frequency(TsTrans),start=ForcTsIni, deltat=deltat(TsTrans))
    }
    else{
      if(as.numeric(TendRad)==4){#Reg Cúbica
        #pronosticar tendencia#ver que pasa con los corrimientos de tiempo.
        PTend <- predict(TsTend[[1]],Dtransx3,
                         se.fit = TRUE,
                         level = as.numeric(CI))
        FTend<-ts(PTend$fit,freq=frequency(TsTrans),start=ForcTsIni, deltat=deltat(TsTrans))
      }
      else{
        print("Caso de regresión lineal pérdida")

      }

    }

  }

  if(as.numeric(SeaRad)!=1)
  {
    #pronosticar estacionalidad
    PSea <- predict(TsSea[[1]],seadf,
                    se.fit = TRUE,na.action=na.pass,
                    level = as.numeric(CI))
    FSea<-ts(PSea$fit,freq=frequency(TsTrans),start=ForcTsIni,deltat =deltat(TsTrans))
      }

  if(as.numeric(ModRad)!=1){
    PMod <- predict(TsMod[[1]], n.ahead = ForcN,
                    prediction.interval = TRUE,
                    level = as.numeric(CI))

      if(ModRad!=7){
         #pronosticar modelo de TS #por ser modelos ARIMA

   FMod<-PMod$pred
      }
    else{

      FMod<-PMod[,1]
    }
  }

  #Modelo con Tendencia, Estacionalidad y Modelado TS
  if((as.numeric(TendRad)!=1) & (as.numeric(SeaRad)!=1) & (as.numeric(ModRad)!=1)    )
  {
    print("Caso: 111")
    tsas<-ts.intersect(FTend,FSea,FMod)
    Tend<-tsas[,1]
    Sea<-tsas[,2]
    Mod<-tsas[,3]
    Cons<-Tend+Sea+Mod
  }
  else{
  #Modelo con Tendencia, Estacionalidad
  if((as.numeric(TendRad)!=1) & (as.numeric(SeaRad)!=1) & (as.numeric(ModRad)==1)  )
  {
    print("Caso: 110")
    tsas<-ts.intersect(FTend,FSea)
    Tend<-tsas[,1]
    Sea<-tsas[,2]
    Cons<-Tend+Sea
  }
    else{

      #Modelo con Tendencia y Modelado TS
      if((as.numeric(TendRad)!=1) & (as.numeric(SeaRad)==1) & (as.numeric(ModRad)!=1))
      {
        print("Caso: 101")
        tsas<-ts.intersect(FTend,FMod)
        Tend<-tsas[,1]
        Mod<-tsas[,2]
        Cons<-Tend+Mod
      }
      else{
        #Estacionalidad y Modelado TS
        if((as.numeric(TendRad)==1) & (as.numeric(SeaRad)!=1) & (as.numeric(ModRad)!=1))
        {
          print("Caso: 011")
          tsas<-ts.intersect(FSea,FMod)
          Sea<-tsas[,1]
          Mod<-tsas[,2]
          Cons<-Tend+Sea+Mod
          }
        else{
            #Modelo con Tendencia
          if((as.numeric(TendRad)!=1) & (as.numeric(SeaRad)==1) & (as.numeric(ModRad)==1) )
          {
            print("Caso: 100")
            Cons<-FTend
            }
          else{
            #Modelo con Estacionalidad
            if((as.numeric(TendRad)==1) & (as.numeric(SeaRad)!=1) & (as.numeric(ModRad)==1)   )
            {
              print("Caso: 010")
              Cons<-FSea

            }
            else{
                  #Modelo con Modelado TS
              if((as.numeric(TendRad)==1) & (as.numeric(SeaRad)==1) & (as.numeric(ModRad)!=1) )
              {
                  print("Caso: 001")
                    Cons<-FMod
                  }
              else{
                #Ninguno (retorna cero)
                if((as.numeric(TendRad)==1) & (as.numeric(SeaRad)==1) & (as.numeric(ModRad)==1) )
                {
                  print("Caso: 000")

                  Cons<-ts(0,freq=frequency(TsTrans),start=ForcTsIni,deltat=deltat(xts))

                }
                else{

                  print("Caso de pronóstico perdido,revisar")

                }

            }


        }

    }
  }

  print('finalizó pronóstico')
  Cons[1]<-tail(as.vector(TsTrans),1)
  return(Cons)
      }
    }
  }
}


#' Demo text
#'
#' Demo text
#'
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
tsmodfordummy <- function()
{

  mes<-"Hola Mundo desde tsmodfor 2019/09/29_20:16"
  return(mes)
}

#' Ejecutar aplicación Shiny de análisis de series de tiempo
#'
#' Aplicación que permite analizar series de tiempo  y realizar modelamiento con series
#' que contienen tendencia Lineal, Cuadrática o Cúbica.
#' Estacionalidad, patrones modelables como AR, MA, ARMA, ARIMA y Holt-Winters.
#'
#'
#' @param none
#' @return none
#' @export
appshinyts <- function() {
  require(shiny)
  require(dygraphs)

  shinyApp(
#______________
  # Define UI for application that draws a histogram
  ui <- navbarPage(
    tabPanel("Component A"),
    tabPanel("Carga de datos",
             # Application title
             #  titlePanel("Old Faithful Geyser Data"),
             titlePanel("Análisis y Pronóstico de Series de Tiempo"),

             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(

                 # Input: Select a file ----
                 fileInput("file1", "Elejir archivo CSV (1 col)",
                           multiple = TRUE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),

                 checkboxInput("showgrid", label = "Mostrar Grilla", value = TRUE),
                 hr(),
                 div(strong("Desde: "), textOutput("from", inline = TRUE)),
                 div(strong("Hasta: "), textOutput("to", inline = TRUE)),

                 # value is always yyyy-mm-dd, even if the display format is different
                 dateInput("DateIn", "Fecha inicial:", value = "2000/01/01", format = "yyyy/mm/dd"),

                 selectInput("DataFreq", label = h3("Seleccionar frecuencia"),
                             choices = list("1" = 1, "4" = 4, "12" = 12),
                             selected = 12)

                 #          div(strong("Date clicked: "), textOutput("clicked", inline = TRUE)),
                 #          div(strong("Nearest point clicked: "), textOutput("point", inline = TRUE))


               ),
               mainPanel(
                 dygraphOutput("dygraph"),
                 #verbatimTextOutput("demo")
                 dygraphOutput("TsSub")
                 #               plotOutput("RawTsPlot")
                 #               tableOutput("contents")
                 #          verbatimTextOutput("from"),
                 #          verbatimTextOutput("to")
               )
             )
    ),
    #________________________________________________________________________________________

    tabPanel("Análisis Exploratorio",
             # Application title
             #  titlePanel("Old Faithful Geyser Data"),

             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(


                 sliderInput("bins",
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30),

                 radioButtons("ExpRanWalk", label = h3("Tranformar"),
                              choices = list("No" = 1, "Diferenciar" = 2, "Logaritmo" = 3),
                              selected = 1),
                 actionButton("But_TransTS", label = "Transformar serie")

               ),
               mainPanel(
                 plotOutput("WorkTsPlot"),
                 plotOutput("distPlot"),
                 plotOutput("AcfPlot"),
                 plotOutput("PacfPlot"),
                 plotOutput("StlPlot"),
                 plotOutput("AcfRandomPlot"),
                 plotOutput("PacfRandomPlot"),
                 plotOutput("SpectrumPlot")

               )
             )
    ),
    #________________________________________________________________________________________

    tabPanel("Modelo",

             titlePanel("Modelo"),

             fluidRow(

               column(4,
                      wellPanel(

                        radioButtons("ModTendRad01", label = h3("Tipo de Tendencia",align='centered'),
                                     choices = list("Ninguna" = 1, "Lineal" = 2, "Cuadrática" = 3, "Cúbica" = 4),
                                     selected = 1),

                        conditionalPanel(
                          condition = "input.ModTendRad01 == 2", #Opciones lineal

                          radioButtons("ModTendLinRad01", label = h4("Opciones Modelo Lineal"),
                                       choices = list("Con Intercepto" = 1, "Sin intercepto" = 2),
                                       selected = 1)
                        ),

                        conditionalPanel(
                          condition = "input.ModTendRad01 == 3", #Opciones cuadrático

                          radioButtons("ModTendCuaRad01", label = h4("Opciones Modelo Cuadrático"),
                                       choices = list("Con Intercepto" = 1, "Sin intercepto" = 2),
                                       selected = 1)
                        ),

                        conditionalPanel(
                          condition = "input.ModTendRad01 == 4", #Opciones cúbico

                          radioButtons("ModTendCubRad01", label = h4("Opciones Modelo Cúbico"),
                                       choices = list("Con Intercepto" = 1, "Sin intercepto" = 2),
                                       selected = 1)
                        )


                      )#cierre wellpanel
               ),

               column(4,
                      wellPanel(

                        radioButtons("ModEstRad01", label = h3("Tipo de Estacionalidad"),
                                     choices = list("Ninguna" = 1, "Hay estacionalidad" = 2),
                                     selected = 1),

                        conditionalPanel(
                          condition = "input.ModEstRad01 == 2", #Opciones Estacionalidad

                          numericInput("ModEstFreq", label = "Períodos por Ciclo",
                                       value = 4, min = 1, max = 24, step = 1),

                          # Copy the line below to make a text input box
                          textInput("ModEstText", label = h4("Ingrese patrón de estacionalidad"), value = "0100")
                        )


                      )
               ),
               column(4,
                      wellPanel(

                        radioButtons("TsModRad01", label = h3("Modelo residuo"),
                                     choices = list("Ninguno"=1,"Ruido Gaussiano" = 2, "AR" = 3, "MA" = 4, "ARMA" = 5, "ARIMA" = 6, "Holt-Winters" = 7),
                                     selected = 1),

                        conditionalPanel(
                          condition = "input.TsModRad01 == 3", #Opciones AR

                          radioButtons("TsModARRad01", label = h4("Opciones Modelo AR"),
                                       choices = list("Manual" = 1, "Automático" = 2),
                                       selected = 1),

                          conditionalPanel(
                            condition = "input.TsModARRad01 == 1", #Opciones AR Manual

                            numericInput("TsModARRad01", label = "Orden del Modelo AR",
                                         value = 1, min = 1, max = 12, step = 1)
                          )),
                        #_________
                        conditionalPanel(
                          condition = "input.TsModRad01 == 4", #Opciones MA

                          radioButtons("TsModMARad01", label = h4("Opciones Modelo MA"),
                                       choices = list("Manual" = 1, "Automático" = 2),
                                       selected = 1),

                          conditionalPanel(
                            condition = "input.TsModMARad01 == 1", #Opciones MA Manual

                            numericInput("TsModMAOr", label = "Orden del Modelo MA",
                                         value = 1, min = 1, max = 12, step = 1)
                          )),
                        #_________
                        conditionalPanel(
                          condition = "input.TsModRad01 == 5", #Opciones ARMA

                          radioButtons("TsModARMARad01", label = h4("Opciones Modelo ARMA"),
                                       choices = list("Manual" = 1, "Automático" = 2),
                                       selected = 1),

                          conditionalPanel(
                            condition = "input.TsModARMARad01 == 1", #Opciones MA Manual

                            numericInput("TsModARMAOr", label = "Orden del Modelo",
                                         value = 1, min = 1, max = 12, step = 1)
                          )),
                        #_________
                        conditionalPanel(
                          condition = "input.TsModRad01 == 6", #Opciones ARIMA

                          radioButtons("TsModARIMARad01", label = h4("Opciones Modelo ARIMA"),
                                       choices = list("Manual" = 1, "Automático" = 2),
                                       selected = 1),

                          conditionalPanel(
                            condition = "input.TsModARIMARad01 == 1", #Opciones ARIMA Manual

                            numericInput("TsModARIMAOr", label = "Orden del Modelo ARIMA",
                                         value = 1, min = 1, max = 12, step = 1)
                          )),
                        #_________

                        conditionalPanel(
                          condition = "input.TsModRad01 == 7", #Opciones Holt-Winters

                          radioButtons("TsModHWRad01", label = h4("Opciones Modelo Holt-Winters"),
                                       choices = list("Manual" = 1, "Automático" = 2),
                                       selected = 1),
                          radioButtons("TsModHWRad02", label = h4("Mod. Aditivo/Multiplicativo"),
                                       choices = list("Aditivo" = 1, "Multiplicativo" = 2),
                                       selected = 1),

                          conditionalPanel(
                            condition = "input.TsModHWRad01 == 1", #Opciones Holt-Winters Manual

                            checkboxGroupInput("TsModHWChk01", label = h4("Estimar parámetros Modelo Holt-Winters"),
                                               choices = list("alpha" = 1, "beta" = 2, "gamma" = 3),
                                               selected = 1)

                          ))
                        #_________

                      )#cierre wellpanel
               )
             ),#fin fluidrow, menus principales

             fluidRow(

               column(4,
                      wellPanel(align="left", actionButton("ButTend", "Ajustar Tendencia!") )
               ),
               column(4,
                      wellPanel(align="center", actionButton("ButSea", "Ajustar Estacionalidad!") )
               ),
               column(4,
                      wellPanel(align="right", actionButton("ButMod", "Ajustar Modelo!") )
               )


             ),#fin fluid row ajuste modelo
             fluidRow(h4("Ajuste Tendencia"),

                      column(4,
                             wellPanel("Serie Vs. Modelo",
                                       plotOutput("ModTendSvsMPlot")
                             )
                      ),

                      column(4,
                             wellPanel("Residuos",

                                       plotOutput("ModTendTendPlot")
                             )
                      ),
                      #             column(4,
                      #                    wellPanel("Q-Q plot",
                      #                              tableOutput("TendnText")
                      #                     )
                      #             ),
                      column(4,
                             wellPanel("ACF Residuos",

                                       plotOutput("ModTendAcfResPlot")
                             )
                      )
             ),#Fin fluid row gráficos de diagnostico tendencia
             fluidRow(h4("Ajuste Estacionalidad"),

                      column(4,
                             wellPanel("Serie Vs. Modelo",
                                       plotOutput("ModSeaSvsMPlot")
                             )
                      ),

                      column(4,
                             wellPanel("Residuos",

                                       plotOutput("ModSeaTendPlot")
                             )
                      ),
                      #            column(3,
                      #                    wellPanel("Q-Q plot",
                      #                              tableOutput("SeanText")
                      #                    )
                      #             ),
                      column(4,
                             wellPanel("ACF Residuos",

                                       plotOutput("ModSeaAcfResPlot")
                             )
                      )
             ),#Fin fluid row gráficos de diagnostico season

             fluidRow(h4("Ajuste Modelo TS"),

                      column(4,
                             wellPanel("Serie Vs. Modelo",
                                       plotOutput("TsModSvsMPlot")
                             )
                      ),

                      column(4,
                             wellPanel("Residuos",

                                       plotOutput("TsModTendPlot")
                             )
                      ),
                      #            column(3,
                      #                    wellPanel("Q-Q plot",
                      #                              tableOutput("SeanText")
                      #                    )
                      #             ),
                      column(4,
                             wellPanel("ACF Residuos",

                                       plotOutput("TsModAcfResPlot")
                             )
                      )
             ),#Fin fluid row gráficos de diagnostico modelo TS
             fluidRow(h4("Ajuste Debug"),

                      column(4,
                             wellPanel("Debug 1",
                                       textOutput("debugtext1")
                             )
                      ),

                      column(4,
                             wellPanel("Debug 2",
                                       textOutput("debugtext2")
                             )
                      ),
                      column(4,
                             wellPanel("Debug 3"
                                       #                              tableOutput("nText")

                             )
                      )

             )#fin Fluid Row Debug




    ),#Cierre tab modelo
    #________________________________________________________________________________________
    tabPanel("Desempeño",
             titlePanel("Desempeño"),


             fluidRow(

               column(12,
                      wellPanel(h4("Serie Vs. Modelo"),
                                dygraphOutput("DesSvsM2Plot")
                      )
               )),
             fluidRow(
               column(12,
                      wellPanel(h4("Consolidado Serie Vs. Modelo"),
                                dygraphOutput("DesSvsM3Plot")
                      )
               )),

             fluidRow(

               column(6,
                      wellPanel(h4("Serie Vs. Modelo"),
                                plotOutput("DesSvsMPlot")
                      )
               ),

               column(6,
                      wellPanel(h4("Residuos"),

                                plotOutput("DesResPlot")
                      )
               )),
             fluidRow(
               column(6,
                      wellPanel(h4("Q-Q plot"),
                                plotOutput("DesQQPlot")
                      )
               ),
               column(6,
                      wellPanel(h4("ACF Residuos"),

                                plotOutput("DesAcfResPlot")
                      )
               )
             )#Fin fluid row gráficos de desempleño



    ),
    #________________________________________________________________________________________
    tabPanel("Pronóstico",

             titlePanel("Pronóstico de series de Tiempo"),

             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(

                 checkboxInput("CI", label = "Mostrar Intervalos de confianza", value = TRUE),
                 sliderInput("forper", "Períodos a pronósticar:",
                             min = 1, max = 1000,
                             value = 120),

                 conditionalPanel(
                   condition = "input.CI == true", #Opciones pronóstico
                   selectInput("interval", label = "Prediction Interval",
                               choices = c("0.80", "0.90", "0.95", "0.99"),
                               selected = "0.95")
                 ),

                 wellPanel(align="center", actionButton("ButPron", "Pronosticar!") )

               ),
               mainPanel(
                 # Output: Data file ----
                 dygraphOutput("dygraphforecast")

               )
             )
    ),
    tabPanel("Pronóstico + Incertidumbres",

             titlePanel("Pronóstico de series de Tiempo"),

             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(



               ),
               mainPanel(
                 # Output: Data file ----
                 dygraphOutput("dygraphforecastincert")

               )
             )
    )

  ),

#SERVER####################################################################################


  # Define server logic required to draw a histogram
  server <- function(input, output) {

    #PROCESO DE SERIES DE TIEMPO

    x.ts <- reactive({
      #x.ts <-  ts(as.vector(read.csv('/home/andraxas/Data_Science_Reference/07_Projects/05_Shiny/Demo_TS01/data.csv', header=TRUE,sep=',')), start = 1980, fr = 4)
      req(input$file1)
      infile <- input$file1
      if (is.null(infile)) {      # User has not uploaded a file yet
        return(NULL)
      }
      ts(as.vector(read.csv(infile$datapath, header=TRUE,sep=',')[[1]]), start = c(as.integer(substr(input$DateIn, 1, 4)),as.integer(substr(input$DateIn, 6, 7))), frequency =as.integer(input$DataFreq) )
    })


    #paste (substr(input$DateIn, 1, 4),substr(input$DateIn, 6, 7), input$DataFreq, sep = ", ")

    x.sub.ts <- reactive({
      req(input$file1)
      tstart<-c(as.integer(substr(strftime(req(input$dygraph_date_window[[1]]), " %Y %m %d "), 1, 5)), as.integer(substr(strftime(req(input$dygraph_date_window[[1]]), " %Y %m %d "), 6, 8)))
      tsend<-c(as.integer(substr(strftime(req(input$dygraph_date_window[[2]]), " %Y %m %d "), 1, 5)), as.integer(substr(strftime(req(input$dygraph_date_window[[2]]), " %Y %m %d "), 6, 8)))
      txsub<-window(x.ts(),start=tstart,end=tsend)

      return(txsub)
    })

    h <- reactiveValues(But_TransTS=FALSE)

    observeEvent(input$But_TransTS,
                 {
                   h$But_TransTS = TRUE
                   v$ButTend=FALSE
                   r$ButSea=FALSE
                   t$ButPron=FALSE

                 })



    x.trans.ts <- reactive({
      #    if (h$But_TransTS){
      req(input$ExpRanWalk)
      mtyp=as.numeric(input$ExpRanWalk)
      tem<-TsTrans(mtyp, x.sub.ts())
      #      h$But_TransTS = FALSE
      return(tem)
      #    }
    })

    v <- reactiveValues(ButTend=FALSE)

    observeEvent(input$ButTend, {
      v$ButTend = TRUE
    })

    ModTend <- reactive({
      if (v$ButTend){
        print("botón presionado/Calculo de tendencia")
        mtyp=as.numeric(input$ModTendRad01)
        print(mtyp)
        savedlist <- TsTendModel(mtyp, x.trans.ts())
        return(savedlist)
      }
    })



    x.tend.ts <- reactive({
      if(v$ButTend){
        tem<-ModTend()[[3]]#residuo del modelo de tendencia
        return(tem)
      }
    })

    r <- reactiveValues(ButSea=FALSE)

    observeEvent(input$ButSea, {
      r$ButSea = TRUE
    })

    ModSea <- reactive({
      if (r$ButSea){
        print("botón presionado/Calculo de Estacionalidad")
        mtyp=as.numeric(input$ModEstRad01)
        print(mtyp)
        savedlist <- TsSeaModel(mtyp,x.tend.ts())
        #     print("Estacionalidad modelo xx")
        return(savedlist)
      }
    })

    x.sea.ts <- reactive({
      if(r$ButSea){
        tem<-ModSea()[[3]]#residuo del modelo de estacionalidad
        return(tem)
      }
    })

    s <- reactiveValues(ButMod=FALSE)

    observeEvent(input$ButMod, {
      s$ButMod = TRUE
    })

    TsMod <- reactive({
      if (s$ButMod){
        print("botón presionado/Calculo de modelo TS")
        mtyp=as.numeric(input$TsModRad01)
        print(mtyp)
        savedlist <- TsModel(mtyp, x.sea.ts())
        return(savedlist)
      }
    })
    #_________________________________________________________________________


    output$contents <- renderTable({

      if (is.null(x.trans.ts())) {
        return(NULL)
      }
      else
      {
        head(x.trans.ts(),3)
      }

    })

    output$dygraph <- renderDygraph({
      req(input$file1)
      dygraph(x.ts(), main = "Serie de tiempo") %>%
        dyRangeSelector() %>%
        dyOptions(drawGrid = input$showgrid)
      #     dyAxis(name = 'y',valueRange = c(100, 500))
      #       dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%


    })

    output$TsSub <- renderDygraph({
      req(input$file1)
      dygraph(x.sub.ts(), main = "Serie de tiempo seleccionada") %>%
        dyOptions(drawGrid = input$showgrid)
    })


    # output$demo <- renderText({
    #    paste (substr(strftime(req(input$dygraph_date_window[[1]]), " %Y %m %d "), 1, 5),substr(strftime(req(input$dygraph_date_window[[1]]), " %Y %m %d "), 6, 8), input$DataFreq, sep = "*")
    #  })

    output$from <- renderText({
      strftime(req(input$dygraph_date_window[[1]]), " %Y %B %d ")
    })

    output$to <- renderText({
      strftime(req(input$dygraph_date_window[[2]]), "%Y %B %d")
    })

    output$clicked <- renderText({
      strftime(req(input$dygraph_click$x), "%d %b %Y")
    })

    output$point <- renderText({
      paste0('X = ', strftime(req(input$dygraph_click$x_closest_point), "%d %b %Y"),
             '; Y = ', req(input$dygraph_click$y_closest_point))
    })

    output$RawTsPlot <- renderPlot({
      plot(x.ts())
    })




    #______________________________________________________________________
    #GRÁFICAS ANÁLISIS EXPLORATORIO

    output$WorkTsPlot <- renderPlot({
      #     stl(x,s.window=1)
      plot(x.trans.ts(),main="Tramo de serie de tiempo a procesar")
      #     return(m$figure)
    })

    output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      #      x    <- faithful[, 2]
      #      x<-ts(as.vector(df()[[1]]), start = 1980, fr = 4)
      bins <- seq(min(x.trans.ts()), max(x.trans.ts()), length.out = input$bins + 1)

      # draw the histogram with the specified number of bins
      hist(x.trans.ts(), breaks = bins, col = 'darkgray', border = 'white',freq = FALSE, main="Histograma de la serie de tiempo")
      lines(density(x.trans.ts()),col='red', lwd = 2,main="Función de densidad de probabilidad de la serie de tiempo")

    })

    output$AcfPlot <- renderPlot({
      acf(x.trans.ts(),main="ACF de la serie de tiempo")
    })

    output$PacfPlot <- renderPlot({
      pacf(x.trans.ts(),main="Pacf de la serie de tiempo")
    })

    decom <- reactive({
      decompose(x.trans.ts(),'multiplicative')
    })

    output$StlPlot <- renderPlot({
      plot(decom())
    })

    output$AcfRandomPlot <- renderPlot({
      acf(na.omit((decom())$random), main="ACF de los residuos de la descomposición")
    })

    output$PacfRandomPlot <- renderPlot({
      pacf(na.omit((decom())$random), main="PACF de los residuos de la descomposición")
    })

    output$SpectrumPlot <- renderPlot({
      spectrum(na.omit(x.trans.ts()), main="Espectro en el dominio de la frecuencia")
    })



    #_________________________________________________________________________________
    #MODELOS
    # You can access the values of the widget (as a vector)
    # with input$radio, e.g.
    output$ModTendRad01 <- renderPrint({ input$ModTendRad01 })
    output$ModTendLinRad01 <- renderPrint({ input$ModTendLinRad01 })
    output$ModEstRad01 <- renderPrint({ input$ModEstRad01 })
    output$ModEstFreq <- renderPrint({ input$ModEstFreq })
    output$ModEstText <- renderPrint({ input$ModEstText })



    #*******************************************************************************************
    #MODELO DE TENDENCIA

    output$ModTendSvsMPlot <- renderPlot({
      if(v$ButTend){# plot only after button because before list dont exist

        #    options(repr.plot.width=6, repr.plot.height=6)
        plot( x.trans.ts(), type='o', lwd=2)
        lines(ModTend()[[2]], col='red',lwd=2)
        legend( "topleft",
                c("TS real","Modelo"),
                lwd = c(2, 2),
                col = c('black','red'),
                bty = "n")
        grid()

      }
    })


    output$ModTendTendPlot <- renderPlot({
      if(v$ButTend){# plot only after button because before list dont exist
        plot(ModTend()[[3]])

      }
    })

    output$ModTendAcfResPlot <- renderPlot({
      if(v$ButTend){# plot only after button because before list dont exist
        acf(ModTend()[[3]])
      }
    })

    # output$TendnText <- renderTable({
    #    if(v$ButTend){# plot only after button because before list dont exist
    #        head(as.vector(ModTend()[[3]]))
    #    }
    #  })


    #*******************************************************************************************
    #*******************************************************************************************
    #MODELO DE ESTACIONALIDAD


    output$ModSeaSvsMPlot <- renderPlot({
      if(r$ButSea){# plot only after button because before list dont exist
        #    options(repr.plot.width=6, repr.plot.height=6)
        plot( x.tend.ts(), type='o', lwd=2)
        lines(ModSea()[[2]], col='red',lwd=2)
        legend( "topleft",
                c("TS real","Modelo"),
                lwd = c(2, 2),
                col = c('black','red'),
                bty = "n")
        grid()

      }
    })


    output$ModSeaTendPlot <- renderPlot({
      if(r$ButSea){# plot only after button because before list dont exist
        plot(ModSea()[[3]])

      }
    })

    output$ModSeaAcfResPlot <- renderPlot({
      if(r$ButSea){# plot only after button because before list dont exist
        acf(ModSea()[[3]])
      }
    })

    # output$SeanText <- renderTable({
    #    if(v$ButSea){# plot only after button because before list dont exist
    #      head(as.vector(ModSea()[[3]]))
    #    }
    #  })


    #*******************************************************************************************
    #MODELO DE TS


    output$TsModSvsMPlot <- renderPlot({
      if(s$ButMod){# plot only after button because before list dont exist

        #    options(repr.plot.width=6, repr.plot.height=6)
        tsas<-ts.intersect(x.sea.ts(),TsMod()[[2]])
        plot( tsas[,1], type='o', lwd=2)
        lines(tsas[,2], col='red',lwd=2)
        legend( "topleft",
                c("TS Real","Modelo"),
                lwd = c(2, 2),
                col = c('black','red'),
                bty = "n")
        grid()

      }
    })


    output$TsModTendPlot <- renderPlot({
      if(s$ButMod){# plot only after button because before list dont exist
        plot(TsMod()[[3]])

      }
    })

    output$TsModAcfResPlot <- renderPlot({
      if(s$ButMod){# plot only after button because before list dont exist
        acf(TsMod()[[3]])
      }
    })



    #*******************************************************************************************
    #DESEMPEÑO
    output$DesSvsMPlot <- renderPlot({
      if(s$ButMod){# plot only after button because before list dont exist

        #    options(repr.plot.width=6, repr.plot.height=6)
        tsas<-ts.intersect(x.sea.ts(),TsMod()[[2]])
        plot( tsas[,1], type='o', lwd=2)
        lines(tsas[,2], col='red',lwd=2)
        legend( "topleft",
                c("TS Real","Modelo"),
                lwd = c(2, 2),
                col = c('black','red'),
                bty = "n")
        grid()

      }
    })


    output$DesResPlot <- renderPlot({
      if(s$ButMod){# plot only after button because before list dont exist
        plot(TsMod()[[3]])

      }
    })

    output$DesAcfResPlot <- renderPlot({
      if(s$ButMod){# plot only after button because before list dont exist
        acf(TsMod()[[3]])
      }
    })

    output$DesQQPlot <- renderPlot({
      if(s$ButMod){# plot only after button because before list dont exist
        qqnorm(TsMod()[[3]])
        abline(0,1)

      }
    })

    output$DesSvsM2Plot <- renderDygraph({
      if(s$ButMod){# plot only after button because before list dont exist

        upper <- fitted(TsMod()[[1]]) + 1.96*sqrt(TsMod()[[1]]$sigma2)
        lower <- fitted(TsMod()[[1]]) - 1.96*sqrt(TsMod()[[1]]$sigma2)
        tsas<-ts.intersect(x.sea.ts(),fitted(TsMod()[[1]]),upper,lower)
        ser<-tsas[,1]
        mft<-tsas[,2]
        mup<-tsas[,3]
        mlw<-tsas[,4]

        all <- cbind(ser, mft, mup, mlw)

        dygraph(all, "Real Vs. Modelo") %>%
          dySeries("ser", label = "Serie actual") %>%
          dySeries(c("mlw","ser","mup"), label = "Modelo")

      }
    })

    output$DesSvsM3Plot <- renderDygraph({
      if(s$ButMod){# plot only after button because before list dont exist



        GTs<-GroupTs(x.trans.ts(),ModTend(),ModSea(),TsMod(),input$ModTendRad01,input$ModEstRad01,input$TsModRad01)


        dygraph(GTs, "Real Vs. Modelo")%>%
          dySeries("Trans", label = "Serie original") %>%
          dySeries("Cons", label = "Serie modelada")
        #        dySeries(c("mlw","ser","mup"), label = "Modelo")

      }
    })
    #################################################################################33
    #PRONÓSTICO
    t <- reactiveValues(ButPron=FALSE)

    observeEvent(input$ButPron, {
      t$ButPron = TRUE
    })

    predicted <- reactive({

      if(t$ButPron){

        PronGroupTs(x.trans.ts(),ModTend(),ModSea(),TsMod(),input$ModTendRad01,input$ModEstRad01,input$TsModRad01,as.numeric(input$forper),as.numeric(input$interval))#recibe TS.sea, devuelve TS.res

      }
    })

    output$dygraphforecast <- renderDygraph({
      if(t$ButPron){


        #     upr <- predicted()$pred + (2 * predicted()$se) ## upper and lower confidence intervals
        #      lwr <- predicted()$pred - (2 * predicted()$se) ## approximate 95% pointwise

        #     all <- cbind(x.trans.ts(), predicted()$pred, upr, lwr)



        PData <- cbind(x.trans.ts(), predicted())
        print("Pronostico 01")
        dygraph(PData, "Pronóstico") %>%
          dySeries("x.trans.ts()", label = "Serie actual") %>%
          dySeries("predicted()", label = "Serie Pronósticada")
        #        dySeries(c("lwr","predicted()$pred","upr"), label = "Pronóstico")

      }
    })


    #*******************************************************************************************

    output$dygraphforecastincert <- renderDygraph({
      if(t$ButPron){


        prediction <- predict(TsMod()[[1]], n.ahead = as.numeric(input$forper),
                              prediction.interval = TRUE,
                              level = as.numeric(input$interval))

        upr <- prediction$pred + (2 * prediction$se) ## upper and lower confidence intervals
        lwr <- prediction$pred - (2 * prediction$se) ## approximate 95% pointwise

        all <- cbind(x.trans.ts(), prediction$pred, upr, lwr)



        print("Pronostico 01")
        dygraph(all, "Pronóstico") %>%
          dySeries("x.trans.ts()", label = "Serie actual") %>%
          dySeries("prediction$pred", label = "Serie Pronósticada") %>%
          dySeries(c("lwr","prediction$pred","upr"), label = "Pronóstico")

      }
    })


    output$debugtext2 <- renderText({
      str(TsMod()[[2]])
    })

    output$debugtext1 <- renderText({
      str(x.sea.ts())
    })

  }
)#cierra Shiny app.
}

#  tsshinyapp<-function( var)
#  {
#    require(shiny)
#    shinyApp(ui = ui, server = server)


