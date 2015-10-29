


#############################     PIE    #########################################33
tabocupa <- table (SDEMT215$CLASE1)
tabocprc <- (round(((tabocupa/margin.table(tabocupa))*100),1))
etiquetas <- c("Menos15","PEA", "PNEA")
etiquetas <- paste(etiquetas, tabocprc)
etiquetas <- paste(etiquetas, "%", sep="")
pie (tabocprc, labels = etiquetas, clockwise = TRUE, 
     main = "Poblacion", sub = "Fuente: calculos propios con base en la ENOE 2015")


#########################     BARPLOT    #############################
table(SDEMT215$EDA5C)
barplot (table(SDEMT215$EDA5C), 
         main = "Grafica 1. Grupos de edad, Mexico, 2015", 
         sub= "Fuente: calculos propios con base en la ENOE 2015", 
         xlab = "grupos de edad", 
         ylab= "Numero de personas", 
         ylim = c(0,120000), 
         names.arg=c("NA","15 a 24","25 a 44","45 a 64","65 +","NA"),
         col = c(41,42,43, 44))

#############################    PLOT  #####################

desocupa <- read.csv ("desocupa.csv")
desocupa1 <- read.csv ("desocupa1.csv")
desocupa1 <- ts(desocupa1, start = 2005, frequency = 4)
desocupa <- ts(desocupa, start = 2005, frequency = 4)

plot(desocupa, plot.conf=FALSE, ylab="Tasa de Desocupacion",     
     xlab="ano",  main="Tasa de desocupacion, Mexico 2005 - 2015", 
     fcol="white", type="o", ylim = c(2,10))
lines((desocupa), col="blue", type="o")
lines((desocupa1), col="red", type= "o")

legend("topleft",lty=1, col=c("blue","red"), c("datos1", "datos2"),pch=1)

###############          PIRAMIDE    ###################

install.packages("plotrix")
require(plotrix)
attach(SDEMT215)
SDEMT215$EDA1 <- as.numeric (as.character (SDEMT215$EDA))
SDEMT215$SEX1 <- as.numeric (as.character (SDEMT215$SEX))
agegrp<-cut(SDEMT215$EDA1, breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,97))
women = as.vector(100*table(agegrp[SDEMT215$SEX1==2])/sum(SDEMT215$SEX1==2))
men = as.vector(100*table(agegrp[SDEMT215$SEX1==1])/sum(SDEMT215$SEX1==1))
pyramid.plot(men, women, 
             labels=c("0-4","5-9","10-14","15-19", 
                      "20-24","25-29","30-34","35-39","40-44",
                      "45-49","50-54","55-59","60-64","65-69","70-74",
                      "75-79","80-84","85 y m?s"), 
             main="Pir?mide Poblacional por grupos de edad, M?xico, 2015",
             top.labels=c("Masculino", "Femenino"), gap=2, space=0, 
             lxcol = "deepskyblue3", rxcol = "firebrick4",
             laxlab = c(0,3,6,9,12),raxlab = c(0,3,6,9,12))

