
#################pegado de bases ####################3
library (foreign)
sdem215 <- read.dbf ("/home/sgr/Dropbox/Dropbox1/art br-mx/base mx/SDEMT215.dbf")
coe1t215 <- read.dbf ("/home/sgr/Dropbox/Dropbox1/art br-mx/base mx/COE1T215.dbf")
coe2t215 <- read.dbf ("/home/sgr/Dropbox/Dropbox1/art br-mx/base mx/COE2T215.dbf")
folio <- paste (sdem215$CD_A, sdem215$ENT, sdem215$CON, sdem215$V_SEL, sdem215$N_HOG, 
                sdem215$H_MUD, sdem215$N_REN)
x<-data.frame (sdem215, folio)
folio<- paste(coe1t215$CD_A, coe1t215$ENT, coe1t215$CON, coe1t215$V_SEL, coe1t215$N_HOG, 
              coe1t215$H_MUD, coe1t215$N_REN)
x1<-data.frame(coe1t215, folio)
folio<- paste(coe2t215$CD_A, coe2t215$ENT, coe2t215$CON, coe2t215$V_SEL, coe2t215$N_HOG, 
              coe2t215$H_MUD, coe2t215$N_REN)
x2<-data.frame(coe2t215, folio)
total <- merge(x,x1,by="folio")
enoetotal <-merge(total, x2, by="folio")
rm(coe1t215, coe2t215, sdem215, total, x, x1, x2, folio)

################################################   DATA FRAME #######33
#5. Uso data frame con la ENOE.
#En R el uso de data frame es la creación de un marco de datos
#Uno de los usos más frecuentes es la generación de "sub-bases"
#Es decir por ejemplo si vamos a trabajar con la ENOE y ya ubicamos las variables
#que vamos a utilizar, entonces el data frame o marco de datos
#nos permite crear una nueva base con las variables que se van a usar 
#Por ejemplo si vamos a conocer las condiciones de los trabajadores podriamos
#hacer una base solo con nuestras variables de interes..
#En este caso vamos a utilizar las siguientes variables: 
#1) condición de acceso a la seguridad social (SEG_SOC), 2) tipo de contrato (TIP_CON), 
#3) tamaño de la empresa (EMPLE7C) y 4) duración de la jornada (DUR9C), 
#5) sexo (SEX), 6) edad (EDA), 7) Factor de expansión (FAC)
condiciones <- data.frame (enoetotal$SEG_SOC, enoetotal$TIP_CON, 
                           enoetotal$EMPLE7C, enoetotal$SEG_SOC, enoetotal$DUR9C,
                           enoetotal$SEX, enoetotal$EDA, enoetotal$FAC)
## Otro ejemplo sería si vamos a calcular algunos aspectos de los ocupados, por lo que
#creamos una base con data frame con todas las preguntaS 1 de la ENOE, sexo y edad
desempleo <- data.frame (enoetotal$P1, enoetotal$P1A1, enoetotal$P1A2, enoetotal$P1A3,
                         enoetotal$P1B, enoetotal$P1C, enoetotal$P1D, enoetotal$P1E,
                         enoetotal$SEX, enoetotal$EDA)
################# indicadores seleccionados #####################
#############seleccionamos los cuestionarios terminados#####
enoetotal$R_DEF.x<- as.numeric (as.character (enoetotal$R_DEF.x)) ### la variable se convierte en numerico
enoetotal$C_RES<- as.numeric (as.character (enoetotal$C_RES)) ### la variable se convierte en numerico
enoetotal1 <- subset (enoetotal, (enoetotal$R_DEF.x == 0) & (enoetotal$C_RES== 1 |  enoetotal$C_RES==3))
########################### seleccionamos poblacion 15 años o mas ############3
enoetotal1$EDA <- as.numeric(as.character(enoetotal1$EDA))
enoetotal1 <- subset (enoetotal1, EDA >= 15 & EDA <= 97) ###
################PEA y PNEA ##########################33
enoetotal1$CLASE1 <- ordered(enoetotal1$CLASE1,levels = c(1,2),labels = c("PEA", "PNEA"))
#Obtiene la variable CLASE1(PEA y PNEA) de enoetotal1, a la cual asigna etiquetas a los 
#niveles: 1=PEA y 2=PNEA
require (questionr)
c1<-wtd.table (enoetotal1$CLASE1, weights=enoetotal1$FAC)
write.csv(c1,file='/home/sgr/Dropbox/Dropbox1/clases r/taller gpo laboral/PEA.csv')
c1
#Obtiene la tabla expandida con las nuevas etiquetas de CLASE1. Emplea la variable FAC (Factor de expansi?n)
#Se asigna a una variable para poderla exportar
##################### PORCENTAJES#######################
tabrama <- wtd.table (enoetotal1$CLASE1, weights=enoetotal1$FAC)
c1.1 <-round((tabrama/margin.table(tabrama))*100, 2)
write.csv(c1.1,file='/home/sgr/Dropbox/Dropbox1/clases r/taller gpo laboral/PEA1.csv')
c1.1
#Obtiene las tablas con n?meros relativos
#################OCUPADOS y No ocupados#####################
enoetotal1$CLASE2 <- ordered(enoetotal1$CLASE2,levels = c(1,2,3,4),labels = c("Ocupados", "Desocupados", "Disponible", "No disponible"  ))
#Obtiene la variable CLASE2(Poblaci?n ocupada y no ocupada) a la cual asigna etiquetas a los niveles
c2<-wtd.table (enoetotal1$CLASE2, weights=enoetotal1$FAC)
write.csv(c2,file='/home/sgr/Dropbox/Dropbox1/clases r/taller gpo laboral/Ocupados.csv')
c2
##################### PORCENTAJES#######################
tabclase <- wtd.table (enoetotal1$CLASE2, weights=enoetotal1$FAC)
c2.2<-round((tabclase/margin.table(tabclase))*100, 2)
write.csv(c2.2,file='/home/sgr/Dropbox/Dropbox1/clases r/taller gpo laboral/Ocupados1.csv')
c2.2
#################### Posicion en la ocupaci?n##################
enoetotal1$POS_OCU <- ordered(enoetotal1$POS_OCU,levels = c(1,2,3,4, 5),labels = c("Subornidados", "Empleadores", "TCP", "Sin pago", "N.E."  ))
c3<-wtd.table (enoetotal1$POS_OCU, weights=enoetotal1$FAC.y)
write.csv(c3,file='/home/sgr/Dropbox/Dropbox1/clases r/taller gpo laboral/Posicion.csv')
c3
##################### PORCENTAJES#######################
tabpos <- wtd.table (enoetotal1$POS_OCU, weights=enoetotal1$FAC)
c3.3<-round((tabpos/margin.table(tabpos))*100, 2)
write.csv(c3.3,file='/home/sgr/Dropbox/Dropbox1/clases r/taller gpo laboral/Posicio1.csv')
c3.3
####### Selecciono unicamente a los remunerados#####################
enoetotal1$POS_OCU <- as.numeric(as.character(enoetotal1$POS_OCU))
remun <- subset (enoetotal1, enoetotal1$POS_OCU == 1 )
################## sectores de actividad económica#######
table (enoetotal1$RAMA_EST1)
enoetotal1$RAMA_EST1 <- ordered(enoetotal1$RAMA_EST1,levels = c(0,1,2,3,4),labels = c("NA", "Primario", "Secundario", "Terciario", "N.E."))
c4<-wtd.table (remun$RAMA_EST1, weights=remun$FAC)
c4
write.csv(c4,file='/home/sgr/Dropbox/Dropbox1/clases r/taller gpo laboral/SecAE.csv')
##################### PORCENTAJES#######################
tabrama1 <- wtd.table (remun$RAMA_EST1, weights=remun$FAC)
c4.4<- round((tabrama1/margin.table(tabrama1))*100, 2)
c4.4
write.csv(c4,file='/home/sgr/Dropbox/Dropbox1/clases r/taller gpo laboral/SecAE1.csv')


#table (RAMA_EST1)
#remun$RAMA_EST1 <- ordered(remun$RAMA_EST1,levels = c(0,1,2,3,4),labels = c("Primario", "Secundario", "Terciario", "N.E."))
#c4<-wtd.table (remun$RAMA_EST1, weights=remun$FAC)
#c4
#write.csv(c4,file='/home/sgr/Dropbox/Dropbox1/clases r/taller gpo laboral/SecAE.csv')
##################### PORCENTAJES#######################
#tabrama1 <- wtd.table (remun$RAMA_EST1, weights=remun$FAC)
#c4.4<- round((tabrama1/margin.table(tabrama1))*100, 2)
#c4.4
#write.csv(c4,file='/home/sgr/Dropbox/Dropbox1/clases r/taller gpo laboral/SecAE1.csv')

##################### CONDICIÓN DE ACCESO A LAS INSTITUCIONES DE SALUD###############
table (remun$IMSSISSSTE)
remun$IMSSISSSTE <- ordered(remun$IMSSISSSTE,levels = c(1,2,3, 4, 5),labels = c("IMSS", "ISSSTE", "OTRA", "NO RECIBE", "NE"))
c5<-wtd.table (remun$IMSSISSSTE, weights=remun$FAC)
c5
write.csv(c5,file='/home/sgr/Dropbox/Dropbox1/clases r/taller gpo laboral/ISalud.csv')
##################### PORCENTAJES#######################
tabseg <- wtd.table (remun$IMSSISSSTE, weights=remun$FAC)
tabseg
c5.5<-round((tabseg/margin.table(tabseg))*100, 2)
c5.5
write.csv(c5.5,file='/home/sgr/Dropbox/Dropbox1/clases r/taller gpo laboral/ISalud1.csv')
################### DESOCUPADOS #######################
################## Actual medición##################################
desocupa <-  subset (enoetotal1, enoetotal1$CLASE2 == 2)
#####################################  DESOCUPA Y GRUPOS DE EDAD###########3
desocupa$EDA5C <- ordered(desocupa$EDA5C,levels = c(1,2,3,4, 5),labels = c("15 a 24", "25 a 44", "45 a 64", "65 o mas", "N.E."))
c6<-wtd.table (desocupa$EDA5C, weights=desocupa$FAC)
c6
##################  porcentaje  ###############
tabeda <- wtd.table (desocupa$EDA5C, weights=desocupa$FAC)
c6.6 <- round((tabeda/margin.table(tabeda))*100, 2)
c6.6
##############################DESOCUPA Y ESCOLARIDAD ##########################
desocupa$NIV_INS <- ordered(desocupa$NIV_INS,levels = c(1,2,3,4, 5),labels = c("Primaria incompleta", "Primaria completa", "Secundaria completa", "Medio superior y superior", "N.E."))
c7<-wtd.table (desocupa$NIV_INS, weights=desocupa$FAC)
c7
##################  porcentaje  ###############
tabesc <- wtd.table (desocupa$NIV_INS, weights=desocupa$FAC)
c7.7 <- round((tabesc/margin.table(tabesc))*100, 2)
c7.7
#######################################EXPORTAR#####################################333

#Exporta cada DataFrame en archivos diferentes
write.csv(c1,file='PEA.csv')
write.csv(c2,file='Ocupados.csv')
write.csv(c3,file='Posicion.csv')
write.csv(c4,file='SecAE.csv')
write.csv(c5,file='ISalud.csv')
write.csv(c6,file='Desocupados y grupos de edad.csv')
write.csv(c7,file='Desocupados y escolaridad.csv')


#Exportar cada DataFrame en diferentes hojas de un archivo excel
install.packages("xlsx")
install.packages("rJava")
library(xlsx)

write.xlsx(c1, file="filename.xlsx", sheetName="sheet1")
write.xlsx(c2, file="filename.xlsx", sheetName="sheet2", append=TRUE)
write.xlsx(c3, file="filename.xlsx", sheetName="sheet3", append=TRUE)
write.xlsx(c4, file="filename.xlsx", sheetName="sheet4", append=TRUE)
write.xlsx(c5, file="filename.xlsx", sheetName="sheet5", append=TRUE)
write.xlsx(c6, file="filename.xlsx", sheetName="sheet6", append=TRUE)
write.xlsx(c7, file="filename.xlsx", sheetName="sheet7", append=TRUE)

############### NUEVA MEDICION######################





