#Instalar RTools
#https://cran.r-project.org/bin/windows/Rtools/

install.packages(c("foreign","devtools"))
library(devtools)

install_github("jcms2665/joinENOE")
library(foreign)
library(joinENOE)

#Poner la ruta con la diagonal invertida
setwd("C:/JC/Package/2/Bases")

#Cargar las Bases de datos
SDEMT215<-data.frame(read.dbf("SDEMT215.dbf"))
COE1T215<-data.frame(read.dbf("COE1T215.dbf"))

#Ejecutar este comando y ver si corre
jcENOE(SDEMT215,COE1T215,"P2_3")