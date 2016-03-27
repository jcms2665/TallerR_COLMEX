
#=======================================

# Introducción a R

# El objetivo de este módulo es proporcionar una breve al entorno de R
# y sus características más básicas.



#0. Limpiar el espacio de trabajo
#1. Librerías
#2. Directorio de trabajo
#3. R como calculadora
#4. Tipos de datos
#5. Conversión de datos
#6. Vectores
#7. Listas
#8. Matrices
#9. Missing values
#10. Data Frame




#=======================================


###LIMPIAR ESPACIO DE TRABAJO
ls()           # Lista de objetos actuales
rm(list=ls())  # Borrar objetos actuales
ls() 
gc()           # Garbage collection, reporta memoria en uso


### LIBRERIAS
search()       # Busca librer??as instaladas

library(base)     # Funciones b??sicas (aritm??ticas, input/output, basic programming support,...)
library(gdata)    # Leer y escribir Microsoft files
#install.packages("foreign", repos="http://stat.ethz.ch/CRAN/", dependencies = TRUE)
library(foreign)  # Leer y escribir STATA files
library(stats)    # Estad??sticas b??sicas
#install.packages("descr", repos="http://stat.ethz.ch/CRAN/", dependencies = TRUE)
library(prettyR)  # Frecuencias

update.packages() # Actualizar librer??as instalados


### DIRECTORIO DE TRABAJO
getwd()           # Directorio actual
setwd("/Users/Nina/Documents/Nina/Grupo Mercados de Trabajo/") # Cambio de directorio

list.files()      # Lista de archivos




### R COMO CALCULADORA, RESULTADO INMEDIATO
1+1 # Suma dos d??gitos
5*7 # 5*7

### DATA TYPES
c('a','b','c')  # Caracter
1:7             # Entero
40<80           # Valor logico
2+2 == 5        # Valor logico
T == TRUE       # T expresion corta de verdadero

x <- 24         # Asignaci??n de valor 24 a la variable x para su uso posterior (OBJETO)
x/2             # Uso posterior de variable u objeto x
x               # Imprime en pantalla el valor de la variable u objeto
x <- TRUE       # Asigna el valor l??gico TRUE a la variable x OJO: x toma el ??ltimo valor que se le asigna
x
sum (10,20,30)    # Funci??n suma
rep('R', times=3) # Repite la letra R el n??mero de veces que se indica
sqrt(9)           # Ra??z cuadrada de 9

###HELP
help(sum)         # Ayuda sobre funci??n sum
example(min)      # Ejemplo de funci??n min

### VECTORES
y <- c(2,4,6)     # Vector num??rico
y <- c('Primaria', 'Secundaria') # Vector caracteres
1:5               # Secuencia 1-5
seq(1, 10, 0.5)   # Secuencia con incrementos diferentes a 1
y[2]              # Acceder al segundo valor del vector y
y[3] <- 'Preparatoria y m??s' # Asgina valor a la tercera componente del vector
sex <-1:2         # Asigna a la variable sex los valores 1 y 2
names(sex) <- c("Femenino", "Masculino") # Asigna nombres al vector de elementos sexo
sex[2]            # Segundo elemento del vector sex
z <- c(0, y, 5)   # Concatena escalares y vectores
z
w <- vector('numeric', length=10) # Funci??n vector
class(w)

# FUNCI??N AS
as.numeric(c('-.1','2.7','B')) # Funci??n as.*

#LISTAS
xx <- list (1, 'a', TRUE, 1+4i) # Lista (vector con elementos de diferentes clases)
xx

###MATRICES
m <- matrix (nrow=2, ncol=3, 1:6) # Matrices Ejemplo 1
m
dim(m)
attributes(m)
n <- 1:6     # Matrices Ejemplo 2
dim(n) <- c(2,3)
n
xx <-10:12   # Matrices Ejemplo 3
yy<-14:16
cbind(xx,yy) # Une vectores por Columnas
rbind(xx,yy) # Une vectores por Renglones

### FACTORES
x <- factor ( c('si', 'si', 'no')) # Factores
x
table(x)     # Frecuencia
unclass(x)   # Clase
x <- factor ( c('si', 'si', 'no'), levels=c('no', 'si'))
x

### MISSING VALUES
y <- c(1, 2, NA, 10, 3) # Missing values
is.na(y)    # Es missimg?

### DATA FRAMES
x <- data.frame(id=1:4, sex=c('F', 'F', 'M', 'M')) # Data Frames
x
nrow(x)    # N??mero de renglones
ncol(x)    # N??mero de columnas

### ETIQUETAS
x <- 1:2   # Etiquetas para cualquier objeto en R
names(x)
names(x) <- c("Ocupado", "Desocupado")
names(x)

#############################
####SCRIPT PEGADO DE BASES ENOE ######
#############################

#library (foreign)

# DIRECTORIO DE TRABAJO
getwd() # Directorio actual
setwd("/Users/Nina/Documents/Nina/Grupo Mercados de Trabajo/Materiales reuni??n de dicusi??n PEA. 130415/ENOE_2_15") # Cambio de directorio

# LEER DBF FILES (ENOE)
sdem215 <- read.dbf ("/Users/Nina/Documents/Nina/Grupo Mercados de Trabajo/Materiales reuni??n de dicusi??n PEA. 130415/ENOE_2_15/SDEMT215.dbf")
coe1t215 <- read.dbf ("/Users/Nina/Documents/Nina/Grupo Mercados de Trabajo/Materiales reuni??n de dicusi??n PEA. 130415/ENOE_2_15/COE1T215.dbf")
coe2t215 <- read.dbf ("/Users/Nina/Documents/Nina/Grupo Mercados de Trabajo/Materiales reuni??n de dicusi??n PEA. 130415/ENOE_2_15/COE2T215.dbf")

# CREAR UN OBJETO folio CON LAS VARIABLES:
folio <- paste (sdem215$CD_A, sdem215$ENT, sdem215$CON, sdem215$V_SEL, sdem215$N_HOG, 
                sdem215$H_MUD, sdem215$N_REN)

# CREAR UN DATA FRAME x CON LAS VARIABLES EN sdem215 Y folio
x<-data.frame (sdem215, folio)

# CREAR UN OBJETO folio CON LAS VARIABLES:
folio<- paste(coe1t215$CD_A, coe1t215$ENT, coe1t215$CON, coe1t215$V_SEL, coe1t215$N_HOG, 
              coe1t215$H_MUD, coe1t215$N_REN)

# CREAR UN DATA FRAME x1 CON LAS VARIABLES EN coe1t215 Y folio
x1<-data.frame(coe1t215, folio)

# CREAR UN OBJETO folio CON LAS VARIABLES:
folio<- paste(coe2t215$CD_A, coe2t215$ENT, coe2t215$CON, coe2t215$V_SEL, coe2t215$N_HOG, 
              coe2t215$H_MUD, coe2t215$N_REN)

# CREAR UN DATA FRAME x1 CON LAS VARIABLES EN coe1t215 Y folio
x2<-data.frame(coe2t215, folio)

# JUNTAR EL CONTENIDO DE LOS OBJETOS x Y x1 POR LA VARIABLE folio
total <- merge(x,x1,by="folio")

# JUNTAR EL CONTENIDO DE LOS OBJETOS total Y x2 POR LA VARIABLE folio
EnoeTotal <-merge(total, x2, by="folio")
