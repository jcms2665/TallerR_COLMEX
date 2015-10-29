####################################################################################################

# 3. Conocer la ENOE desde R (etiquetar, recodificar y seleccionar casos).
# 4. Tabulados

####################################################################################################


# Todas las bases de datos de la ENOE pueden ser obtenidas en la pagina de internet del INEGI.
# R tiene varios metodos que tienen el mismo resultado, por lo cual se nombrar como M1, M2,...,Mn
# cuando exista mas de un metodo para hacer lo mismo.
# Al final de cada seccion se encuentran algunas ligas que pueden servir como referencia para cada
# uno de los temas

#Para llevar a cabo los ejercicios se utilizaran las siguientes bases:

#COE1T215.dbf
#COE2T215.dbf
#SDEMT115.dbf

####################################################################################################
#0. Consideraciones iniciales

      #0.1  Instalar los paquete necesarios 
      install.packages(c("foreign","devtools","data.table","questionr","survey","base","car"))
      install.packages("dplyr")
      #0.2  Cargar las librerias que se van a utilizar
      library(data.table)
      library(foreign)
      library(devtools)
      library(questionr)
      library(survey)
      library(base)
      #library(car)
      #library(dplyr)

      #0.3  Limpiar el entorno de trabajo
      rm(list = ls())
      
      #Ctrl + L = Limpia la consola  
      
      #0.4  Definir el directorio raiz de las bases de datos
      setwd("C:/JC/Package/2/Bases")
      
      #0.5  Cargar las Bases de datos
      SDEMT215<-data.frame(read.dbf("SDEMT215.dbf"))
      COE1T215<-data.frame(read.dbf("COE1T215.dbf"))
      COE2T215<-data.frame(read.dbf("COE2T215.dbf"))


      
####################################################################################################
#1. Frecuencias

      #1.1  Frecuencias con datos muestrales (sin ponderar)
            #Metodo 1
            wtd.table(SDEMT215$POS_OCU)
            wtd.table(SDEMT215$POS_OCU,na.show = TRUE)
            
            #Metodo 2
            table (SDEMT215$POS_OCU)
            table (SDEMT215$POS_OCU,useNA = "always")
            
            #Metodo 3
            attach(SDEMT215)
            table (POS_OCU)
            table (POS_OCU,useNA = "always")
            detach(SDEMT215) 
      
      #1.2  Frecuencias con datos ponderados
            wtd.table(SDEMT215$POS_OCU,weights=SDEMT215$FAC,na.show = TRUE)

      #1.4 Documentacion          
      #http://www.inside-r.org/packages/cran/questionr/docs/wtd.table


####################################################################################################
#2. Tabulados

      #2.1  Tabulados con datos muestrales (sin ponderar)
            #Metodo 1
            wtd.table(SDEMT215$SEX,SDEMT215$POS_OCU)
            
            #Metodo 2
            xtabs(~ SEX+POS_OCU, data=SDEMT215)
            
            #Metodo 3
            table(SDEMT215$SEX,SDEMT215$POS_OCU)
      
      #2.2  Tabulados con datos ponderados
            wtd.table(SDEMT215$SEX,SDEMT215$POS_OCU,weights=SDEMT215$FAC)
      
      #2.3  Ayuda
            ?wtd.table

      #2.3 Documentacion
            #http://www.statmethods.net/stats/frequencies.html


####################################################################################################
#3. Etiquetar

      #3.1  Etiquetar en otra variable
            SDEMT215$SEXO <- factor(SDEMT215$SEX,levels = c(1,2),labels = c("Hombre", "Mujer"))
            wtd.table(SDEMT215$SEXO)
      
      #3.2  Etiquetar en la misma variable
            SDEMT215$POS_OCU <- factor(SDEMT215$POS_OCU,levels = c(1,2,3,4,5),labels = c("Subordinados y remunerados", "Empleadores","Cuenta propia", "Sin pago","NE"))
            wtd.table(SDEMT215$POS_OCU)
      
      #3.3  Tabulado de las variables etiquetadas
            wtd.table(SDEMT215$POS_OCU, SDEMT215$SEXO, weights=SDEMT215$FAC)
      
      #3.4 Documentacion
            #https://stat.ethz.ch/R-manual/R-devel/library/base/html/factor.html

####################################################################################################
#4. Recodificar 
      
      #4.1  Recodificion de variables numericas (rangos)
     
            #Metodo 1
            attach(SDEMT215)
            SDEMT215$EDAD11[as.numeric(EDA) >= 0 & as.numeric(EDA)<=10] <- 1
            SDEMT215$EDAD11[as.numeric(EDA) >= 11 & as.numeric(EDA)<=20] <- 2
            SDEMT215$EDAD11[as.numeric(EDA) >= 21 & as.numeric(EDA)<=30] <- 3
            SDEMT215$EDAD11[as.numeric(EDA) >= 31 & as.numeric(EDA)<=40] <- 4
            SDEMT215$EDAD11[as.numeric(EDA) >= 41] <- 5          
            detach(SDEMT215) 
            
            #Metodo3
            attach(SDEMT215)
            SDEMT215$EDAD111 <- ifelse(as.numeric(EDA) >= 0 & as.numeric(EDA)<=10,1,
                                       ifelse(as.numeric(EDA) >= 11 & as.numeric(EDA)<=20,2,
                                              ifelse(as.numeric(EDA) >= 21 & as.numeric(EDA)<=30,3,
                                                     ifelse(as.numeric(EDA) >= 31 & as.numeric(EDA)<=40,4,5))))
            
            detach(SDEMT215) 


            #4.1.1 Comparacion de los tres metodos
            
                  wtd.table(SDEMT215$EDAD11)
                  wtd.table(SDEMT215$EDAD111)
            
            
      #4.1  Recodificion de variables tipo caracter
            
                  attach(SDEMT215)
                  SDEMT215$VALIDO[as.character(R_DEF)!="00"] <- "Entrevista completa"
                  SDEMT215$VALIDO[as.character(R_DEF)=="00"] <- "Entrevista incompleta"
                  detach(SDEMT215)
                  
                  wtd.table(SDEMT215$VALIDO)
                 
                  
      #4.1  Documentacion
            #https://stat.ethz.ch/R-manual/R-devel/library/base/html/ifelse.html
            #http://www.statmethods.net/management/variables.html




####################################################################################################
#5. Subconjunto de datos

      #5.1  Seleccionando variables de interes
            #Metodo 1
            var<-c("R_DEF", "LOC", "MUN","C_RES")
            newdata1 <- SDEMT215[,var]
            
            #Metodo 2
            var0 <- names(SDEMT215) %in% c("R_DEF", "LOC", "MUN","C_RES") 
            newdata1_1 <- SDEMT215[,var0]

      #5.2  Excluyendo variables  
            newdata2 <- SDEMT215[,!var0]
            
      #5.3 Seleccionar casos          
            #Funciona?
            newdata3 <- SDEMT215[ which(SDEMT215$SEXO=='Hombre'& SDEMT215$EDA <18), ]
      
            #(Si corre)
            newdata3 <- SDEMT215[ which(SDEMT215$SEXO=='Hombre'& as.numeric(SDEMT215$EDA) <18), ]

            # Ejemplo de un filtro necesario en la ENOE
            newdata4 <- SDEMT215[ which(as.character(SDEMT215$R_DEF)=='00'& (as.numeric(SDEMT215$C_RES)==1 | as.numeric(SDEMT215$C_RES)==3)), ]
      
      #5.4  Seleccionar variables y casos
      
            #Ejemplo: De la base SDEMT215 solo considero:
                      #i)    residentes habituales 
                      #ii)   entrevista completa
                      #iii)  solo considero las variables: SEXO, CS_P17, POS_OCU
                      
            
            newdata4 <- SDEMT215[which(as.character(SDEMT215$R_DEF)=='00'& (as.numeric(SDEMT215$C_RES)=="1" | as.numeric(SDEMT215$C_RES)=="3")), c("SEX", "CS_P17", "POS_OCU")]
      
      #5.5 Documentacion
          #http://www.statmethods.net/management/subset.html
      
      #5.6 Documentacion INEGI
          #http://www.inegi.org.mx/est/contenidos/proyectos/encuestas/hogares/regulares/enoe/doc/fd_c_amp_v4.pdf
        

####################################################################################################
#6. Fucionando bases de datos

      #Para este ejemplo se usaro la base COE1T215, variable P5 (Jornada de trabajo) 
      #Conviene limpiar el entorno de trabajo con la instruccion: 
      rm("newdata1","newdata1_1","newdata2","newdata3","newdata4","var","var1","var0")

      #6.1 Fucionando toda la base de datos
      
          #6.1.1 Defir llave
                  llave<-c("CD_A","ENT","CON","V_SEL","N_HOG","N_REN")
          
          #6.1.2 Caso 1. Toma como referencia los registros que estan en ambas bases
                  b_inner <- merge(SDEMT215,COE1T215,by=llave, all=FALSE)
          
          #6.1.3 Caso 2. Toma como referencia los registros que estan en la base SDEMT215
                  b_right <- merge(SDEMT215,COE1T215,by=llave, all.x=TRUE)
          
          #6.1.4 Caso 3. Toma como referencia los registros que estan en la base COE1T215
                  b_left <- merge(SDEMT215,COE1T215,by=llave, all.y=TRUE)
          
          #6.1.5 Documentacion
          #http://aliquote.org/memos/2011/01/07/venn-diagrams-and-sql-joins-in-r
          #http://www.inside-r.org/packages/cran/plyr/docs/join
      
      
      #6.1 Agregando  UNA variable (en este ejemplo P5 que esta en COE1T215)
          #6.1.1. Definir la llave
                  llave<-c("CD_A","ENT","CON","V_SEL","N_HOG","N_REN")
                  
          #6.1.2. Crear una base nueva con las variables de inter?s
                  k<-c(llave,"P5")              
                  new_COE1T215<-COE1T215[k]
                  
          #6.1.3. Unir las bases
                  SDEM_COE <- merge(SDEMT215,new_COE1T215,by=llave, all.x=TRUE)
                  wtd.table(SDEM_COE$P5)    
      
      #6.2 Unir dos variables de bases distintas  
          
            #Ejemplo: (COE1T215 variable P5) y (COE2T215 variable P6B1)  a la base SDEMT215
      
            #6.2.1 Llave
                    llave<-c("CD_A","ENT","CON","V_SEL","N_HOG","N_REN")
            
            #6.2.2 Llave + P5 de la base COE1T215
                    k1<-c(llave,"P5")              
            
            #6.2.3 Llave + P6B1 de la base COE2T215
                    k2<-c(llave,"P6B1")
            
            #6.2.4 Unir SOCIO1 = SDEMT215 + COE1T215(solo la llave y la variable de interés), 
                    
                    SOCIO1 <- merge(SDEMT215,COE1T215[k1],by=llave, all.x=TRUE)
            
            #6.2.4 Unir SOCIO_T = SDEMT1 + COE2T215(solo la llave y la variable de interés)
                    
                    SOCIO_T <- merge(SOCIO1,COE2T215[k2],by=llave, all.x=TRUE)
      
      
      #Con las frecuencias podemos identificar si todas las variables estan en b5
            wtd.table(SOCIO_T$P5)
            wtd.table(SOCIO_T$P6B1)
            wtd.table(SOCIO_T$SEX)


####################################################################################################

            
            #7. EJERCICIO
            
            ####################################################################################################
            
            #Crear una base en donde solo estan las mujeres trabajadoras residentes en el DF
            #Hacer un tabulado para saber cuantas de ellas tienen IMSS
            
            ####################################################################################################
            
            #Paso 0. Identificar las bases que necesita y las variables
            #SDEMT215: SEX (sexo), ENT (entidad)
            #COE2T215: P6D (acceso a seguridad social)
            
            #Paso 1. Defir llave

            
            
            
            #Paso 2. Identificar las variables que nos interesa
            #        Sugerencia: Crea un vector que una llave + P6D y nombralo "w"

            
            
            
            
            #Paso 3. Unir las bases de datos SDEMT215 y COE2T215
            #         OJO: No queremos pegar toda la base, solo queremos anexar la variable P7 a la base SDEMT215 
            #         Sugerencia: Crea una nueva base que se llame "SDEMT215_2" y usa el vector "w" que ya creaste

            
            
            
            
            #Paso 4. Seleccionar los casos que nos interesan
            #        Sugerencia: Ahora que en la base SDEMT215_2 ya esta la variable "P7", solo nos interesa seleccionar a las mujeres que viven en el DF
            #        OJO: Usa la base que ya creaste
            

            
            
            #Paso 5. Recodificar la variable que recién agregamos (P6D)
            #         Sugerencia: Considera los códigos (levels) y las etiquetas (labels):
            #                     Para las etiquetas usa el vector: labels = c("IMSS", "Pemex","ISSSTE", "ISSSTE_Est","Otra","No recibe","NS")
            #                     Para los niveles considera el vector: levels = c(1,2,3,4,5,6,9) 
            

            
            
            #Paso 6. Por último, tabula P6D x POS_OCU con y sin ponderar
            



