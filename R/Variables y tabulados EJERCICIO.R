


#7. EJERCICIO

####################################################################################################

#Crear una base en donde solo estan las mujeres trabajadoras residentes en el DF
#Hacer un tabulado para saber cuantas de ellas tienen IMSS

####################################################################################################

#Paso 0. Identificar las bases que necesita y las variables
#SDEMT215: SEX (sexo), ENT (entidad)
#COE2T215: P6D (acceso a seguridad social)

#Paso 1. Defir llave
          llave<-c("CD_A","ENT","CON","V_SEL","N_HOG","N_REN")

#Paso 2. Identificar las variables que nos interesa
#        Sugerencia: Crea un vector que una llave + P6D y nombralo "w"
          w<-c(llave,"P6D")

          
#Paso 3. Unir las bases de datos SDEMT215 y COE2T215
#         OJO: No queremos pegar toda la base, solo queremos anexar la variable P7 a la base SDEMT215 
#         Sugerencia: Crea una nueva base que se llame "SDEMT215_2" y usa el vector "w" que ya creaste
          SDEMT215_2 <- merge(SDEMT215,COE2T215[w],by=llave, all.x=TRUE)

#Paso 4. Seleccionar los casos que nos interesan
#        Sugerencia: Ahora que en la base SDEMT215_2 ya esta la variable "P7", solo nos interesa seleccionar a las mujeres que viven en el DF
#        OJO: Usa la base que ya creaste
          
        MujDF <-SDEMT215_2[which(SDEMT215_2$ENT=='09'& SDEMT215_2$SEX=="2"), ]

#Paso 5. Recodificar la variable que recién agregamos (P6D)
#         Sugerencia: Considera los códigos (levels) y las etiquetas (labels):
#                     Para las etiquetas usa el vector: labels = c("IMSS", "Pemex","ISSSTE", "ISSSTE_Est","Otra","No recibe","NS")
#                     Para los niveles considera el vector: levels = c(1,2,3,4,5,6,9) 
        
        MujDF$P6D<- factor(MujDF$P6D,levels = c(1,2,3,4,5,6,9),labels = c("IMSS", "Pemex","ISSSTE", "ISSSTE_Est","Otra","No recibe","NS"))
        
#Paso 6. Por último, tabula P6D x POS_OCU con y sin ponderar
        
        wtd.table(MujDF$P6D,MujDF$POS_OCU) 
        wtd.table(MujDF$P6D,MujDF$POS_OCU,weights=MujDF$FAC)
