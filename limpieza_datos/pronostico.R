#install.packages("dplyr")
library(dplyr)

setwd("~/R_sites/R-ladies/limpieza_datos")
Datos <- read.csv("Ejercicio_pronostico.csv", stringsAsFactors = TRUE)
head(Datos)
dim(Datos)
str(Datos)


#renómbrame las columnas
colnames(Datos) <- c("Vendedor", "Sector", "Mat_registrado", "Prom_3M", "Estadistica", "Tipo_sum")
colnames(Datos)[3] <- "Producto"


#dame el nombre de los vendedores que hay
unique(Datos$Vendedor)
#dime que productos se vendieron
unique(Datos$Producto)


#dame el resumen (#rows) de los vendedores
summary(Datos$Vendedor) #Opción A
table(Datos$Vendedor) #Opción B
Datos %>% filter(Vendedor == "Samuel") %>% nrow() #Opción C si quiero el dato de alguien en específico

#¿Quien es el empleado del mes? (el que movió más dinero...)
Samuel <- Datos[(Datos$Vendedor == "Samuel"),] #Opción A respetando el orden original
Samuel <- subset(Datos, subset = Datos$Vendedor == "Samuel") #Opción B respetando el orden original
Samuel <- Datos %>% filter(Vendedor == "Samuel") #Opción C usando un orden de corrido
sum(Samuel$Prom_3M)

rm(vendedores)
vendedores <- Datos %>%
  group_by(Vendedor) %>%
  mutate(suma = sum(Prom_3M))

JoseL <- subset(Datos, subset = Datos$Vendedor == "Jose Luis") 
sum(JoseL$Prom_3M)

Cristian<- subset(Datos, subset = Datos$Vendedor == "Cristian") #empleado del mes
sum(Cristian$Prom_3M)

Hector <- subset(Datos, subset = Datos$Vendedor == "Hector") 
sum(Hector$Prom_3M)

Daniel <- subset(Datos, subset = Datos$Vendedor == "Daniel") 
sum(Daniel$Prom_3M)


#¿Cuántas "tintas líquidas" vendió el empleado del mes?
unique(Datos$Sector)
#Opción A
Datos %>% select(Vendedor, Sector) %>% filter(Vendedor == "Cristian" & Sector == "Tintas Liquidas") %>% nrow()
# Opcion B
Datos %>% 
      filter(!is.na(Vendedor)) %>%
      group_by(Vendedor) %>%
      dplyr::count(Sector) %>%
      select(Vendedor, Sector, Count = n)%>%
      arrange(-Count)


#¿Cuál es el producto estrella vendido?
Datos %>% 
    filter(!is.na(Producto)) %>%
    group_by(Producto) %>%
    dplyr::count(Producto) %>%
    select(Producto, Count = n) %>%
    arrange(-Count)
  
    
#Sácame índices que requiero
attach(Datos)
Datos$MRP <- ifelse(Estadistica=="6_3"|Estadistica=="5_3"|Estadistica=="4_3"|Estadistica=="3_3", "SI",
             ifelse(Estadistica=="5_1"|Estadistica=="3_1"|Estadistica=="2_0"|Estadistica=="0_0", "NO",
             ifelse(Estadistica=="4_2"|Estadistica=="3_2"|Estadistica=="2_2", "QUIZA", 
             ifelse(Estadistica=="", "NA",
             "aqui_que?"))))

Datos2 <- Datos %>%
  mutate(indice = ifelse(Tipo_sum =="Stock" & MRP == "SI", Prom_3M * 1.5,
                  ifelse(Tipo_sum =="Especial" & MRP=="SI", Prom_3M * 0.8,
                  ifelse(MRP == "NO", "0",
                  "nada"))))

#write.csv(Datos2, "pronostico.csv"

