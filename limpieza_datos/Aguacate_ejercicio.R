#####Ejercicio de limpieza de datos con la base de datos de aguacate########

#Escoger la dirección donde estarás trabajando#
#1. Forma convencional medinate barra 
#2. Mediante el siguiente comando#
setwd("~/R_sites/R-ladies/limpieza_datos")

#Instalación de los paquetes####
#install.packages("dplyr")
#install.packages("reshape2")

#Si ya los tenías instalados, solo llamarlos al ambiente#
library(reshape2)
library(dplyr)

#A)  Importar informacion en R
aguacate <- read.csv("avocado.csv")

# Observar el contenido de nuestro dataframe
head(aguacate)


# B) Renombrar columnas
# Cambiar la columna "name" por "Name"
colnames(aguacate)[colnames(aguacate) == "year"] <- "Year"
colnames(aguacate)[colnames(aguacate) == "X4046"] <- "C4046"
colnames(aguacate)[colnames(aguacate) == "X4225"] <- "C4225"
colnames(aguacate)[colnames(aguacate) == "X4770"] <- "C4770"
colnames(aguacate)[colnames(aguacate) == "type"] <- "Type"
colnames(aguacate)[colnames(aguacate) == "region"] <- "Region"


#Comprobar el cambio del nombre
head(aguacate)

#C) Seleccionar columnas de interes

aguacate <- aguacate %>% dplyr::select(AveragePrice, Total.Volume, C4046, C4225, C4225, C4770, Type, Year, Region) 
# Columnas AveragePrice (Precio promedio aguacate), Total.Volume (volumen total), C4046, C4225 y C4770 (Producción por calibre aguacate), Year (año), Region (region de produccion)

head(aguacate)

#EXTRA POR PROBLEMA CON LA LIBRERIA
remove.packages("rlang")  #Tuve que hacer esto porque me daba error al seleccionar las columnas de interés
install.packages("rlang")
library(rlang)
library(dplyr)

#D) PREGUNTAS


#Queremos saber de de los dos tipos de aguacate cuantos de los registros son orgánicos y cuales convencional
#Elegimos de la columna type solamente los que dicen conventional y organic
aguaType <- aguacate[(aguacate$Type == "conventional" | aguacate$Type == "organic"), ]
head(aguaType)

table(aguacate$Type)

#Emiminar todos los  TotalUS en Región
attach(aguacate)
library(tidyverse)
#View(dataframe)

ha <- aguacate %>% filter(!(Region == "TotalUS"))
View(ha)


#Cuánta producción hay por tipo aguacate?

#OPCIÓN A
abc <- ha %>% group_by(Type) 
abc
fff <- abc %>% summarise(sum(Total.Volume)) 
fff

#OPCIÓN B
ha %>% group_by(Type) %>%
  summarise(sum(Total.Volume))



#Cuanta producción hay por el tipo de aguacate pero por región y por año?

#OPCIÓN A
def <- ha %>% group_by(Type, Region, Year)
ggg <- def %>% summarise(sum(Total.Volume))
ggg
ddd<- ggg %>% arrange(-`sum(Total.Volume)`) #Quiero que lo ordene
View(ddd)

#OPCIÓN B
ha %>% group_by(Type, Region, Year) %>%
  summarise(sum(Total.Volume)) %>% arrange(-`sum(Total.Volume)`)



#En qué año fue la mayor producción de aguacate y de que tipo?

#OPCIÓN A
tuv <- ha %>% group_by(Type, Year)
www <- tuv %>% summarise(sum(Total.Volume))
www
xxx<- www %>% arrange(-`sum(Total.Volume)`) 
View(xxx)

#OPCIÓN B
ha %>% group_by(Type, Year) %>% 
  summarise(sum(Total.Volume)) %>% 
  arrange(-`sum(Total.Volume)`)

#En qué año hubó más producción de aguacate

#OPCIÓN A
wxy <- ha %>% group_by(Year)
zzz <- wxy %>% summarise(sum(Total.Volume))
zzz
aaa<- zzz %>% arrange(-`sum(Total.Volume)`) 
View(aaa)

#OPCIÓN B
ha %>% group_by(Year) %>% 
  summarise(sum(Total.Volume)) %>% 
  arrange(-`sum(Total.Volume)`)



# Según el calibre del aguacate ¿en que año se produce más?

#OPCIÓN A
bcd <- ha %>% group_by(Year)
eee <- bcd %>% summarise(sum(C4046), sum(C4225), sum(C4770))
eee

#OPCIÓN B
ha %>% group_by(Year) %>% 
  summarise(sum(C4046), sum(C4225), sum(C4770)) %>%
  arrange(-`sum(C4046)`, - `sum(C4225)`,- `sum(C4770)`)




# Según el calibre del aguacate en que región se produce más?
#Calibre 4046
efg <- ha %>% group_by(Year, Region)
hij <- efg %>% summarise(sum(C4046))
hij
ygh<- hij %>% arrange(-`sum(C4046)`)
ygh


#Calibre 4225
jhu <- ha %>% group_by(Year, Region)
juk <- jhu %>% summarise(sum(C4225))
jkl<- juk %>% arrange(-`sum(C4225)`)
jkl

#Calibre 4770
hkn <- ha %>% group_by(Year, Region)
kij <- hkn %>% summarise(sum(C4770))
hil<- kij %>% arrange(-`sum(C4770)`)
hil
#Opción B
ha %>% group_by(Type, Year) %>% 
  summarise(sum(C4046), sum(C4225), sum(C4770)) %>% 
  arrange(-`sum(C4046)`, - `sum(C4225)`,- `sum(C4770)`)




#if_else
colnames(ha)
mean(Total.Volume)


ha_edit <- ha %>% mutate(Tipo_exportador = if_else( Total.Volume>=850644, "Gran exportador",
                                                    "Pequeño exportador"))
View(ha_edit)       








































