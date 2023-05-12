# Universidad Autónoma de Aguascalientes
# Alumna:Laura Yazmin de la Fuente Bernal
# Profesor: Francisco Javier Luna Rosas
# Fecha: 09/05/2023
# ACP_WebMing

#install.packages("maps")
#install.packages("rvest")
#install.packages("FactoMineR")
#install.packages("stringr")

library(maps)
library(rvest)
library(FactoMineR)
library(stringr)

#Se lee la información de la página de wikipedia y se extrae la tabla
heritage_parsed <-read_html("https://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger")
tables <- html_table(heritage_parsed, fill= TRUE)
tables

danger_table <-tables[[2]]
names(danger_table)

#las columnas a utilizar son la 1,3,4,6 y 7 (nombre, ubicación, criterio, año creación, año de peligro de extinción)
danger_table <- danger_table[,c(1,3,4,6,7)]
dim(danger_table)
names(danger_table)

#reenombras las columnas de la tabla
colnames(danger_table) <-c("nombre","locn","crit","y_inicio","y_peligro")
danger_table$nombre[1:3]

#limpieza de datos de la columna "criterio"
danger_table$crit<-ifelse(str_detect(danger_table$crit, "Natural")==T, "nat", "cult")
danger_table$crit[1:10]

#limpieza de datos de la columna "año"
danger_table$y_inicio <- as.numeric(danger_table$y_inicio)
danger_table$y_inicio[1:5]
length(danger_table$y_inicio)

#limpieza de datos de la columna "año peligro de extinción"
danger_table$y_peligro
danger_table$y_peligro[20]<-"1993-"
danger_table$y_peligro[22]<-"1984-"
danger_table$y_peligro[42]<-"1996-"
danger_table$y_peligro

for(i in 1:length(danger_table$y_peligro))
  danger_table$y_peligro[i]<-substr(danger_table$y_peligro[i],1,4)
danger_table$y_peligro <- as.numeric(danger_table$y_peligro)
danger_table$y_peligro

# Obtención de coordenadas
reg_y <- "[/][ -]*[[:digit:]]*[.]*[[:digit:]]*[;]"
reg_x <- "[;][ -]*[[:digit:]]*[.]*[[:digit:]]*"
y_coords <- str_extract(danger_table$locn, reg_y)
(y_coords <- as.numeric(str_sub(y_coords, 3, -2)))

danger_table$y_coords <- y_coords
x_coords <- str_extract(danger_table$locn, reg_x)
(x_coords <- as.numeric(str_sub(x_coords, 3, -1)))

danger_table$x_coords <- x_coords
danger_table$locn <- NULL

head(danger_table)
danger_table

#Gráfico de lugares en peligro de extinción
par(oma=c(0,0,0,0))
par(mar=c(0,0,0,0))
pch<- ifelse(danger_table$crit=="nat", 19, 2)
map("world", col ="darkgrey", lwd = 0.5, mar = c(0.1, 0.1, 0.1, 0.1))
title("Patrimonio de la humanidad en peligro \n")
points(danger_table$x_coords, danger_table$y_coords, pch = pch)
box()

#PCA con y_inicio, y_peligro, y_coords, x_coords
danger_table_acp <-danger_table[,c(3,4,5,6)]
dim(danger_table_acp)
names(danger_table_acp)
danger_table_acp
res<-PCA(danger_table_acp, scale.unit=TRUE, ncp=5, graph = FALSE)
plot (res, axes=c(1, 2), choix="ind", col.ind="red",new.plot=TRUE)
plot (res, axes=c(1, 2), choix="var", col.ind="blue",new.plot=TRUE)

plot (res, axes=c(1, 2), choix="ind", col.ind="red",new.plot=TRUE)

