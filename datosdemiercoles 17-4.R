##'Cargamos las librerias necesarias
library(readr)
library(ggplot2)
library(dplyr)
##'Cargamos la base de datos
cambio_lealtades <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/cambio_lealtades.csv")

##'Exploramos rapidamente el data set

head(cambio_lealtades)
summary(cambio_lealtades)
glimpse(cambio_lealtades)
unique(cambio_lealtades$fin_t1)

##'Observamos cuantos personajes muertos hay en la primera temporada y en la ultima
sum(cambio_lealtades$fin_t1=="Muerta/o")
sum(cambio_lealtades$fin_t7=="Muerta/o")

##'Creamos un for loop para construir la variable "Muertes_por_temporada", sabiendo que se van acumulando. Es decir, si un personaje muere en la temporada 3 en las siguentes tambien va a aparecer como Muerta/o
muertes_por_temporada = c()
for(i in c(4:10)) {
  muertos <- sum(cambio_lealtades[,i]=="Muerta/o") - sum(muertes_por_temporada)
  muertes_por_temporada <- append(muertes_por_temporada, muertos)
}
  
##'Creamos la variable "Numero_de_temporada" tambien con un for loop
numero_de_temporada <- c()
for (i in c(1:7)) {
  temporada <- paste("Temporada", i)
  numero_de_temporada <- append(numero_de_temporada, temporada)
}


##'Combinamos ambas variables en un nuevo data frame
Temporadas_muertes <- as.data.frame(cbind(numero_de_temporada,as.factor(muertes_por_temporada)))


##'Generamos el grafico
ggplot(Temporadas_muertes, aes(numero_de_temporada, muertes_por_temporada))+
  geom_col(aes(fill=numero_de_temporada))+
  scale_y_continuous(breaks =c(15,30,45,60,75))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title ="Muertes por Temporada", y = "Cantidad de Muertes", x = "Temporadas") 

  