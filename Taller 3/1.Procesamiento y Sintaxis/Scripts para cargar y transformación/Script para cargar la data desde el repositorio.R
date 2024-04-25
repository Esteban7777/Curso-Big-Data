#Cargar la data 
library(tidyverse)
library(sf)

train<-read.csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/train.csv")

test<-read.csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/test.csv")

MGN<-st_read("C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/0.Insumos/MGN/MGN_URB_MANZANA.shp")

#Filtramos para Bogotá y para manzanas urbanas

MGN<-MGN %>% filter(mpio_cdpmp=="11001" & clas_ccdgo=="1")

manzanas<-ggplot() + geom_sf(data = MGN, fill="darkseagreen")+
  labs(
    title="Manzanas de Bogotá MGN DANE")

#Se obtienen los estratos en el servicio de energía de las viviendas
#Fuente CNPV DANE 2018: http://systema59.dane.gov.co/bincol/RpWebEngine.exe/Portal?BASE=CNPVBASE4V2&lang=esp

