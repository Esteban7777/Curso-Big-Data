#Cargamos las librerias
library(tidyverse)
library(sf)
library(readxl)
#Cargar la data 
train<-read.csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/train.csv")

test<-read.csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/test.csv")

MGN<-st_read("C:/Users/HP-Laptop/Downloads/MGN2023_MANZANA/MGN_URB_MANZANA.shp")

#Filtramos para Bogotá y para manzanas urbanas

MGN<-MGN %>% filter(mpio_cdpmp=="11001" & clas_ccdgo=="1")

#Ploteamos el mapa de manzanas de Bogotá que se encuentra en el MGN del DANE
manzanas<-ggplot() + geom_sf(data = MGN, fill="darkseagreen")+
  labs(
    title="Manzanas de Bogotá MGN DANE")

#Se obtienen los estratos en el servicio de energía de las viviendas
#Fuente CNPV DANE 2018: http://systema59.dane.gov.co/bincol/RpWebEngine.exe/Portal?BASE=CNPVBASE4V2&lang=esp

manzanas_censo<-read_xlsx("C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/0.Insumos/Reporte con codigos de manzana.xlsx")

#Se crea la variable estrato predominante de la manzana con el estrato de mayor frecuencia por manzana
manzanas_censo$estrato_predominante <- apply(
  manzanas_censo[, c("Estrato 1", "Estrato 2", "Estrato 3", "Estrato 4", "Estrato 5", "Estrato 6", "Sin Estrato", "No sabe el estrato")], 1, function(row) {
  # Encuentra el índice del valor máximo
  max_idx <- which.max(row)
  # Devuelve el nombre de la columna correspondiente
  return(names(row)[max_idx])
})
#Validamos cuantas manzanas hay por estrato
table(manzanas_censo$estrato_predominante)

#Convertimos test en un elemento sf para hacer el join espacial con el MGN 
test_sf<-st_as_sf(test,coords = c("lon","lat"))
#Ajustamos el test para que comparta el mismo sistema de coordenadas que el MGN
test_sf<-st_set_crs(test_sf,4326)
test_sf<-st_transform(test_sf,4686)


#Ploteamos la ubicación de las viviendas en test
test_map<-ggplot() + geom_sf(data = test_sf)+
  labs(
    title="Viviendas en Test")

#Realizamos el join espacial para obtener el codigo de manzana donde se ubica cada vivienda
test_join<-st_join(test_sf,MGN)

#Convertimos en dataframe y hacemos el join con la información del Censo
test_join<-as.data.frame(test_join)

#Validamos cuantos NA obtuvimos
table(is.na(test_join$manz_ccnct))
#Se obtienen un 30% de viviendas en las que no se logró hacer join con el MGN

#Se debe depurar el marco de manzanas del Censo

#Identificamos las urbanas y las filtramos
manzanas_censo$clase<-substr(manzanas_censo$Código,6,6)
table(manzanas_censo$clase)
manzanas_censo<- manzanas_censo %>% filter(clase==1)

#Extraemos los últimos 8 digitos que corresponen al sector, sección y manzana urbana
manzanas_censo$id_manzana<-substr(manzanas_censo$Código,
                                  nchar(manzanas_censo$Código)-7,
                                  nchar(manzanas_censo$Código))

test_join$id_manzana<-substr(test_join$manz_ccnct,
                             nchar(test_join$manz_ccnct)-7,
                             nchar(test_join$manz_ccnct))

test_join<-test_join %>% left_join(manzanas_censo,by = "id_manzana")

#Hay un 34% de viviendas que no fue posible cruzar su NSE
table(is.na(test_join$estrato_predominante))

######

#Ahora repetimos el proceso para train

#Convertimos train en un elemento sf para hacer el join espacial con el MGN 
train_sf<-st_as_sf(train,coords = c("lon","lat"))
#Ajustamos el test para que comparta el mismo sistema de coordenadas que el MGN
train_sf<-st_set_crs(train_sf,4326)
train_sf<-st_transform(train_sf,4686)


#Ploteamos la ubicación de las viviendas en test
train_map<-ggplot() + geom_sf(data = train_sf)+
  labs(
    title="Viviendas en Train")

#Realizamos el join espacial para obtener el codigo de manzana donde se ubica cada vivienda
train_join<-st_join(train_sf,MGN)

#Convertimos en dataframe y hacemos el join con la información del Censo
train_join<-as.data.frame(train_join)

#Validamos cuantos NA obtuvimos
table(is.na(train_join$manz_ccnct))
#Se obtienen un % de viviendas en las que no se logró hacer join con el MGN

train_join$id_manzana<-substr(train_join$manz_ccnct,
                             nchar(train_join$manz_ccnct)-7,
                             nchar(train_join$manz_ccnct))

train_join<-train_join %>% left_join(manzanas_censo,by = "id_manzana")

#Hay un 34% de viviendas que no fue posible cruzar su NSE
table(is.na(train_join$estrato_predominante))

#Seleccionamos las variables de interés

#Recuperamos el estrato

train_coordenadas<-train %>% select(property_id,lon,lat)
test_coordenadas<-test %>% select(property_id,lon,lat)

train_join<-train_join %>% left_join(train_coordenadas, by="property_id")
test_join<-test_join %>% left_join(test_coordenadas, by="property_id")

train_join<-train_join %>% select(property_id,city,price,
                                  month,year,surface_total,
                                  surface_covered,rooms,
                                  bedrooms,bathrooms,
                                  property_type,operation_type,
                                  title,description,
                                  estrato_predominante,
                                  lon,lat)

test_join<-test_join %>% select(property_id,city,price,
                                  month,year,surface_total,
                                  surface_covered,rooms,
                                  bedrooms,bathrooms,
                                  property_type,operation_type,
                                  title,description,
                                  estrato_predominante,
                                  lon,lat)

#Guardamos las datas con el estrato asignado.
write.csv(train_join,file = "C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/0.Insumos/train_join.csv")
write.csv(test_join,file = "C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/0.Insumos/test_join.csv")