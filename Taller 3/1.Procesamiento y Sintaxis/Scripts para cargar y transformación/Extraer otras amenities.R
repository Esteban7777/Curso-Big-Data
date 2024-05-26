library(sf)
library(osmdata)

test2 <- test %>% st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")
train2 <- train %>%  st_as_sf(coords = c("Lon", "Lat"), crs = 4326, agr = "constant")

mapa_bogota<-opq(bbox = getbb("Bogota Colombia"))

##Centros comerciales

centros_comerciales <- mapa_bogota %>%
  add_osm_feature(key = 'shop'  , value = 'mall') %>%
  osmdata_sf()


cc <- centros_comerciales$osm_points


# Instituciones educativas
escuelas <- mapa_bogota %>%
  add_osm_feature(key = 'amenity', value = 'school') %>%
  osmdata_sf()

esc<- escuelas$osm_points

universidades<-mapa_bogota %>%
  add_osm_feature(key = 'amenity', value = 'university') %>%
  osmdata_sf()

uni <- universidades$osm_points


#Zonas verdes

cesped <- mapa_bogota %>%
  add_osm_feature(key = 'landuse', value = 'grass') %>%
  osmdata_sf()

cesp <- cesped$osm_points

bosques <- mapa_bogota %>%
  add_osm_feature(key = 'landuse', value = 'forest') %>%
  osmdata_sf()

bosques <- bosques$osm_points


parques <- mapa_bogota %>%
  add_osm_feature(key = 'leisure', value = 'park') %>%
  osmdata_sf()

parq <- parques$osm_points


#Entretenimiento

cinemas <- mapa_bogota %>%
  add_osm_feature(key = 'amenity', value = 'cinema') %>%
  osmdata_sf()

cine <- cinemas$osm_points


discotecas <- mapa_bogota %>%
  add_osm_feature(key = 'amenity', value = 'nightclub') %>%
  osmdata_sf()

disco <- discotecas$osm_points


# Transporte público

estaciones <- mapa_bogota %>%
  add_osm_feature(key = 'amenity', value = 'bus_station') %>%
  osmdata_sf()

estacion <- estaciones$osm_points


# Calcular distancias

train2$distancia_centro_comercial <- st_nearest_feature(train2, cc)
test2$distancia_centro_comercial <- st_nearest_feature(test2, cc)

train2$distancia_escuela <- st_nearest_feature(train2, esc)
test2$distancia_escuela <- st_nearest_feature(test2, esc)

train2$distancia_universidad <- st_nearest_feature(train2, uni)
test2$distancia_universidad <- st_nearest_feature(test2, uni)

train2$distancia_cesped <- st_nearest_feature(train2, cesp)
test2$distancia_cesped <- st_nearest_feature(test2, cesp)

train2$distancia_bosques <- st_nearest_feature(train2, bosques)
test2$distancia_bosques <- st_nearest_feature(test2, bosques)

train2$distancia_parque <- st_nearest_feature(train2, parq)
test2$distancia_parque <- st_nearest_feature(test2, parq)

train2$distancia_cine <- st_nearest_feature(train2, cine)
test2$distancia_cine <- st_nearest_feature(test2, cine)

train2$distancia_discoteca <- st_nearest_feature(train2, disco)
test2$distancia_discoteca <- st_nearest_feature(test2, disco)

train2$distancia_transporte <- st_nearest_feature(train2, estacion)
test2$distancia_transporte <- st_nearest_feature(test2, estacion)


amenities_train<-train2 %>% select(distancia_centro_comercial,distancia_escuela,
                                   distancia_universidad,distancia_cesped,
                                   distancia_bosques,distancia_parque,
                                   distancia_cine,distancia_discoteca,
                                   distancia_transporte)


amenities_test<-test2 %>% select(distancia_centro_comercial,distancia_escuela,
                                   distancia_universidad,distancia_cesped,
                                   distancia_bosques,distancia_parque,
                                   distancia_cine,distancia_discoteca,
                                   distancia_transporte)


write.csv(amenities_train,file = "C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/0.Insumos/amenities_train.csv")
write.csv(amenities_test,file = "C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/0.Insumos/amenities_test.csv")