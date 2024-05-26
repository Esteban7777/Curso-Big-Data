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

####################
# Iglesias
iglesias <- mapa_bogota %>%
  add_osm_feature(key = 'amenity', value = 'place_of_worship') %>%
  osmdata_sf()

iglesia <- iglesias$osm_points

#Supermercados
supermercados <- mapa_bogota %>%
  add_osm_feature(key = 'shop', value = 'supermarket') %>%
  osmdata_sf()

super <- supermercados$osm_points

#Comida
restaurantes <- mapa_bogota %>%
  add_osm_feature(key = 'amenity', value = 'restaurant') %>%
  osmdata_sf()

rest <- restaurantes$osm_points

comida_rapida<- mapa_bogota %>%
  add_osm_feature(key = 'amenity', value = 'fast_food') %>%
  osmdata_sf()

fast_food <- comida_rapida$osm_points

cafeteria<- mapa_bogota %>%
  add_osm_feature(key = 'amenity', value = 'cafe') %>%
  osmdata_sf()

cafe <- cafeteria$osm_points

#Bancos

bancos<- mapa_bogota %>%
  add_osm_feature(key = 'amenity', value = 'bank') %>%
  osmdata_sf()

banco <- bancos$osm_points

cajeros_automaticos<- mapa_bogota %>%
  add_osm_feature(key = 'amenity', value = 'atm') %>%
  osmdata_sf()

cajeros <- cajeros_automaticos$osm_points

#Gimnasios

gimnasios<- mapa_bogota %>%
  add_osm_feature(key = 'leisure', value = 'fitness_centre') %>%
  osmdata_sf()

gym <- gimnasios$osm_points

#Bombas de gasolina
bombas_gas<- mapa_bogota %>%
  add_osm_feature(key = 'amenity', value = 'fuel') %>%
  osmdata_sf()

bombas <- bombas_gas$osm_points


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

#####
train2$distancia_iglesia <- st_nearest_feature(train2, iglesia)
test2$distancia_iglesia <- st_nearest_feature(test2, iglesia)

train2$distancia_supermercado <- st_nearest_feature(train2, super)
test2$distancia_supermercado <- st_nearest_feature(test2, super)

train2$distancia_restaurante <- st_nearest_feature(train2, rest)
test2$distancia_restaurante <- st_nearest_feature(test2, rest)

train2$distancia_comida_rapida <- st_nearest_feature(train2, fast_food)
test2$distancia_comida_rapida <- st_nearest_feature(test2, fast_food)

train2$distancia_cafeteria <- st_nearest_feature(train2, cafe)
test2$distancia_cafeteria <- st_nearest_feature(test2, cafe)

train2$distancia_banco <- st_nearest_feature(train2, banco)
test2$distancia_banco <- st_nearest_feature(test2, banco)

train2$distancia_cajero <- st_nearest_feature(train2, cajeros)
test2$distancia_cajero <- st_nearest_feature(test2, cajeros)

train2$distancia_gym <- st_nearest_feature(train2, gym)
test2$distancia_gym <- st_nearest_feature(test2, gym)




amenities_train<-train2 %>% select(distancia_centro_comercial,distancia_escuela,distancia_universidad,
                                   distancia_cesped,distancia_bosques,distancia_parque,
                                   distancia_cine,distancia_discoteca,distancia_transporte,
                                   distancia_iglesia,distancia_supermercado,
                                   distancia_restaurante,distancia_restaurante,distancia_comida_rapida,
                                   distancia_cafeteria,distancia_banco,distancia_cajero,
                                   distancia_gym)


amenities_test<-test2 %>% select(distancia_centro_comercial,distancia_escuela,distancia_universidad,
                                 distancia_cesped,distancia_bosques,distancia_parque,
                                 distancia_cine,distancia_discoteca,distancia_transporte,
                                 distancia_iglesia,distancia_supermercado,
                                 distancia_restaurante,distancia_restaurante,distancia_comida_rapida,
                                 distancia_cafeteria,distancia_banco,distancia_cajero,
                                 distancia_gym)


write.csv(amenities_train,file = "C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/0.Insumos/amenities_train.csv")
write.csv(amenities_test,file = "C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/0.Insumos/amenities_test.csv")