#Cargamos librerias
library(devtools)
library(tidyverse)
library(stringi)
library(tokenizers)
library(stopwords)
library(SnowballC)
library(wordcloud)


#Leemos la data
train<-read_csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/Taller_3/train_join2.csv")
test<-read_csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/Taller_3/test_join2.csv")

#Homogeneizamos las descripciones
test<-test %>% mutate(description2=str_to_lower(description)) %>% 
  mutate(description2=iconv(description2,from="UTF-8",to="ASCII//TRANSLIT")) %>% 
  mutate(description2=str_replace_all(description2,"[^[:alnum:]]", " ")) %>% 
  mutate(description2=str_trim(gsub("\\s+", " ", description2)) ) %>% 
  mutate(description2 = gsub("\\buno\\b", "1", description2, ignore.case = TRUE),
         description2 = gsub("\\bdos\\b", "2", description2, ignore.case = TRUE),
         description2 = gsub("\\btres\\b", "3", description2, ignore.case = TRUE),
         description2 = gsub("\\bcuatro\\b", "4", description2, ignore.case = TRUE),
         description2 = gsub("\\bcinco\\b", "5", description2, ignore.case = TRUE),
         description2 = gsub("\\bseis\\b", "6", description2, ignore.case = TRUE),
         description2 = gsub("\\bsiete\\b", "7", description2, ignore.case = TRUE),
         description2 = gsub("\\bocho\\b", "8", description2, ignore.case = TRUE),
         description2 = gsub("\\bnueve\\b", "9", description2, ignore.case = TRUE),
         description2 = gsub("\\bdiez\\b", "10", description2, ignore.case = TRUE),
         description2 = gsub("\\bonce\\b", "11", description2, ignore.case = TRUE),
         description2 = gsub("\\bdoce\\b", "12", description2, ignore.case = TRUE),
         description2 = gsub("\\btrece\\b", "13", description2, ignore.case = TRUE),
         description2 = gsub("\\bun\\b", "1", description2, ignore.case = TRUE)
           )

#Necesitamos extraer el numero de baños de las descripciones
#Primero extramos los numeros que se encuentren antes de la palabra baños
test <- test %>%
  mutate(banios= str_extract(description2, "(\\d+) ba[on]os"))

#Luego extraemos el número veces que se repite baño en la descripcion
test <- test %>%
  mutate(banios_cuenta=str_count(description2,"\\bba[on]o\\b"))
           
#Unimos los dos hallazgos
test<-test %>% 
  mutate(nbanios=ifelse(is.na(banios), banios_cuenta,banios
  ))

table(test$nbanios)
table(is.na(test$nbanios))

#Se identifican algunos registros mal asignados con valores grandes
#Se revisan algunos registros y se identifica problemas de espacios con digitos de información anterior
#Se decide tomar el último digito de cada uno de estos registros

test<-test %>% mutate(nbanios=str_replace_all(nbanios, "\\bba[on]os\\b",""))

test<-test %>% mutate(nbanios=str_trim(nbanios))

test<-test %>% mutate(nbanios=substr(nbanios, nchar(nbanios), nchar(nbanios)))

table(is.na(test$nbanios))
table(test$nbanios)

#Si no tenemos cantidad de baños en nbanios se la extramos del que viene por base
test<-test %>% 
  mutate(nbanios=ifelse(nbanios==0, bathrooms,nbanios
  ))

table(test$nbanios)
table(is.na(test$nbanios))
#Dado que se asume que deben tener al menos un baño 

#Reemplazamos NA con 1
test<-test %>% 
  mutate(nbanios=ifelse(is.na(nbanios), 1,nbanios
  ))

table(test$nbanios)
table(is.na(test$nbanios))

#Revisamos cuantos registros no coincide el baño de la base de datos con el extraido de la descripcion
table(test$nbanios==test$bathrooms)
table(is.na(test$nbanios))
table(is.na(test$bathrooms))
table(test$nbanios<test$bathrooms)
table(test$bathrooms)

#Ahora trabajamos en la variable habitaciones
#Primero extramos los numeros que se encuentren antes de la palabra baños

test <- test %>%
  mutate(habitaciones= str_extract(description2, "(\\d+) habitaciones"))

test <- test %>%
  mutate(cuartos= str_extract(description2, "(\\d+) cuartos"))

test <- test %>%
  mutate(piezas= str_extract(description2, "(\\d+) piezas"))

test <- test %>%
  mutate(dormitorios= str_extract(description2, "(\\d+) dormitorios"))

test <- test %>%
  mutate(alcobas= str_extract(description2, "(\\d+) alcobas"))


table(test$dormitorios)
table(is.na(test$piezas))
table(test$cuartos)
table(test$habitaciones)
table(test$alcobas)

#Piezas fue la única que no sirvió 
test$piezas<-NULL

table(is.na(test$dormitorios))
table(is.na(test$cuartos))
table(is.na(test$habitaciones))
table(is.na(test$alcobas))

#Dejamos solo los digitos
test<-test %>% mutate(habitaciones=str_replace_all(habitaciones, "habitaciones","")) %>%
  mutate(cuartos=str_replace_all(cuartos, "cuartos","")) %>% 
  mutate(dormitorios=str_replace_all(dormitorios, "dormitorios",""))%>% 
  mutate(alcobas=str_replace_all(alcobas, "alcobas",""))

test<-test %>% mutate(habitaciones=str_trim(habitaciones)) %>% 
  mutate(cuartos=str_trim(cuartos)) %>% 
  mutate(dormitorios=str_trim(dormitorios)) %>% 
  mutate(alcobas=str_trim(alcobas))

#Tomamos el último digito para evitar datos extremos
#Esto se hace salvo para aquellos que tienen más de once habitaciones
test$alcobas<-as.numeric(test$alcobas)
test$dormitorios<-as.numeric(test$dormitorios)
test$cuartos<-as.numeric(test$cuartos)
test$habitaciones<-as.numeric(test$habitaciones)

test<-test %>% 
  mutate(habitaciones=ifelse(habitaciones>11,
                             substr(habitaciones,
                                    nchar(habitaciones),
                                    nchar(habitaciones)),
                             habitaciones)) %>% 
  mutate(cuartos=ifelse(cuartos>11,
                        substr(cuartos,
                               nchar(cuartos),
                               nchar(cuartos)),
                        cuartos)) %>% 
  mutate(dormitorios=ifelse(dormitorios>11,
                            substr(dormitorios,
                                   nchar(dormitorios),
                                   nchar(dormitorios)),
                            dormitorios)) %>% 
  mutate(alcobas=ifelse(alcobas>11,
                        substr(alcobas,
                               nchar(alcobas),
                               nchar(alcobas)),alcobas))

table(test$alcobas)
table(test$dormitorios)
table(test$cuartos)
table(test$habitaciones)

test$alcobas<-as.numeric(test$alcobas)
test$dormitorios<-as.numeric(test$dormitorios)
test$cuartos<-as.numeric(test$cuartos)
test$habitaciones<-as.numeric(test$habitaciones)

#Ponemos los NA como ceros
test<-test %>% 
  mutate(habitaciones=ifelse(is.na(habitaciones),
                             0,habitaciones)) %>% 
  mutate(cuartos=ifelse(is.na(cuartos),
                        0,cuartos)) %>% 
  mutate(dormitorios=ifelse(is.na(dormitorios),
                            0,dormitorios)) %>% 
  mutate(alcobas=ifelse(is.na(alcobas),
                        0,alcobas))


#Intentamos unir habitaciones y alcobas
table(is.na(test$habitaciones),is.na(test$alcobas))
#Hay 37 registros con información en alcobas y en habitaciones
#Exploramos para poder escoger
revision<-test %>% filter(!is.na(habitaciones) & !is.na(alcobas))

#Identificamos que se repiten porque en alguna parte se anuncia la cantidad de habitaciones
#Luego se describen cuantas tienen una caracteristica y cuantas otra.
#La regla de decisión utilizará para estos casos es tomar el valor más grande entre los dos

test<-test %>%
  mutate(nhabitaciones=pmax(habitaciones,
                            alcobas,
                            cuartos,
                            dormitorios))

table(is.na(test$nhabitaciones))
table(test$nhabitaciones)

#Revisamos la variable que viene por base
table(is.na(test$bedrooms))
table(is.na(test$rooms))

#Se trabaja con bedrooms que no tiene NA 
table(test$bedrooms)
table(test$nhabitaciones)

#Se crear la variable con la que tenga el número más alto de habitaciones 

table(test$bedrooms==test$nhabitaciones)

test<-test %>%
  mutate(nhabitaciones=pmax(nhabitaciones,
                            bedrooms))

table(test$nhabitaciones)


#Extraemos el piso donde se ubica el apartamento
#Cambiamos las categorias ordinales en número
test<-test %>% mutate(description2 = gsub("\\bprimer\\b", "1", description2, ignore.case = TRUE),
         description2 = gsub("\\bsegundo\\b", "2", description2, ignore.case = TRUE),
         description2 = gsub("\\btercer\\b", "3", description2, ignore.case = TRUE),
         description2 = gsub("\\bcuarto\\b", "4", description2, ignore.case = TRUE),
         description2 = gsub("\\bquinto\\b", "5", description2, ignore.case = TRUE),
         description2 = gsub("\\bsexto\\b", "6", description2, ignore.case = TRUE),
         description2 = gsub("\\bseptimo\\b", "7", description2, ignore.case = TRUE),
         description2 = gsub("\\boctavo\\b", "8", description2, ignore.case = TRUE),
         description2 = gsub("\\bnoveno\\b", "9", description2, ignore.case = TRUE),
         description2 = gsub("\\bdecimo\\b", "10", description2, ignore.case = TRUE))


test <- test %>%
  mutate(piso_apartamento1= str_extract(description2, "(\\d+) piso"))

test <- test %>%
  mutate(piso_apartamento2= str_extract(description2, "piso (\\d+)"))

table(is.na(test$piso_apartamento1),is.na(test$piso_apartamento2))
#Hay 375 registros con dato en ambas variables exploramos por qué
revision<-test %>% filter(!is.na(piso_apartamento1) & !is.na(piso_apartamento2))

#Se identifica que a veces hay información númerica despues de la palabra piso que refiere a otra caracteristica
#La regla que se utilizará es unicamente si piso_apartamento1 es vacio utilizar piso_apartamento2 

test <- test %>%
  mutate(piso_apartamento=ifelse(is.na(piso_apartamento1),piso_apartamento2,piso_apartamento1))

table(test$piso_apartamento)
table(is.na(test$piso_apartamento))

#Se revisa la realación de pisos por tipo de apartamento
table(test$piso_apartamento,test$property_type)

#Las casas no pueden estar en un piso que no sea el primero

test <- test %>%
  mutate(piso_apartamento=ifelse(property_type=="Casa",1,piso_apartamento))

table(test$piso_apartamento)
table(is.na(test$piso_apartamento))

test<-test %>% mutate(piso_apartamento=str_replace_all(piso_apartamento, "piso","")) %>%
  mutate(piso_apartamento=as.numeric(piso_apartamento))

#Se observan unos datos atipicos, para aquellos que son mayores del piso 33 se toma el primer digito

test<-test %>% 
  mutate(piso_apartamento=ifelse(piso_apartamento>33,
                             substr(piso_apartamento,
                                    1,
                                    1),
                             piso_apartamento))

table(test$piso_apartamento)

#Llenamos los vacios con un aleatorio entre 1 y 4
set.seed(1234)

posiciones_na <- which(is.na(test$piso_apartamento))

test$piso_apartamento[posiciones_na] <- sample(1:4, length(posiciones_na), replace = TRUE)


table(test$piso_apartamento)
table(is.na(test$piso_apartamento))

##### Repetimos el mismo procedimiento para train #####

train<-train %>% mutate(description2=str_to_lower(description)) %>% 
  mutate(description2=iconv(description2,from="UTF-8",to="ASCII//TRANSLIT")) %>% 
  mutate(description2=str_replace_all(description2,"[^[:alnum:]]", " ")) %>% 
  mutate(description2=str_trim(gsub("\\s+", " ", description2)) ) %>% 
  mutate(description2 = gsub("\\buno\\b", "1", description2, ignore.case = TRUE),
         description2 = gsub("\\bdos\\b", "2", description2, ignore.case = TRUE),
         description2 = gsub("\\btres\\b", "3", description2, ignore.case = TRUE),
         description2 = gsub("\\bcuatro\\b", "4", description2, ignore.case = TRUE),
         description2 = gsub("\\bcinco\\b", "5", description2, ignore.case = TRUE),
         description2 = gsub("\\bseis\\b", "6", description2, ignore.case = TRUE),
         description2 = gsub("\\bsiete\\b", "7", description2, ignore.case = TRUE),
         description2 = gsub("\\bocho\\b", "8", description2, ignore.case = TRUE),
         description2 = gsub("\\bnueve\\b", "9", description2, ignore.case = TRUE),
         description2 = gsub("\\bdiez\\b", "10", description2, ignore.case = TRUE),
         description2 = gsub("\\bonce\\b", "11", description2, ignore.case = TRUE),
         description2 = gsub("\\bdoce\\b", "12", description2, ignore.case = TRUE),
         description2 = gsub("\\btrece\\b", "13", description2, ignore.case = TRUE),
         description2 = gsub("\\bun\\b", "1", description2, ignore.case = TRUE)
  )

#Necesitamos extraer el numero de baños de las descripciones
#Primero extramos los numeros que se encuentren antes de la palabra baños
train <- train %>%
  mutate(banios= str_extract(description2, "(\\d+) ba[on]os"))

#Luego extraemos el número veces que se repite baño en la descripcion
train <- train %>%
  mutate(banios_cuenta=str_count(description2,"\\bba[on]o\\b"))

#Unimos los dos hallazgos
train<-train %>% 
  mutate(nbanios=ifelse(is.na(banios), banios_cuenta,banios
  ))

table(train$nbanios)
table(is.na(train$nbanios))

#Se identifican algunos registros mal asignados con valores grandes
#Se revisan algunos registros y se identifica problemas de espacios con digitos de información anterior
#Se decide tomar el último digito de cada uno de estos registros

train<-train %>% mutate(nbanios=str_replace_all(nbanios, "\\bba[on]os\\b",""))

train<-train %>% mutate(nbanios=str_trim(nbanios))

train<-train %>% mutate(nbanios=substr(nbanios, nchar(nbanios), nchar(nbanios)))

table(is.na(train$nbanios))
table(train$nbanios)

#Si no tenemos cantidad de baños en nbanios se la extramos del que viene por base
train<-train %>% 
  mutate(nbanios=ifelse(nbanios==0, bathrooms,nbanios
  ))

table(train$nbanios)
table(is.na(train$nbanios))
#Dado que se asume que deben tener al menos un baño 

#Reemplazamos NA con 1
train<-train %>% 
  mutate(nbanios=ifelse(is.na(nbanios), 1,nbanios
  ))

table(train$nbanios)
table(is.na(train$nbanios))

#Revisamos cuantos registros no coincide el baño de la base de datos con el extraido de la descripcion
table(train$nbanios==train$bathrooms)
table(is.na(train$nbanios))
table(is.na(train$bathrooms))
table(train$nbanios<train$bathrooms)
table(train$bathrooms)

#Ahora trabajamos en la variable habitaciones
#Primero extramos los numeros que se encuentren antes de la palabra baños

train <- train %>%
  mutate(habitaciones= str_extract(description2, "(\\d+) habitaciones"))

train <- train %>%
  mutate(cuartos= str_extract(description2, "(\\d+) cuartos"))

train <- train %>%
  mutate(piezas= str_extract(description2, "(\\d+) piezas"))

train <- train %>%
  mutate(dormitorios= str_extract(description2, "(\\d+) dormitorios"))

train <- train %>%
  mutate(alcobas= str_extract(description2, "(\\d+) alcobas"))


table(train$dormitorios)
table(is.na(train$piezas))
table(train$cuartos)
table(train$habitaciones)
table(train$alcobas)

#Piezas fue la única que no sirvió 
train$piezas<-NULL

table(is.na(train$dormitorios))
table(is.na(train$cuartos))
table(is.na(train$habitaciones))
table(is.na(train$alcobas))

#Dejamos solo los digitos
train<-train %>% mutate(habitaciones=str_replace_all(habitaciones, "habitaciones","")) %>%
  mutate(cuartos=str_replace_all(cuartos, "cuartos","")) %>% 
  mutate(dormitorios=str_replace_all(dormitorios, "dormitorios",""))%>% 
  mutate(alcobas=str_replace_all(alcobas, "alcobas",""))

train<-train %>% mutate(habitaciones=str_trim(habitaciones)) %>% 
  mutate(cuartos=str_trim(cuartos)) %>% 
  mutate(dormitorios=str_trim(dormitorios)) %>% 
  mutate(alcobas=str_trim(alcobas))

#Tomamos el último digito para evitar datos extremos
#Esto se hace salvo para aquellos que tienen más de once habitaciones
train$alcobas<-as.numeric(train$alcobas)
train$dormitorios<-as.numeric(train$dormitorios)
train$cuartos<-as.numeric(train$cuartos)
train$habitaciones<-as.numeric(train$habitaciones)

train<-train %>% 
  mutate(habitaciones=ifelse(habitaciones>11,
                             substr(habitaciones,
                                    nchar(habitaciones),
                                    nchar(habitaciones)),
                             habitaciones)) %>% 
  mutate(cuartos=ifelse(cuartos>11,
                        substr(cuartos,
                               nchar(cuartos),
                               nchar(cuartos)),
                        cuartos)) %>% 
  mutate(dormitorios=ifelse(dormitorios>11,
                            substr(dormitorios,
                                   nchar(dormitorios),
                                   nchar(dormitorios)),
                            dormitorios)) %>% 
  mutate(alcobas=ifelse(alcobas>11,
                        substr(alcobas,
                               nchar(alcobas),
                               nchar(alcobas)),alcobas))

table(train$alcobas)
table(train$dormitorios)
table(train$cuartos)
table(train$habitaciones)

train$alcobas<-as.numeric(train$alcobas)
train$dormitorios<-as.numeric(train$dormitorios)
train$cuartos<-as.numeric(train$cuartos)
train$habitaciones<-as.numeric(train$habitaciones)

#Ponemos los NA como ceros
train<-train %>% 
  mutate(habitaciones=ifelse(is.na(habitaciones),
                             0,habitaciones)) %>% 
  mutate(cuartos=ifelse(is.na(cuartos),
                        0,cuartos)) %>% 
  mutate(dormitorios=ifelse(is.na(dormitorios),
                            0,dormitorios)) %>% 
  mutate(alcobas=ifelse(is.na(alcobas),
                        0,alcobas))


#Intentamos unir habitaciones y alcobas
table(is.na(train$habitaciones),is.na(train$alcobas))
#Hay 37 registros con información en alcobas y en habitaciones
#Exploramos para poder escoger
revision<-train %>% filter(!is.na(habitaciones) & !is.na(alcobas))

#Identificamos que se repiten porque en alguna parte se anuncia la cantidad de habitaciones
#Luego se describen cuantas tienen una caracteristica y cuantas otra.
#La regla de decisión utilizará para estos casos es tomar el valor más grande entre los dos

train<-train %>%
  mutate(nhabitaciones=pmax(habitaciones,
                            alcobas,
                            cuartos,
                            dormitorios))

table(is.na(train$nhabitaciones))
table(train$nhabitaciones)

#Revisamos la variable que viene por base
table(is.na(train$bedrooms))
table(is.na(train$rooms))

#Se trabaja con bedrooms que no tiene NA 
table(train$bedrooms)
table(train$nhabitaciones)

#Se crear la variable con la que tenga el número más alto de habitaciones 

table(train$bedrooms==train$nhabitaciones)

train<-train %>%
  mutate(nhabitaciones=pmax(nhabitaciones,
                            bedrooms))

table(train$nhabitaciones)


#Extraemos el piso donde se ubica el apartamento
#Cambiamos las categorias ordinales en número
train<-train %>% mutate(description2 = gsub("\\bprimer\\b", "1", description2, ignore.case = TRUE),
                      description2 = gsub("\\bsegundo\\b", "2", description2, ignore.case = TRUE),
                      description2 = gsub("\\btercer\\b", "3", description2, ignore.case = TRUE),
                      description2 = gsub("\\bcuarto\\b", "4", description2, ignore.case = TRUE),
                      description2 = gsub("\\bquinto\\b", "5", description2, ignore.case = TRUE),
                      description2 = gsub("\\bsexto\\b", "6", description2, ignore.case = TRUE),
                      description2 = gsub("\\bseptimo\\b", "7", description2, ignore.case = TRUE),
                      description2 = gsub("\\boctavo\\b", "8", description2, ignore.case = TRUE),
                      description2 = gsub("\\bnoveno\\b", "9", description2, ignore.case = TRUE),
                      description2 = gsub("\\bdecimo\\b", "10", description2, ignore.case = TRUE))


train <- train %>%
  mutate(piso_apartamento1= str_extract(description2, "(\\d+) piso"))

train <- train %>%
  mutate(piso_apartamento2= str_extract(description2, "piso (\\d+)"))

table(is.na(train$piso_apartamento1),is.na(train$piso_apartamento2))
#Hay 375 registros con dato en ambas variables exploramos por qué
revision<-train %>% filter(!is.na(piso_apartamento1) & !is.na(piso_apartamento2))

#Se identifica que a veces hay información númerica despues de la palabra piso que refiere a otra caracteristica
#La regla que se utilizará es unicamente si piso_apartamento1 es vacio utilizar piso_apartamento2 

train <- train %>%
  mutate(piso_apartamento=ifelse(is.na(piso_apartamento1),piso_apartamento2,piso_apartamento1))

table(train$piso_apartamento)
table(is.na(train$piso_apartamento))

#Se revisa la realación de pisos por tipo de apartamento
table(train$piso_apartamento,train$property_type)

#Las casas no pueden estar en un piso que no sea el primero

train <- train %>%
  mutate(piso_apartamento=ifelse(property_type=="Casa",1,piso_apartamento))

table(train$piso_apartamento)
table(is.na(train$piso_apartamento))

train<-train %>% mutate(piso_apartamento=str_replace_all(piso_apartamento, "piso","")) %>%
  mutate(piso_apartamento=as.numeric(piso_apartamento))

#Se observan unos datos atipicos, para aquellos que son mayores del piso 33 se toma el primer digito

train<-train %>% 
  mutate(piso_apartamento=ifelse(piso_apartamento>33,
                                 substr(piso_apartamento,
                                        1,
                                        1),
                                 piso_apartamento))

table(train$piso_apartamento)

#Llenamos los vacios con un aleatorio entre 1 y 4
set.seed(1234)

posiciones_na <- which(is.na(train$piso_apartamento))

train$piso_apartamento[posiciones_na] <- sample(1:4, length(posiciones_na), replace = TRUE)


table(train$piso_apartamento)
table(is.na(train$piso_apartamento))


#Se deflactan los precios para que queden en base 2019

inflacion<-read_csv2("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/Inflacion.csv")

inflacion<-inflacion %>% arrange(Periodo) %>% 
  mutate(inflacion_acum=cumprod(1+Inflacion)) %>% 
  rename(fecha=Mes)


train <- train %>%
  mutate(fecha=ifelse(month>9,
                      paste0("01/",month,"/",year),
                      paste0("01/0",month,"/",year)))%>%
  left_join(inflacion, by = "fecha") %>%
  mutate(precio_2019 = price / inflacion_acum)

table(train$precio_2019*train$inflacion_acum==train$price)
train$precio_actual<-train$precio_2019*train$inflacion_acum
train$validacion<-train$price-train$precio_actual
table(train$validacion)

#### VALIDAMOS QUE LAS DATAS HAYAN QUEDADO CORRECTAMENTE PROCESADAS ####



table(test$)



