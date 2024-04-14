set.seed(1234)
#Librerias
library(devtools)
library(tidyverse)

#Cargar la data ####
source_url("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%202/1.Procesamiento%20y%20sintaxis/Script%20para%20cargar%20la%20data%20desde%20el%20repositorio.R")

#Cargamos la función para los marge
source_url("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%202/1.Procesamiento%20y%20sintaxis/funMergePersonasHogares.R")

#Creación de variables de interés####

#Nos interesa las caracteristicas del jefe de hogar####
#Creamos jefe para Train
train_personas$jefe<-ifelse(test = train_personas$P6050==1,1,0)
#Creamos jefe para Test
test_personas$jefe<-ifelse(test = test_personas$P6050==1,1,0)

#Los hogares que tienen una mujer como jefe de hogar podrían ser probablemente más pobres####
train_personas$jefe<-ifelse(test = train_personas$P6050==1,1,0)

crear_sexo_jefe<-function(df){
  aux<-df %>% filter(jefe==1)
  aux2<-data.frame(sexo_jefe=aux$P6020,id=aux$id)
  df<-left_join(df,aux2,by="id")
  return(df)
}

#Creamos la variable en Train
train_personas<-crear_sexo_jefe(train_personas)
train_hogares<-traer_variable(train_hogares,train_personas,"sexo_jefe")
train_hogares$sexo_jefe<-as.factor(train_hogares$sexo_jefe)
#Creamos la variable en Test
test_personas<-crear_sexo_jefe(test_personas)
test_hogares<-traer_variable(test_hogares,test_personas,"sexo_jefe")
test_hogares$sexo_jefe<-as.factor(test_hogares$sexo_jefe)


train_hogares$sexo_jefe<- ifelse(train_hogares$sexo_jefe=='1','1','0')
train_hogares$sexo_jefe <- factor(train_hogares$sexo_jefe, levels = c("1", "0"))

test_hogares$sexo_jefe<- ifelse(test_hogares$sexo_jefe=='1','1','0')
test_hogares$sexo_jefe <- factor(test_hogares$sexo_jefe, levels = c("1", "0"))

#Los hogares cuyo jefe pertenece al regimen subsidiado podrían ser probablemente más pobres####

crear_regimen_jefe<-function(df){
  aux<-df %>% filter(jefe==1)
  aux2<-data.frame(regimen_jefe=aux$P6100,id=aux$id)
  df<-left_join(df,aux2,by="id")
  return(df)
}



#Creamos la variable en Train
train_personas<-crear_regimen_jefe(train_personas)
train_hogares<-traer_variable(train_hogares,train_personas,"regimen_jefe")
train_hogares$regimen_jefe<-as.factor(train_hogares$regimen_jefe)
train_hogares$regimen_subsidiado_jefe<-ifelse(train_hogares$regimen_jefe==3,1,0)

#Creamos la variable en Test
test_personas<-crear_regimen_jefe(test_personas)
test_hogares<-traer_variable(test_hogares,test_personas,"regimen_jefe")
test_hogares$regimen_jefe<-as.factor(test_hogares$regimen_jefe)
test_hogares$regimen_subsidiado_jefe<-ifelse(test_hogares$regimen_jefe==3,1,0)

#Los hogares con jefes con menor nivel de escolaridad podrían ser probablemente más pobres####

crear_educacion_jefe<-function(df){
  aux<-df %>% filter(jefe==1)
  aux2<-data.frame(educacion_jefe=aux$P6210,id=aux$id)
  df<-left_join(df,aux2,by="id")
  return(df)
}

#Creamos la variable en Train
train_personas<-crear_educacion_jefe(train_personas)
#Ya que 9 es el código de no  sabe no informa se reemplazo con Cero asumiendo que la persona que no sabe su nivel educativo es porque no tiene ninguno
train_personas$educacion_jefe<-ifelse(train_personas$educacion_jefe==9,0,train_personas$educacion_jefe)
train_hogares<-traer_variable(train_hogares,train_personas,"educacion_jefe")

#Creamos la variable en Test
test_personas<-crear_educacion_jefe(test_personas)
#Ya que 9 es el código de no  sabe no informa se reemplazo con Cero asumiendo que la persona que no sabe su nivel educativo es porque no tiene ninguno
test_personas$educacion_jefe<-ifelse(test_personas$educacion_jefe==9,0,test_personas$educacion_jefe)
test_hogares<-traer_variable(test_hogares,test_personas,"educacion_jefe")

#Los hogares cuyo jefe de hogar no está empleado probablemente sean más pobres #####

crear_ocupacion_jefe<-function(df){
  aux<-df %>% filter(jefe==1)
  aux2<-data.frame(ocupacion_jefe=aux$P6240,id=aux$id)
  df<-left_join(df,aux2,by="id")
  return(df)
}

#Creamos la variable en Train
train_personas<-crear_ocupacion_jefe(train_personas)
train_hogares<-traer_variable(train_hogares,train_personas,"ocupacion_jefe")
train_hogares$desempleo_jefe<-ifelse(train_hogares$ocupacion_jefe==1,0,1)
table(is.na(train_hogares$desempleo_jefe))
#Tiene un dato perdido. Lo imputamos con un valor aleatorio

train_hogares$desempleo_jefe <- replace(train_hogares$desempleo_jefe,
                                        is.na(train_hogares$desempleo_jefe),
                                        sample(c(0,1),1))
table(is.na(train_hogares$desempleo_jefe))
#Creamos la variable en Test
test_personas<-crear_ocupacion_jefe(test_personas)
test_hogares<-traer_variable(test_hogares,test_personas,"ocupacion_jefe")
test_hogares$desempleo_jefe<-ifelse(test_hogares$ocupacion_jefe==1,0,1)

#Tambien traemos las variables "Pet"  "Oc"  "Des"  "Ina" de los jefes de hogar

crear_Pet_jefe<-function(df){
  aux<-df %>% filter(jefe==1)
  aux2<-data.frame(Pet_jefe=aux$Pet,
                   id=aux$id)
  df<-left_join(df,aux2,by="id")
  return(df)
}
crear_Oc_jefe<-function(df){
  aux<-df %>% filter(jefe==1)
  aux2<-data.frame(Oc_jefe=aux$Oc,
                   id=aux$id)
  df<-left_join(df,aux2,by="id")
  return(df)
}
crear_Des_jefe<-function(df){
  aux<-df %>% filter(jefe==1)
  aux2<-data.frame(Des_jefe=aux$Des,
                   id=aux$id)
  df<-left_join(df,aux2,by="id")
  return(df)
}
crear_Ina_jefe<-function(df){
  aux<-df %>% filter(jefe==1)
  aux2<-data.frame(Ina_jefe=aux$Ina,
                   id=aux$id)
  df<-left_join(df,aux2,by="id")
  return(df)
}

#En Train
train_personas<-crear_Pet_jefe(train_personas)
train_personas<-crear_Oc_jefe(train_personas)
train_personas<-crear_Des_jefe(train_personas)
train_personas<-crear_Ina_jefe(train_personas)

train_hogares<-traer_variable(train_hogares,train_personas,"Pet_jefe")
train_hogares<-traer_variable(train_hogares,train_personas,"Oc_jefe")
train_hogares<-traer_variable(train_hogares,train_personas,"Des_jefe")
train_hogares<-traer_variable(train_hogares,train_personas,"Ina_jefe")

train_hogares$Pet_jefe[is.na(train_hogares$Pet_jefe)] <- 0
train_hogares$Oc_jefe[is.na(train_hogares$Oc_jefe)] <- 0
train_hogares$Des_jefe[is.na(train_hogares$Des_jefe)] <- 0
train_hogares$Ina_jefe[is.na(train_hogares$Ina_jefe)] <- 0


#En Test
test_personas<-crear_Pet_jefe(test_personas)
test_personas<-crear_Oc_jefe(test_personas)
test_personas<-crear_Des_jefe(test_personas)
test_personas<-crear_Ina_jefe(test_personas)

test_hogares<-traer_variable(test_hogares,test_personas,"Pet_jefe")
test_hogares<-traer_variable(test_hogares,test_personas,"Oc_jefe")
test_hogares<-traer_variable(test_hogares,test_personas,"Des_jefe")
test_hogares<-traer_variable(test_hogares,test_personas,"Ina_jefe")

test_hogares$Pet_jefe[is.na(test_hogares$Pet_jefe)] <- 0
test_hogares$Oc_jefe[is.na(test_hogares$Oc_jefe)] <- 0
test_hogares$Des_jefe[is.na(test_hogares$Des_jefe)] <- 0
test_hogares$Ina_jefe[is.na(test_hogares$Ina_jefe)] <- 0

#Posición laboral del jefe de hogar####

crear_posicion_jefe<-function(df){
  aux<-df %>% filter(jefe==1)
  aux2<-data.frame(posicion_jefe=aux$P6430,id=aux$id)
  df<-left_join(df,aux2,by="id")
  return(df)
}

#Creamos la variable en Train
train_personas<-crear_posicion_jefe(train_personas)
train_hogares<-traer_variable(train_hogares,train_personas,"posicion_jefe")
train_hogares$posicion_jefe<-as.factor(train_hogares$posicion_jefe)

#Creamos la variable en Test
test_personas<-crear_posicion_jefe(test_personas)
test_hogares<-traer_variable(test_hogares,test_personas,"posicion_jefe")
test_hogares$posicion_jefe<-as.factor(test_hogares$posicion_jefe)

#Creamos personas por habitación

#En train
train_hogares$Personas_habitacion<-train_hogares$Nper/train_hogares$P5010

train_hogares$Personas_habitacion_r <- round(train_hogares$Personas_habitacion)
train_hogares$Personas_habitacion_r <- as.factor(train_hogares$Personas_habitacion_r)

test_hogares$Personas_habitacion<-test_hogares$Nper/test_hogares$P5010

test_hogares$Personas_habitacion_r <- round(test_hogares$Personas_habitacion)
test_hogares$Personas_habitacion_r <- as.factor(test_hogares$Personas_habitacion_r)

### Tipo de vivienda

train_hogares <- train_hogares %>% mutate(tipo_casa=ifelse(P5090==1,1,0))

train_hogares$tipo_casa <- factor(train_hogares$tipo_casa, levels = c("1", "0"))

### En test
test_hogares <- test_hogares %>% mutate(tipo_casa=ifelse(P5090==1,1,0))

test_hogares$tipo_casa <- factor(test_hogares$tipo_casa, levels = c("1", "0"))


### Edad del jefe de hogar

crear_edad_jefe<-function(df){
  aux<-df %>% filter(jefe==1)
  aux2<-data.frame(edad_jefe=aux$P6040,id=aux$id)
  df<-left_join(df,aux2,by="id")
  return(df)
}

#Creamos la variable en Train
train_personas<-crear_edad_jefe(train_personas)
train_hogares<-traer_variable(train_hogares,train_personas,"edad_jefe")
table(is.na(train_hogares$edad_jefe))

#Creamos la variable en Test
test_personas<-crear_edad_jefe(test_personas)
test_hogares<-traer_variable(test_hogares,test_personas,"edad_jefe")


#Creamos la variable en Train

train_hogares<-train_hogares %>%
  mutate(edad_jefe_joven = ifelse(edad_jefe<=30,1,0))

train_hogares$edad_jefe_joven <- factor(train_hogares$edad_jefe_joven, levels = c("1", "0"))

#Creamos la variable en Test
test_hogares<-test_hogares %>%
  mutate(edad_jefe_joven = ifelse(edad_jefe<=30,1,0))

test_hogares$edad_jefe_joven <- factor(test_hogares$edad_jefe_joven, levels = c("1", "0"))


### Subsidio



crear_subsidio_jefe<-function(df){
  aux<-df %>% filter(jefe==1)
  aux2<-data.frame(subsidio_jefe=aux$P6585s3,id=aux$id)
  df<-left_join(df,aux2,by="id")
  return(df)
}

#Creamos la variable en Train
train_personas<-crear_subsidio_jefe(train_personas)
train_hogares<-traer_variable(train_hogares,train_personas,"subsidio_jefe")
table(is.na(train_hogares$subsidio_jefe))

#Creamos la variable en Test
test_personas<-crear_subsidio_jefe(test_personas)
test_hogares<-traer_variable(test_hogares,test_personas,"subsidio_jefe")



###### clase_rural

# crear_zona_jefe<-function(df){
#   aux<-df %>% filter(jefe==1)
#   aux2<-data.frame(zona_jefe=aux$Dominio,id=aux$id)
#   df<-left_join(df,aux2,by="id")
#   return(df)
# }
# 
# 
# #Creamos la variable en Train
# train_personas<-crear_zona_jefe(train_personas)
# train_hogares<-traer_variable(train_hogares,train_personas,"zona_jefe")
# 
# train_hogares$zona_jefe<-as.factor(train_hogares$zona_jefe)
# train_hogares$Dominio
# train_hogares$zona_jefe<-

# train_hogares <- train_hogares %>%
#   mutate(
#     zona_jefe = case_when(
#       zona_jefe == "RURAL" ~ 1,
#       zona_jefe == "RESTO URBANO" ~ 2,
#       TRUE ~ 3
#     )
#   )


#Creamos la variable en Test
# test_personas<-crear_zona_jefe(test_personas)
# train_hogares<-traer_variable(train_hogares,train_personas,"zona_jefe")
# train_hogares$zona_jefe<-as.factor(train_hogares$zona_jefe)
# 
# test_personas<-crear_zona_jefe(test_personas)
# test_hogares<-traer_variable(test_hogares,test_personas,"zona_jefe")
# test_hogares$zona_jefe<-as.factor(test_hogares$zona_jefe)
# 
# test_hogares <- test_hogares %>%
#   mutate(
#     zona_jefe = case_when(
#       zona_jefe == "RURAL" ~ 1,
#       zona_jefe == "RESTO URBANO" ~ 2,
#       TRUE ~ 3
#     )
#   )
# 


train_hogares$zona_jefe<- ifelse(train_hogares$Dominio=="RURAL",1,0)
table(train_hogares$zona_jefe)
train_hogares$zona_jefe <- factor(train_hogares$zona_jefe, levels = c("1", "2","3"))

test_hogares$zona_jefe<- ifelse(test_hogares$Dominio=="RURAL",1,0)
table(test_hogares$zona_jefe)
test_hogares$zona_jefe <- factor(test_hogares$zona_jefe, levels = c("1", "2","3"))






#### Informalidad


crear_informalidad_jefe<-function(df){
  aux<-df %>% filter(jefe==1)
  aux2<-data.frame(informalidad_jefe=aux$P6870,id=aux$id)
  df<-left_join(df,aux2,by="id")
  return(df)
}

#Creamos la variable en Train
train_personas<-crear_informalidad_jefe(train_personas)
train_hogares<-traer_variable(train_hogares,train_personas,"informalidad_jefe")


#Creamos la variable en Test
test_personas<-crear_informalidad_jefe(test_personas)
test_hogares<-traer_variable(test_hogares,test_personas,"informalidad_jefe")





# Convertir a factor en train
train_hogares$informalidad_jefe <- factor(train_hogares$informalidad_jefe, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
train_hogares$informalidad_jefe <- ifelse(train_hogares$informalidad_jefe %in% c("1","2", "3", "4"), "1", "0")
train_hogares$informalidad_jefe <- factor(train_hogares$informalidad_jefe, levels = c("0", "1"))


# Convertir a factor en Test
test_hogares$informalidad_jefe <- factor(test_hogares$informalidad_jefe, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
test_hogares$informalidad_jefe <- ifelse(test_hogares$informalidad_jefe %in% c("1","2", "3", "4"), "1", "0")
test_hogares$informalidad_jefe <- factor(test_hogares$informalidad_jefe, levels = c("0", "1"))

### Interacción jefe de hogar informalidad

#En train
train_hogares$sexo_jefe_numeric <- as.numeric(train_hogares$sexo_jefe)
train_hogares$informalidad_jefe_numeric <- as.numeric(train_hogares$informalidad_jefe)
train_hogares$Sexo_informalidad <- train_hogares$sexo_jefe_numeric * train_hogares$informalidad_jefe_numeric
train_hogares <- subset(train_hogares, select = -c(sexo_jefe_numeric, informalidad_jefe_numeric))
train_hogares$Sexo_informalidad <- factor(train_hogares$Sexo_informalidad)

#En test
test_hogares$sexo_jefe_numeric <- as.numeric(test_hogares$sexo_jefe)
test_hogares$informalidad_jefe_numeric <- as.numeric(test_hogares$informalidad_jefe)
test_hogares$Sexo_informalidad <- test_hogares$sexo_jefe_numeric * test_hogares$informalidad_jefe_numeric
test_hogares <- subset(test_hogares, select = -c(sexo_jefe_numeric, informalidad_jefe_numeric))
test_hogares$Sexo_informalidad <- factor(test_hogares$Sexo_informalidad)



## Limpieza variable pobre

#En train
train_hogares$Pobre <- factor(train_hogares$Pobre,levels= c('1','0'))

# Personas por habitacion cortada por el punto donde los arboles encuentran diferencias

train_hogares$Personas_habitacion_round<-ifelse(train_hogares$Personas_habitacion<=2.5,1,0)
train_hogares$Personas_habitacion_round<-as.factor(train_hogares$Personas_habitacion_round)

test_hogares$Personas_habitacion_round<-ifelse(test_hogares$Personas_habitacion<=2.5,1,0)
test_hogares$Personas_habitacion_round<-as.factor(test_hogares$Personas_habitacion_round)


#Se organizan los formatos de las Clases
str(train_hogares$Clase)
str(test_hogares$Clase)

train_hogares$Clase<-as.factor(train_hogares$Clase)
test_hogares$Clase<-as.factor(test_hogares$Clase)


# Creamos la variable pobre texto 
train_hogares$pobre_texto<-ifelse(train_hogares$Pobre==1,"Pobre","No_Pobre")


