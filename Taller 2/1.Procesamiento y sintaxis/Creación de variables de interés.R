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
#En test
test_hogares$Personas_habitacion<-test_hogares$Nper/test_hogares$P5010
