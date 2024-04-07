#Librerias
library(devtools)
library(tidyverse)

#Cargar la data ####
source_url("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%202/1.Procesamiento%20y%20sintaxis/Script%20para%20cargar%20la%20data%20desde%20el%20repositorio.R")

#Cargamos la función para los marge
source_url("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%202/1.Procesamiento%20y%20sintaxis/funMergePersonasHogares.R")

#Creación de variables de interés####


#Los hogares que tienen una mujere como jefe de hogar podrían ser probablemente más pobres####
train_personas$jefe<-ifelse(test = train_personas$P6050==1,1,0)

crear_sexo_jefe<-function(df){
  aux<-df %>% filter(jefe==1)
  aux2<-data.frame(sexo_jefe=aux$P6020,id=aux$id)
  df<-left_join(df,aux2,by="id")
  return(df)
}

train_personas<-crear_sexo_jefe(train_personas)
train_hogares<-traer_variable(train_hogares,train_personas,"sexo_jefe")



train_hogares<-traer_variable(train_hogares,train_personas,"reg_subsidiado")
