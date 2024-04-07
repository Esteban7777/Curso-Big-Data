#Cargar la data 
library(tidyverse)

train_hogares<-read.csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%202/0.Insumos/Data/train_hogares.csv")

train_personas <-list()
for (i in 1:7) {
url_data<-paste0("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%202/0.Insumos/Data/train_personas/train_personas_",i,".csv")  
data_train_personas<-read.csv(url_data)
  train_personas[[i]]<-data_train_personas
}

data_train_personas<-NULL
train_personas <- do.call(rbind, train_personas)

test_hogares<-read.csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%202/0.Insumos/Data/test_hogares.csv")
test_personas<-read.csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%202/0.Insumos/Data/test_personas.csv")

variables_hogares<-names(test_hogares)
variables_personas<-names(test_personas)

train_hogares<-train_hogares %>% select(variables_hogares,Ingtotug,Ingtotugarr)
train_personas<-train_personas %>% select(variables_personas,Ingtotob,Ingtotes,Ingtot)
