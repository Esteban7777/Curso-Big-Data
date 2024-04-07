#Este script convierte la data de train_personas en formato .R
#ya que es un archivo muy grande para ser cargado en github como .csv

#importamos la data descargada localmente

train_personas <- read.csv("C:/Users/HP-Laptop/Desktop/train_personas.csv")

save(train_personas,file = "train_personas.R")