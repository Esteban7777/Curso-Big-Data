#Se divide la data train_personas en 7 partes ya que pesa demasiado para github

#Leemos localmente la data
train_personas <- read.csv("C:/Users/HP-Laptop/Desktop/train_personas.csv")

#La dividimos en 7 partes

tamano_parte <- ceiling(nrow(train_personas) / 7)

# Dividir el dataframe en 4 partes
train_personas_1 <- train_personas[1:tamano_parte, ]
train_personas_2 <- train_personas[(tamano_parte + 1):(2*tamano_parte), ]
train_personas_3 <- train_personas[(2*tamano_parte + 1):(3*tamano_parte), ]
train_personas_4 <- train_personas[(3*tamano_parte + 1):(4*tamano_parte), ]
train_personas_5 <- train_personas[(4*tamano_parte + 1):(5*tamano_parte), ]
train_personas_6 <- train_personas[(5*tamano_parte + 1):(6*tamano_parte), ]
train_personas_7 <- train_personas[(6*tamano_parte + 1):nrow(train_personas), ]

write.csv(train_personas_1,file = "C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/0.Insumos/Data/train_personas/train_personas_1.csv")
write.csv(train_personas_2,file = "C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/0.Insumos/Data/train_personas/train_personas_2.csv")
write.csv(train_personas_3,file = "C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/0.Insumos/Data/train_personas/train_personas_3.csv")
write.csv(train_personas_4,file = "C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/0.Insumos/Data/train_personas/train_personas_4.csv")
write.csv(train_personas_5,file = "C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/0.Insumos/Data/train_personas/train_personas_5.csv")
write.csv(train_personas_6,file = "C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/0.Insumos/Data/train_personas/train_personas_6.csv")
write.csv(train_personas_7,file = "C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/0.Insumos/Data/train_personas/train_personas_7.csv")