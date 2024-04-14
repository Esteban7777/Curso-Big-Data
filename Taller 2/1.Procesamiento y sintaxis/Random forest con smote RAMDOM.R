# CODIGO1
set.seed(1234)
library(devtools)
library(randomForest)
library(caret)
library(smotefamily)
source_url("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%202/1.Procesamiento%20y%20sintaxis/Creaci%C3%B3n%20de%20variables%20de%20inter%C3%A9s.R")
<<<<<<< HEAD
#Probando sobre muestreo para random forest
predictores<-c("informalidad_jefe",
               #"subsidio_jefe",
               "edad_jefe",
               #"posicion_jefe",
               "Ina_jefe","Des_jefe","Oc_jefe","Pet_jefe","ocupacion_jefe",
               "educacion_jefe",
               #"regimen_jefe",
               "sexo_jefe","Clase",#"Dominio",
               "P5000",
               #"P5010",
               "P5090",
               #"P5100","P5130","P5140",
               "Nper","Npersug",#"Depto",
               #"regimen_subsidiado_jefe",
               "desempleo_jefe","Personas_habitacion","tipo_casa","edad_jefe_joven",
               "Personas_habitacion_round")
view(predictores)

# Hay que convertir a numerico las variables
# Convertir la variable a formato numérico

str(train_hogares$informalidad_jefe)
train_hogares$informalidad_jefe<-as.numeric(train_hogares$informalidad_jefe)

str(train_hogares$sexo_jefe)
train_hogares$sexo_jefe<-as.numeric(train_hogares$sexo_jefe)

str(train_hogares$Clase)
train_hogares$Clase<-as.numeric(train_hogares$Clase)

str(train_hogares$P5090)
train_hogares$P5090<-as.numeric(train_hogares$P5090)

str(train_hogares$Nper)
train_hogares$Nper<-as.numeric(train_hogares$Nper)

str(train_hogares$Npersug)
train_hogares$Npersug<-as.numeric(train_hogares$Npersug)

str(train_hogares$tipo_casa)
train_hogares$tipo_casa<-as.numeric(train_hogares$tipo_casa)

str(train_hogares$edad_jefe_joven)
train_hogares$edad_jefe_joven<-as.numeric(train_hogares$edad_jefe_joven)

str(train_hogares$Personas_habitacion_r)
train_hogares$Personas_habitacion_r<-as.numeric(train_hogares$Personas_habitacion_r)

str(train_hogares$Pobre)
train_hogares$Pobre <- as.numeric(train_hogares$Pobre)

# Convertir todas las variables en predictores a formato numérico
for (col in predictores) {
  train_hogares[[col]] <- as.numeric(train_hogares[[col]])
}

#NUEVO <- train_hogares[predictores]
# Especifica la ruta donde deseas guardar el archivo CSV
#ruta_descarga <- "C:/Users/windows/Downloads/Descarga/NUEVO.csv"

# Guarda los datos en un archivo CSV
#write.csv(train_hogares, file = ruta_descarga, row.names = FALSE)
=======

# Definir predictores
predictores <- c("informalidad_jefe",
                 "edad_jefe",
                 "Ina_jefe", "Des_jefe", "Oc_jefe", "Pet_jefe", "ocupacion_jefe",
                 "educacion_jefe",
                 "sexo_jefe", "Clase",
                 "P5000",
                 "P5090",
                 "Nper", "Npersug",
                 "desempleo_jefe", "Personas_habitacion", "tipo_casa", "edad_jefe_joven",
                 "Personas_habitacion_round")
>>>>>>> parent of a7b5435 (Ajustes al Modelo)

# Convertir variables factor a numéricas
train_hogares$Personas_habitacion_r <- as.numeric(train_hogares$Personas_habitacion_r)

# Control de entrenamiento
ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE,
                     verbose = FALSE,
                     savePredictions = TRUE)

# Aplicación de SMOTE
smote_output <- SMOTE(X = train_hogares[predictores],
                      target = train_hogares$Pobre)

<<<<<<< HEAD
view(train_hogares[predictores])

=======
>>>>>>> parent of a7b5435 (Ajustes al Modelo)
smote_data <- smote_output$data
smote_data$class <- as.factor(smote_data$class)
levels(smote_data$class) <- make.names(levels(smote_data$class))

# Entrenamiento usando Boosting Trees
pobre_smote_boost <- train(class ~ .,
                           data = smote_data,
                           method = "gbm",
                           trControl = ctrl,
                           verbose = FALSE,
                           tuneLength = 10) # Ajustar este parámetro según necesidad

<<<<<<< HEAD
# Evaluación del modelo
predictions <- predict(ada_model, newdata = test_hogares)
confusionMatrix <- confusionMatrix(predictions, test_hogares$class)

# Guardar las predicciones en el conjunto de test
test_hogares$predic_ada <- predictions
table(test_hogares$predic_ada)

# Preparar el archivo de sumisión
sub15 <- test_hogares %>% select(id, predic_ada)
sub15 <- sub15 %>% rename(pobre = predic_ada)
sub15$pobre <- ifelse(sub15$pobre == "X1", 1, 0)
write_csv(x = sub15, "C://Submission15.csv")


=======
# Resumen del modelo
print(pobre_smote_boost)
>>>>>>> parent of a7b5435 (Ajustes al Modelo)
