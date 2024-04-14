set.seed(1234)
library(devtools)
library(randomForest)
library(caret)
library(smotefamily)
library(ada)
library(caret)
library(smotefamily)
source_url("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%202/1.Procesamiento%20y%20sintaxis/Creaci%C3%B3n%20de%20variables%20de%20inter%C3%A9s.R")

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


#Hay que convertir a numerico las variables 
str(train_hogares$informalidad_jefe)
str(train_hogares$sexo_jefe)
str(train_hogares$Clase)
str(train_hogares$P5090)
str(train_hogares$Nper)
str(train_hogares$Npersug)
str(train_hogares$tipo_casa)
str(train_hogares$edad_jefe_joven)
str(train_hogares$Personas_habitacion_r)
#str(train_hogares$Pobre)
#Las variables de arriba son las que vienen como factor, se convirtieron una a una en número
train_hogares$Personas_habitacion_r<-as.numeric(train_hogares$Personas_habitacion_r)


ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE,
                     verbose = FALSE,
                     savePredictions = TRUE)

smote_output <- SMOTE(X = train_hogares[predictores],
                      target = train_hogares$Pobre)

smote_data <- smote_output$data


smote_output <- SMOTE(X = train_hogares[predictores],
                      target = train_hogares$Pobre)

smote_data<-smote_output$data


smote_data$class <- as.factor(smote_data$class)
levels(smote_data$class) <- make.names(levels(smote_data$class))

# Modelo AdaBoost
ada_model <- train(class ~ .,
                   data = smote_data,
                   method = "ada",
                   trControl = ctrl,
                   tuneLength = 5)  # Puedes ajustar el modelo según la necesidad

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
write_csv(x = sub15, "C:/Users/windows/Documents/GitHub/Problem_Set_1/Curso-Big-Data/Taller 2/2.Entregables/Submission16.csv")



