# CODIGO1
set.seed(1234)
library(devtools)
library(randomForest)
library(caret)
library(smotefamily)
source_url("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%202/1.Procesamiento%20y%20sintaxis/Creaci%C3%B3n%20de%20variables%20de%20inter%C3%A9s.R")

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

# Resumen del modelo
print(pobre_smote_boost)
