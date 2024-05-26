#### Regresión líneal ####
set.seed(1234)
#Cargamos la data con las variables construidas para el modelo
library(devtools)
library(caret)
library(tidyverse)
train<-read_csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/Taller_3/train_join2.csv")
test<-read_csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/Taller_3/test_join2.csv")

# Procesar datos de entrenamiento
train <- train %>% mutate(estrato = as.numeric(substr(estrato_predominante, 8, nchar(estrato_predominante))))
test <- test %>% mutate(estrato = as.numeric(substr(estrato_predominante, 8, nchar(estrato_predominante))))

train<-train %>% filter(operation_type=="Venta")

#Modelos####

# Crear folds
sectores <- unique(train$cod_sector)
folds <- createFolds(sectores, k = length(sectores), list = TRUE, returnTrain = TRUE)

#Modelo1
# Variables predictoras
predictores_modelo_1 <- c("nbanios", "nhabitaciones", "piso_apartamento", "estrato")


# Función de validación cruzada
# Función de validación cruzada y ajuste de modelos en cada fold
cv_modelo <- function(fold) {
  train_data <- train %>% filter(cod_sector %in% fold)
  test_data <- train %>% filter(!cod_sector %in% fold)
  
  # Entrenar el modelo
  modelo <- lm(as.formula(paste("precio_2019 ~", paste(predictores_modelo_1, collapse = " + "))), data = train_data)
  
  # Hacer predicciones
  predicciones <- predict(modelo, newdata = test_data)
  
  # Calcular MAE en el conjunto de prueba
  MAE <- mean(abs(test_data$precio_2019 - predicciones))
  
  return(list(MAE = MAE, coef = coef(modelo)))
}

# Ejecutar validación cruzada y recolectar resultados
resultados_cv <- lapply(folds, function(fold) cv_modelo(train$cod_sector[fold]))

# Calcular el MAE promedio
MAE_lm_1 <- mean(sapply(resultados_cv, function(res) res$MAE))
print(MAE_lm_1)

# Promediar los coeficientes de los modelos ajustados en cada fold
coeficientes <- do.call(rbind, lapply(resultados_cv, function(res) res$coef))
coeficientes_promediados <- colMeans(coeficientes)

print(coeficientes_promediados)

formula_modelo <- as.formula(paste("precio_2019 ~", paste(predictores_modelo_1, collapse = " + ")))

# Ajustar el modelo final usando todos los datos de entrenamiento
modelo_final <- lm(formula_modelo, data = train)

# Reemplazar los coeficientes del modelo final con los coeficientes promediados
modelo_final$coefficients <- coeficientes_promediados

# Imprimir el resumen del modelo final con coeficientes promediados
print(summary(modelo_final))


#predecir sobre toda la muestra
train$precio_2019_lm_1<-predict(modelo_final,newdata = train)
train$precio_lm_1<-train$precio_2019_lm_1*train$inflacion_acum

#MAE dentro de muestra
MAE_lm_1_insample<- mean(abs(train$precio_2019 - train$precio_lm_1))
print(MAE_lm_1_insample)

#predecir fuera de muestra
test$precio_lm_1<-predict(modelo_final,newdata = test)

summary(test$precio_lm_1)

sub_lm_1<-test %>% select(property_id,precio_lm_1)

table(is.na(sub_lm_1$property_id))
table(is.na(sub_lm_1$precio_lm_1))

sub_lm_1<-sub_lm_1 %>% rename(price=precio_lm_1)

write_csv(x = sub_lm_1,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/2.Entregables/Submit/Submit_linear_model_1.csv",)
prueba<-read.csv("C:/Users/HP-Laptop/Downloads/submission_template.csv")
######
test$precio_lm<-predict(modelo,newdata=test)

sub_lm<-test %>% select(id,precio_lm)
table(is.na(sub_lm$precio_lm))
table(is.na(sub_lm$id))
      
write_csv(x = sub9,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/2.Entregables/Submission linear regression.csv",)


#Modelo 2
# Variables predictoras
predictores_modelo_2 <- c("nbanios", "nhabitaciones", "piso_apartamento", "estrato")


# Función de validación cruzada
# Función de validación cruzada y ajuste de modelos en cada fold
cv_modelo2 <- function(fold) {
  train_data <- train %>% filter(cod_sector %in% fold)
  test_data <- train %>% filter(!cod_sector %in% fold)
  
  # Entrenar el modelo
  modelo <- lm(as.formula(paste("log(price) ~", paste(predictores_modelo_2, collapse = " + "))), data = train_data)
  
  # Hacer predicciones
  predicciones <- predict(modelo, newdata = test_data)
  
  # Calcular MAE en el conjunto de prueba
  MAE <- mean(abs(test_data$price - exp(predicciones)))
  
  return(list(MAE = MAE, coef = coef(modelo)))
}

# Ejecutar validación cruzada y recolectar resultados
resultados_cv2 <- lapply(folds, function(fold) cv_modelo2(train$cod_sector[fold]))

# Calcular el MAE promedio
MAE_lm_2 <- mean(sapply(resultados_cv2, function(res) res$MAE))
print(MAE_lm_2)

# Promediar los coeficientes de los modelos ajustados en cada fold
coeficientes2 <- do.call(rbind, lapply(resultados_cv2, function(res) res$coef))
coeficientes_promediados2 <- colMeans(coeficientes2)

print(coeficientes_promediados2)

formula_modelo2 <- as.formula(paste("log(price) ~", paste(predictores_modelo_1, collapse = " + ")))

# Ajustar el modelo final usando todos los datos de entrenamiento
modelo_final2 <- lm(formula_modelo2, data = train)

# Reemplazar los coeficientes del modelo final con los coeficientes promediados
modelo_final2$coefficients <- coeficientes_promediados2

# Imprimir el resumen del modelo final con coeficientes promediados
print(summary(modelo_final2))


#predecir sobre toda la muestra
train$precio_lm_2<-predict(modelo_final2,newdata = train)
train$precio_lm_2<-exp(train$precio_2019_lm_2)

#MAE dentro de muestra
MAE_lm_2_insample<- mean(abs(train$price- train$precio_lm_2))
print(MAE_lm_2_insample)

#predecir fuera de muestra #########################################################
test$precio_lm_2<-predict(modelo_final2,newdata = test)
test$precio_lm_2<-exp(test$precio_lm_2)
summary(test$precio_lm_2)

sub_lm_2<-test %>% select(property_id,precio_lm_2)

table(is.na(sub_lm_2$property_id))
table(is.na(sub_lm_2$precio_lm_2))

sub_lm_2<-sub_lm_2 %>% rename(price=precio_lm_2)

write_csv(x = sub_lm_2,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/2.Entregables/Submit/Submit_linear_model_2.csv",)
prueba<-read.csv("C:/Users/HP-Laptop/Downloads/submission_template.csv")
######
test$precio_lm<-predict(modelo,newdata=test)

sub_lm<-test %>% select(id,precio_lm)
table(is.na(sub_lm$precio_lm))
table(is.na(sub_lm$id))

write_csv(x = sub9,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/2.Entregables/Submission linear regression.csv",)


#Modelo 3###################
table(train$Periodo)
table(test$Periodo)
str(test$Periodo)

test<-test %>% mutate(Periodo=as.numeric(Periodo.x))

# Variables predictoras
predictores_modelo_3 <- c("nbanios", "nhabitaciones", "piso_apartamento", "estrato","Periodo")


# Función de validación cruzada
# Función de validación cruzada y ajuste de modelos en cada fold
cv_modelo3 <- function(fold) {
  train_data <- train %>% filter(cod_sector %in% fold)
  test_data <- train %>% filter(!cod_sector %in% fold)
  
  # Entrenar el modelo
  modelo <- lm(as.formula(paste("log(price) ~", paste(predictores_modelo_3, collapse = " + "))), data = train_data)
  
  # Hacer predicciones
  predicciones <- predict(modelo, newdata = test_data)
  
  # Calcular MAE en el conjunto de prueba
  MAE <- mean(abs(test_data$price - exp(predicciones)))
  
  return(list(MAE = MAE, coef = coef(modelo)))
}

# Ejecutar validación cruzada y recolectar resultados
resultados_cv3 <- lapply(folds, function(fold) cv_modelo3(train$cod_sector[fold]))

# Calcular el MAE promedio
MAE_lm_3 <- mean(sapply(resultados_cv3, function(res) res$MAE))
print(MAE_lm_3)

# Promediar los coeficientes de los modelos ajustados en cada fold
coeficientes3 <- do.call(rbind, lapply(resultados_cv3, function(res) res$coef))
coeficientes_promediados3 <- colMeans(coeficientes3)

print(coeficientes_promediados3)

formula_modelo3 <- as.formula(paste("log(price) ~", paste(predictores_modelo_3, collapse = " + ")))

# Ajustar el modelo final usando todos los datos de entrenamiento
modelo_final3 <- lm(formula_modelo3, data = train)

# Reemplazar los coeficientes del modelo final con los coeficientes promediados
modelo_final3$coefficients <- coeficientes_promediados3

# Imprimir el resumen del modelo final con coeficientes promediados
print(summary(modelo_final3))


#predecir sobre toda la muestra
train$precio_lm_3<-predict(modelo_final3,newdata = train)
train$precio_lm_3<-exp(train$precio_lm_3)

#MAE dentro de muestra
MAE_lm_3_insample<- mean(abs(train$price- train$precio_lm_3))
print(MAE_lm_3_insample)

#predecir fuera de muestra #########################################################
test$precio_lm_3<-predict(modelo_final3,newdata = test)
test$precio_lm_3<-exp(test$precio_lm_3)
summary(test$precio_lm_3)

sub_lm_3<-test %>% select(property_id,precio_lm_3)

table(is.na(sub_lm_3$property_id))
table(is.na(sub_lm_3$precio_lm_3))

sub_lm_3<-sub_lm_3 %>% rename(price=precio_lm_3)

write_csv(x = sub_lm_3,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/2.Entregables/Submit/Submit_linear_model_3.csv",)
