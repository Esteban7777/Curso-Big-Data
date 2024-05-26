####Elastic net####

set.seed(1234)

library(devtools)
library(caret)
library(glmnet)
library(tidyverse)
train<-read_csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/Taller_3/train_join2.csv")
test<-read_csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/Taller_3/test_join2.csv")

#Ajustamos estrato
train <- train %>% mutate(estrato = as.numeric(substr(estrato_predominante, 8, nchar(estrato_predominante))))
test <- test %>% mutate(estrato = as.numeric(substr(estrato_predominante, 8, nchar(estrato_predominante))))

#Ajustamos el periodo
inflacion<-read.csv2("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/Inflacion.csv")
inflacion<-inflacion %>% rename(fecha=Mes)
test<-test %>% mutate(fecha= ifelse(month<10,paste0("01/0",month,"/",year),paste0("01/",month,"/",year)))
names(test)[duplicated(names(test))] <- paste0(names(test)[duplicated(names(test))], "_duplicate")


test<-test %>% left_join(inflacion,by="fecha")
table(is.na(test$Periodo))

#Creamos los folds
sectores<-unique(train$cod_sector)
folds<-createFolds(sectores,k=length(sectores),list = TRUE,returnTrain = TRUE)



#Modelo1
# Variables predictoras
predictores_modelo_en_1 <- c("nbanios", "nhabitaciones", "piso_apartamento", "estrato","Periodo")


# Función de validación cruzada
# Función de validación cruzada y ajuste de modelos en cada fold
cv_modelo_en_1 <- function(fold) {
  train_data <- train %>% filter(cod_sector %in% fold)  %>% select(price, predictores_modelo_en_1)
  test_data <- train %>% filter(!cod_sector %in% fold)  %>% select(price, predictores_modelo_en_1)
  
  # Separar las características (X) y la variable objetivo (y)
  x_train <- model.matrix(price ~ . - 1, data = train_data)
  y_train <- train_data$price
  x_test <- model.matrix(price ~ . - 1, data = test_data)
  y_test <- test_data$price
  
  
  # Entrenar el modelo
  modelo <- cv.glmnet(x = x_train, y = y_train, alpha = 0.5)
  
  # Hacer predicciones
  predicciones <- predict(object = modelo, s=modelo$lambda.min ,newx = x_test)
  
  # Calcular MAE en el conjunto de prueba
  MAE <- mean(abs(test_data$price - predicciones))
  
  return(list(MAE = MAE, coef=as.vector(coef(modelo,s="lambda.min"))))
}

# Ejecutar validación cruzada y recolectar resultados
resultados_cv_en_1 <- lapply(folds, function(fold) cv_modelo_en_1(train$cod_sector[fold]))

# Calcular el MAE promedio
MAE_en_1 <- mean(sapply(resultados_cv_en_1, function(res) res$MAE))
print(MAE_en_1)

coeficientes_en_1 <- do.call(rbind, lapply(resultados_cv_en_1, function(res) res$coef))
coeficientes_promediados_en_1 <- colMeans(coeficientes_en_1)

print(coeficientes_promediados_en_1)

x_full <- model.matrix(price ~ . - 1, data = train %>% select(price, all_of(predictores_modelo_en_1)))
y_full <- train$price

# Entrenar el modelo final Elastic Net con todos los datos
modelo_final <- cv.glmnet(x = x_full, y = y_full, alpha = 0.5)

# Imprimir los coeficientes del modelo final usando la lambda óptima
coef_final <- coef(modelo_final, s = modelo_final$lambda.min)
print(coef_final)

# Crear una nueva lista de coeficientes para el modelo final
coef_final <- coef(modelo_final, s = 0.1)
coef_final@x <- coeficientes_promediados

# Imprimir el resumen del modelo final
print(summary(modelo_final))

#predecir sobre toda la muestra
train$precio_en_1<-predict(modelo_final,newx = x_full)

#MAE dentro de muestra
MAE_en_1_insample<- mean(abs(train$price - train$precio_en_1))
print(MAE_en_1_insample)

#predecir fuera de muestra
test<-test %>% rename(Periodo=Periodo.x)

x_full_test <- test %>% select(predictores_modelo_en_1)
x_full_test<-as.matrix(x_full_test)
test$precio_en_1<-predict(modelo_final,newx = x_full_test)

summary(test$precio_en_1)

sub_en_1<-test %>% select(property_id,precio_en_1)

table(is.na(sub_en_1$property_id))
table(is.na(sub_en_1$precio_en_1))

sub_en_1<-sub_en_1 %>% mutate(price=as.vector(precio_en_1))%>% select(property_id,price)

write_csv(x = sub_en_1,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/2.Entregables/Submit/Submit_elastic_net_1.csv",)



#Modelo 2 

#Introducimos variables de crimen
crimen_train<-read.csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/Taller_3/seguridad.csv")
crimen_test<-read.csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/Taller_3/seguridad_test.csv")

train<-cbind(train,crimen_train)

#Ajustamos crimen test
names(crimen_test) <- c("Robos_vivienda", "Robos_personas", "distancia_estacion_policia","geometry","geometry2")
test<-cbind(test,crimen_test)

#Modelo
# Variables predictoras
predictores_modelo_en_2 <- c("nbanios", "nhabitaciones","piso_apartamento", "estrato","Periodo",
                             "Robos_vivienda","Robos_personas","distancia_estacion_policia")



# Función de validación cruzada
# Función de validación cruzada y ajuste de modelos en cada fold
cv_modelo_en_2 <- function(fold) {
  train_data <- train %>% filter(cod_sector %in% fold)  %>% select(price, predictores_modelo_en_2)
  test_data <- train %>% filter(!cod_sector %in% fold)  %>% select(price, predictores_modelo_en_2)
  
  # Separar las características (X) y la variable objetivo (y)
  x_train <- model.matrix(price ~ . - 1, data = train_data)
  y_train <- train_data$price
  x_test <- model.matrix(price ~ . - 1, data = test_data)
  y_test <- test_data$price
  
  
  # Entrenar el modelo
  modelo <- cv.glmnet(x = x_train, y = y_train, alpha = 0.5)
  
  # Hacer predicciones
  predicciones <- predict(object = modelo, s=modelo$lambda.min ,newx = x_test)
  
  # Calcular MAE en el conjunto de prueba
  MAE <- mean(abs(test_data$price - predicciones))
  
  return(list(MAE = MAE, coef=as.vector(coef(modelo,s="lambda.min"))))
}

# Ejecutar validación cruzada y recolectar resultados
resultados_cv_en_2 <- lapply(folds, function(fold) cv_modelo_en_2(train$cod_sector[fold]))

# Calcular el MAE promedio
MAE_en_2 <- mean(sapply(resultados_cv_en_2, function(res) res$MAE))
print(MAE_en_2)

coeficientes_en_2 <- do.call(rbind, lapply(resultados_cv_en_2, function(res) res$coef))
coeficientes_promediados_en_2 <- colMeans(coeficientes_en_2)

print(coeficientes_promediados_en_2)

x_full <- model.matrix(price ~ . - 1, data = train %>% select(price, all_of(predictores_modelo_en_2)))
y_full <- train$price

# Entrenar el modelo final Elastic Net con todos los datos
modelo_final <- cv.glmnet(x = x_full, y = y_full, alpha = 0.5)

# Imprimir los coeficientes del modelo final usando la lambda óptima
coef_final <- coef(modelo_final, s = modelo_final$lambda.min)
print(coef_final)

# Crear una nueva lista de coeficientes para el modelo final
coef_final <- coef(modelo_final, s = 0.1)
coef_final@x <- coeficientes_promediados_en_2

train$precio_en_2<-predict(modelo_final,newx = x_full)

#MAE dentro de muestra
MAE_en_2_insample<- mean(abs(train$price - train$precio_en_2))
print(MAE_en_2_insample)

#predecir fuera de muestra
x_full_test <- test %>% select(predictores_modelo_en_2)
x_full_test<-as.matrix(x_full_test)
test$precio_en_2<-predict(modelo_final,newx = x_full_test)

summary(test$precio_en_2)

sub_en_2<-test %>% select(property_id,precio_en_2)

table(is.na(sub_en_2$property_id))
table(is.na(sub_en_2$precio_en_2))

sub_en_2<-sub_en_2 %>% mutate(price=as.vector(precio_en_2))%>% select(property_id,price)

summary(sub_en_2$price)

write_csv(x = sub_en_2,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/2.Entregables/Submit/Submit_elastic_net_2.csv",)

#Modelo 3 
#Con el precio en logaritmos
train<-train %>% mutate(log_precio=log(price))


# Variables predictoras
predictores_modelo_en_3 <- c("nbanios", "nhabitaciones","piso_apartamento", "estrato","Periodo",
                             "Robos_vivienda","Robos_personas","distancia_estacion_policia")



# Función de validación cruzada
# Función de validación cruzada y ajuste de modelos en cada fold
cv_modelo_en_3 <- function(fold) {
  train_data <- train %>% filter(cod_sector %in% fold)  %>% select(log_precio, predictores_modelo_en_3)
  test_data <- train %>% filter(!cod_sector %in% fold)  %>% select(log_precio, predictores_modelo_en_3)
  
  # Separar las características (X) y la variable objetivo (y)
  x_train <- model.matrix(log_precio ~ . - 1, data = train_data)
  y_train <- train_data$log_precio
  x_test <- model.matrix(log_precio ~ . - 1, data = test_data)
  y_test <- test_data$log_precio
  
  
  # Entrenar el modelo
  modelo <- cv.glmnet(x = x_train, y = y_train, alpha = 0.5)
  
  # Hacer predicciones
  predicciones <- predict(object = modelo, s=modelo$lambda.min ,newx = x_test)
  
  # Calcular MAE en el conjunto de prueba
  MAE <- mean(abs(test_data$log_precio - predicciones))
  
  return(list(MAE = MAE, coef=as.vector(coef(modelo,s="lambda.min"))))
}

# Ejecutar validación cruzada y recolectar resultados
resultados_cv_en_3 <- lapply(folds, function(fold) cv_modelo_en_3(train$cod_sector[fold]))

# Calcular el MAE promedio
MAE_en_3 <- mean(sapply(resultados_cv_en_3, function(res) res$MAE))
print(MAE_en_3)

coeficientes_en_3 <- do.call(rbind, lapply(resultados_cv_en_3, function(res) res$coef))
coeficientes_promediados_en_3 <- colMeans(coeficientes_en_3)

print(coeficientes_promediados_en_3)

x_full <- model.matrix(log_precio ~ . - 1, data = train %>% select(log_precio, all_of(predictores_modelo_en_3)))
y_full <- train$log_precio

# Entrenar el modelo final Elastic Net con todos los datos
modelo_final <- cv.glmnet(x = x_full, y = y_full, alpha = 0.5)

# Imprimir los coeficientes del modelo final usando la lambda óptima
coef_final <- coef(modelo_final, s = modelo_final$lambda.min)
print(coef_final)

# Crear una nueva lista de coeficientes para el modelo final
coef_final <- coef(modelo_final, s = 0.1)
coef_final@x <- coeficientes_promediados_en_3

train$precio_en_3<-exp(predict(modelo_final,newx = x_full))

#MAE dentro de muestra
MAE_en_3_insample<- mean(abs(train$price - train$precio_en_3))
print(MAE_en_3_insample)

#predecir fuera de muestra
x_full_test <- test %>% select(predictores_modelo_en_3)
x_full_test<-as.matrix(x_full_test)
test$precio_en_3<-exp(predict(modelo_final,newx = x_full_test))

summary(test$precio_en_3)

sub_en_3<-test %>% select(property_id,precio_en_3)

table(is.na(sub_en_3$property_id))
table(is.na(sub_en_3$precio_en_2))

sub_en_3<-sub_en_3 %>% mutate(price=as.vector(precio_en_3))%>% select(property_id,price)

summary(sub_en_3$price)

write_csv(x = sub_en_3,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/2.Entregables/Submit/Submit_elastic_net_3.csv",)

#Modelo 4
#Con Caret
predictores_modelo_en_4 <- c("nbanios", "nhabitaciones","piso_apartamento", "estrato","Periodo",
                             "Robos_vivienda","Robos_personas","distancia_estacion_policia")

train_control <- trainControl(method = "cv", index = folds)

# Configurar el grid de hiperparámetros
tune_grid <- expand.grid(.alpha = seq(0, 1, length = 10), 
                         .lambda = 10^seq(-3, 3, length = 100))

# Entrenar el modelo Elastic Net
modelo_en_4 <- train(as.formula(paste("price ~", paste(predictores_modelo_en_4, collapse = "+"))), 
                            data = train, 
                            method = "glmnet", 
                            trControl = train_control, 
                            tuneGrid = tune_grid)


train$precio_en_4<-predict(modelo_en_4, newdata = train)

#MAE dentro de muestra
MAE_en_4_insample<- mean(abs(train$price - train$precio_en_4))
print(MAE_en_4_insample)


#predecir fuera de muestra
test$precio_en_4<-predict(modelo_en_4, newdata = test)

summary(test$precio_en_4)

sub_en_4<-test %>% select(property_id,precio_en_4)

table(is.na(sub_en_4$property_id))
table(is.na(sub_en_4$precio_en_4))

sub_en_4<-sub_en_4 %>% mutate(price=as.vector(precio_en_4))%>% select(property_id,price)

summary(sub_en_4$price)

write_csv(x = sub_en_4,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/2.Entregables/Submit/Submit_elastic_net_4.csv",)

