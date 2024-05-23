####Elastic net####

set.seed(1234)

library(devtools)
library(caret)
library(glmnet)

train<-read_csv("ruta del train")
test<-read_csv("ruta del test")

#Creamos los folds
sectores<-unique(train$cod_sector)
folds<-createFolds(sectores,k=length(sectores),list = TRUE,returnTrain = TRUE)


#Modelo 1
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
