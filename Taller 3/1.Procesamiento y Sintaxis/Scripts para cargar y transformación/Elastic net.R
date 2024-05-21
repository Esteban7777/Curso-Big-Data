####Elastic net####

set.seed(1234)

library(devtools)
library(caret)
library(glmnet)

train<-read_csv("ruta del train")
test<-read_csv("ruta del test")

#Creamos los folds
sectores<-unique(train$COD_Sector)
folds<-createFolds(sectores,k=length(sectores),list = TRUE,returnTrain = TRUE)


#Modelo 1

cv_modelo_en_1<-function(fold){
  train_data<-datos %>% filter(COD_Sector %in% fold) %>% select(price, predictores_modelo_1)
  test_data<- datos %>% filter(!COD_Sector %in% fold) %>% select(price, predictores_modelo_1)
  
  # Separar las características (X) y la variable objetivo (y)
  x_train <- model.matrix(price ~ . - 1, data = train_data)
  y_train <- train_data$price
  x_test <- model.matrix(price ~ . - 1, data = test_data)
  y_test <- test_data$price
  
  # Entrenar el modelo de Elastic Net
  cv_modelo <- cv.glmnet(x_train, y_train, alpha = 0.5)
  
  #predicción fuera de muestra
  prediccciones<-predict(modelo,newdata=test_data)
  
  MAE<-mean(abs(train$price-predicciones))
  
  return(MAE)
  
}

MAE<-mean(sapply(folds,cv_modelo_en_1))

#Modelo 2

cv_modelo_en_2<-function(fold){
  train_data<-datos %>% filter(COD_Sector %in% fold) %>% select(price, predictores_modelo_2)
  test_data<- datos %>% filter(!COD_Sector %in% fold) %>% select(price, predictores_modelo_2)
  
  # Separar las características (X) y la variable objetivo (y)
  x_train <- model.matrix(price ~ . - 1, data = train_data)
  y_train <- train_data$price
  x_test <- model.matrix(price ~ . - 1, data = test_data)
  y_test <- test_data$price
  
  # Entrenar el modelo de Elastic Net
  cv_modelo <- cv.glmnet(x_train, y_train, alpha = 0.5)
  
  #predicción fuera de muestra
  prediccciones<-predict(modelo,newdata=test_data)
  
  MAE<-mean(abs(train$price-predicciones))
  
  return(MAE)
  
}

MAE<-mean(sapply(folds,cv_modelo_en_1))

#Selección del mejor modelo
min(MAE_en_1,MAE_en_2)

#Predicciones

modelo<-function(fold){
  train_data<-datos %>% filter(COD_Sector %in% fold) %>% select(price, PREDICTORES DEL MEJOR MODELO)
  test_data<- datos %>% filter(!COD_Sector %in% fold) %>% select(price, PREDICTORES DEL MEJOR MODELO)
  
  # Separar las características (X) y la variable objetivo (y)
  x_train <- model.matrix(price ~ . - 1, data = train_data)
  y_train <- train_data$price
  x_test <- model.matrix(price ~ . - 1, data = test_data)
  y_test <- test_data$price
  
  # Entrenar el modelo de Elastic Net
  cv_modelo <- cv.glmnet(x_train, y_train, alpha = 0.5)
  
  #predicción fuera de muestra
  prediccciones<-predict(modelo,newdata=test_data)
  
  MAE<-mean(abs(train$price-predicciones))
  
  return(modelo)
  
}

test$precio_en<-predict(modelo,newdata=test)

sub_en<-test %>% select(id,precio_en)
table(is.na(sub_en$precio_en))
table(is.na(sub_en$id))
      
write_csv(x = sub_en,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/2.Entregables/Submission elastic net.csv",)