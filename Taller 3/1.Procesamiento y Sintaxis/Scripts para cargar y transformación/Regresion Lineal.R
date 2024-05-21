#### Regresión líneal ####
set.seed(1234)
#Cargamos la data con las variables construidas para el modelo
library(devtools)
library(caret)
train<-read_csv("ruta del train")
test<-read_csv("ruta del test")

#Creamos los folds
sectores<-unique(train$COD_Sector)
folds<-createFolds(sectores,k=length(sectores),list = TRUE,returnTrain = TRUE)

#Modelos####

#Modelo 1
names(train)
predictores_modelo_1<-names(train)

cv_modelo<-function(fold){
  train_data<-datos %>% filter(COD_Sector %in% fold)
  test_data<- datos %>% filter(!COD_Sector %in% fold)
  modelo<-lm("price~",
             paste(predictores_modelo_1, collapse = " + "), data=train)
  prediccciones<-predict(modelo,newdata=test_data)
  
  MAE<-mean(abs(train$price-train$precio_lm_1))
  
  return(MAE)
  
}

MAE_lm_1<-mean(sapply(folds,cv_modelo))

#Modelo 2

names(train)
predictores_modelo_2<-names(train)

cv_modelo<-function(fold){
  train_data<-datos %>% filter(COD_Sector %in% fold)
  test_data<- datos %>% filter(!COD_Sector %in% fold)
  modelo<-lm("price~",
             paste(predictores_modelo_2, collapse = " + "), data=train)
  prediccciones<-predict(modelo,newdata=test_data)
  
  MAE<-mean(abs(train$price-train$precio_lm_1))
  
  return(MAE)
  
}

MAE_lm_2<-mean(sapply(folds,cv_modelo))



#Selección del mejor modelo

min(MAE_lm_1,MAE_lm_2)

