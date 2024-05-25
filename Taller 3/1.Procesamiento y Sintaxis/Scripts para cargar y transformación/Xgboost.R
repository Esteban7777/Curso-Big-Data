#Xgboost

library(xgboost)
library(caret)
library(tidyverse)


set.seed(1234)

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


#Traemos las variables de crimen

crimen_train<-read.csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/Taller_3/seguridad.csv")
crimen_test<-read.csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/Taller_3/seguridad_test.csv")

train<-cbind(train,crimen_train)

#Ajustamos crimen test
names(crimen_test) <- c("Robos_vivienda", "Robos_personas", "distancia_estacion_policia","geometry","geometry2")
test<-cbind(test,crimen_test)


#Creamos los folds
sectores<-unique(train$cod_sector)
folds<-createFolds(sectores,k=length(sectores),list = TRUE,returnTrain = TRUE)

predictores_modelo_xgboost_1 <- c("nbanios", "nhabitaciones","piso_apartamento", "estrato","Periodo",
                              "Robos_vivienda","Robos_personas","distancia_estacion_policia")

#Modelo 1

grid_xbgoost <- expand.grid(nrounds = c(250),
                            max_depth = c(4), 
                            eta = c(0.01), 
                            gamma = c(0), 
                            min_child_weight = c(10, 25),
                            colsample_bytree = c(0.33,0.66),
                            subsample = c(0.4))

grid_xbgoost

train_control <- trainControl(method = "cv", index = folds)

Xgboost_tree <- train(as.formula(paste("price ~ ",paste(predictores_modelo_xgboost_1,collapse = "+"))),
                      data=train,
                      method = "xgbTree", 
                      trControl = train_control,
                      tuneGrid=grid_xbgoost
)        

Xgboost_tree

train$precio_xgboost_1 <- predict(Xgboost_tree, newdata = train)

MAE_xgboost_1_insample <- mean(abs(train$price -train$precio_xgboost_1))

summary(train$precio_xgboost_1)

#Predicción fuera de muestra

test$precio_xgboost_1 <- predict(Xgboost_tree, newdata = test)

table(is.na(test$precio_xgboost_1))

summary(test$precio_xgboost_1)


#Exportar predicción
sub_xgboost_1<-test %>% select(property_id,precio_xgboost_1)
table(is.na(sub_xgboost_1$precio_xgboost_1))
table(is.na(sub_xgboost_1$property_id))

sub_xgboost_1<-sub_xgboost_1 %>% mutate(price=precio_xgboost_1)%>% select(property_id,price)
summary(sub_xgboost_1$price)


write_csv(x = sub_xgboost_1,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/2.Entregables/Submit/Submit_xgboost_1.csv",)


