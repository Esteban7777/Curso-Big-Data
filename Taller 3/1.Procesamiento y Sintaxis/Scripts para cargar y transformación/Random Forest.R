set.seed(1234)
library(tidyverse)
library(devtools)
library(randomForest)
library(caret)
library(smotefamily)
train<-read.csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/Taller_3/train_join2.csv")
test<-read.csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/Taller_3/test_join2.csv")


#Predictores
train <- train %>% mutate(estrato = as.numeric(substr(estrato_predominante, 8, nchar(estrato_predominante))))
test <- test %>% mutate(estrato = as.numeric(substr(estrato_predominante, 8, nchar(estrato_predominante))))

predictores_modelo_rf_1 <- c("nbanios","nhabitaciones","piso_apartamento","estrato","Periodo")


library(ranger)

modelo_rf<- ranger(formula = as.formula(paste("price~",
                                       paste(predictores_modelo_rf_1, collapse = " + "))), 
            data = train,
            num.trees= 500, ## Numero de bootstrap samples y arboles a estimar. Default 500  
            mtry= 4,   # N. var aleatoriamente seleccionadas en cada partici?n. Baggin usa todas las vars.
            min.node.size  = 1, ## Numero minimo de observaciones en un nodo para intentar 
            ) 

#Predicci?n dentro de muestra

X<-train %>% select(predictores_modelo_rf_1)

precio_rf_1<-predict(modelo_rf,X)

precio_rf_1$predictions

train$precio_rf_1<-precio_rf_1$predictions


table(is.na(train$precio_rf_1))
str(train$precio_rf_1)

MAE_rf_1_insample <- mean(abs(train$price -train$precio_rf_1))

#Fuera de muestra
names(test)

inflacion<-read.csv2("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/Inflacion.csv")

str(inflacion$Mes)
inflacion<-inflacion %>% rename(fecha=Mes)

test<-test %>%
  mutate(fecha=as.character(ifelse(month<10,
                                   paste0("01/0",month,"/",year),
                                   paste0("01/",month,"/",year))))

test<-test %>% left_join(inflacion,by="fecha")

table(is.na(test$cod_sector))

X_test<-test %>% select(predictores_modelo_rf_1)

precio_rf_1_test<-predict(modelo_rf,X_test)

precio_rf_1_test$predictions

test$precio_rf_1<-precio_rf_1_test$predictions


table(is.na(test$precio_rf_1))
summary(test$precio_rf_1)
str(train$precio_rf_1)



#Exportar el submition
sub_rf_1<-test %>% select(property_id,precio_rf_1)
table(is.na(sub_rf_1$precio_rf_1))
table(is.na(sub_rf_1$property_id))

sub_rf_1<-sub_rf_1 %>% mutate(price=precio_rf_1)%>% select(property_id,price)

write_csv(x = sub_rf_1,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/2.Entregables/Submit/Submit_rf_1.csv",)


write_csv(x = sub_rf_1,"C:/Users/AguirreW/Desktop/Submit_rt_1.csv",)


#Incluyendo variables de seguridad

#En train
seguridad_train<-read.csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/Taller_3/seguridad.csv")
train<-cbind(train,seguridad_train)

#En test
seguridad_test<-read.csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/Taller_3/seguridad_test.csv")
names(seguridad_test) <- c("Robos_vivienda", "Robos_personas", "distancia_estacion_policia","geometry","geometry2")


test<-cbind(test,seguridad_test)
names(seguridad_test)


#Nuevos predictores

predictores_modelo_rf_2 <- c("nbanios","nhabitaciones","piso_apartamento",
                             "estrato","Periodo","Robos_vivienda","Robos_personas",
                             "distancia_estacion_policia")




modelo_rf_2<- ranger(formula = as.formula(paste("price~",
                                              paste(predictores_modelo_rf_2, collapse = " + "))), 
                   data = train,
                   num.trees= 500, ## Numero de bootstrap samples y arboles a estimar. Default 500  
                   mtry= 4,   # N. var aleatoriamente seleccionadas en cada partici?n. Baggin usa todas las vars.
                   min.node.size  = 1, ## Numero minimo de observaciones en un nodo para intentar 
) 

#Predicci?n dentro de muestra

X<-train %>% select(predictores_modelo_rf_2)

precio_rf_2<-predict(modelo_rf_2,X)

precio_rf_2$predictions

train$precio_rf_2<-precio_rf_2$predictions


table(is.na(train$precio_rf_2))
str(train$precio_rf_2)

MAE_rf_2_insample <- mean(abs(train$price -train$precio_rf_2))

##
X_test<-test %>% select(predictores_modelo_rf_2)

precio_rf_2_test<-predict(modelo_rf_2,X_test)

precio_rf_2_test$predictions

test$precio_rf_2<-precio_rf_2_test$predictions


table(is.na(test$precio_rf_2))
summary(test$precio_rf_2)
str(train$precio_rf_2)



#Exportar el submition
sub_rf_2<-test %>% select(property_id,precio_rf_2)
table(is.na(sub_rf_2$precio_rf_2))
table(is.na(sub_rf_2$property_id))

sub_rf_2<-sub_rf_2 %>% mutate(price=precio_rf_2)%>% select(property_id,price)
summary(sub_rf_2$price)

write_csv(x = sub_rf_2,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/2.Entregables/Submit/Submit_rf_2.csv",)




#Entrenamos el mismo modelo con el precio en logaritmos
train<-train %>% mutate(precio_log=log(price))


predictores_modelo_rf_3 <- c("nbanios","nhabitaciones","piso_apartamento",
                             "estrato","Periodo","Robos_vivienda","Robos_personas",
                             "distancia_estacion_policia")



modelo_rf_3<- ranger(formula = as.formula(paste("precio_log~",
                                                paste(predictores_modelo_rf_3, collapse = " + "))), 
                     data = train,
                     num.trees= 500, ## Numero de bootstrap samples y arboles a estimar. Default 500  
                     mtry= 4,   # N. var aleatoriamente seleccionadas en cada partici?n. Baggin usa todas las vars.
                     min.node.size  = 1, ## Numero minimo de observaciones en un nodo para intentar 
) 

#Predicci?n dentro de muestra

X<-train %>% select(predictores_modelo_rf_3)

log_precio_rf_3<-predict(modelo_rf_3,X)

log_precio_rf_3$predictions

train$log_precio_rf_3<-log_precio_rf_3$predictions

table(is.na(train$log_precio_rf_3))
str(train$log_precio_rf_3)

train$precio_rf_3<-exp(train$log_precio_rf_3)

MAE_rf_3_insample <- mean(abs(train$price -train$precio_rf_3))

##
X_test<-test %>% select(predictores_modelo_rf_3)

log_precio_rf_3_test<-predict(modelo_rf_3,X_test)

log_precio_rf_3_test$predictions

test$log_precio_rf_3<-log_precio_rf_3_test$predictions

table(is.na(test$log_precio_rf_3))

test$precio_rf_3<-exp(test$log_precio_rf_3)

summary(test$precio_rf_3)
str(train$precio_rf_3)



#Exportar el submition
sub_rf_3<-test %>% select(property_id,precio_rf_3)
table(is.na(sub_rf_3$precio_rf_3))
table(is.na(sub_rf_3$property_id))

sub_rf_3<-sub_rf_3 %>% mutate(price=precio_rf_3)%>% select(property_id,price)
summary(sub_rf_3$price)

write_csv(x = sub_rf_3,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/2.Entregables/Submit/Submit_rf_3.csv",)



#Utilizando cross-validation con Caret

folds <- createFolds(train$cod_sector, k = length(unique(train$cod_sector)))

#Predictores
predictores_modelo_rf_4 <- c("nbanios","nhabitaciones","piso_apartamento",
                             "estrato","Periodo","Robos_vivienda","Robos_personas",
                             "distancia_estacion_policia")
# Configurar la validación cruzada con caret
train_control <- trainControl(method = "cv", index = folds)


tune_grid <- expand.grid(.mtry = sqrt(ncol(train) - 2))


modelo_rf_4 <- train(as.formula(paste("price ~",paste(predictores_modelo_rf_4,collapse = "+"))), data = train, method = "rf", 
               trControl = train_control, tuneGrid = tune_grid, ntree = 500)


train$precio_rf_4 <- predict(modelo_rf_4, newdata = train)

MAE_rf_4_insample <- mean(abs(train$price -train$precio_rf_4))

summary(train$precio_rf_4)

#Predicción fuera de muestra

test$precio_rf_4 <- predict(modelo_rf_4, newdata = test)

table(is.na(test$precio_rf_4))

summary(test$precio_rf_4)


#Exportar predicción
sub_rf_4<-test %>% select(property_id,precio_rf_4)
table(is.na(sub_rf_4$precio_rf_4))
table(is.na(sub_rf_4$property_id))

sub_rf_4<-sub_rf_4 %>% mutate(price=precio_rf_4)%>% select(property_id,price)
summary(sub_rf_4$price)

write_csv(x = sub_rf_4,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 3/2.Entregables/Submit/Submit_rf_4.csv",)

