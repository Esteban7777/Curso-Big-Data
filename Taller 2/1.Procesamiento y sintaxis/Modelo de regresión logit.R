#Modelos Logit
set.seed(1234)
#Cargamos la data con las variables construidas para el modelo
library(devtools)
library(caret)
library(pacman)
library(glmnet)
source_url("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%202/1.Procesamiento%20y%20sintaxis/Creaci%C3%B3n%20de%20variables%20de%20inter%C3%A9s.R")

predictores_modelo3<-c("desempleo_jefe", #Completa
                        "educacion_jefe", #Completa 
                        "sexo_jefe", #Completa
                        "Clase", #Completa
                        "P5090", #Completa
                        "Ina_jefe", #Completa 
                        "Des_jefe", #Completa
                        "Oc_jefe" #Completa
)


m4<-glm(as.formula(
  paste("Pobre~",
        paste(predictores_modelo3, collapse = " + "))),data = train_hogares)

summary(m4)

train_hogares$m4_predict<-predict(m4,newdata = train_hogares)

matrix_predicciones4<-table(train_hogares$m4_predict,train_hogares$Pobre)
confusionMatrix(matrix_predicciones3)

#Fuera de muestra
test_hogares$m4_predict<-predict(object = m4,newdata = test_hogares)

table(is.na(test_hogares$m4_predict))
#Sigue saltando multicolinealidad.

#Se depuran variables
predictores_modelo4<-c("desempleo_jefe", #Completa
                        "educacion_jefe", #Completa 
                        "sexo_jefe", #Completa
                        "Clase", #Completa
                        "P5090" #Completa
)


m5<-glm(as.formula(
  paste("Pobre~",
        paste(predictores_modelo4, collapse = " + "))),data = train_hogares)

summary(m5)

train_hogares$m5_prob_predict<-predict(m5,newdata = train_hogares)
train_hogares$m5_predict<-ifelse(train_hogares$m5_prob_predict>0.5,1,0)

matrix_predicciones5<-table(train_hogares$m5_predict,train_hogares$Pobre)
confusionMatrix(matrix_predicciones5)

#Fuera de muestra
test_hogares$m5_prob_predict<-predict(object = m5,newdata = test_hogares)
test_hogares$m5_predict<-ifelse(test_hogares$m5_prob_predict>0.5,1,0)

table(is.na(test_hogares$m5_predict))

sub3<-test_hogares %>% select(id,m5_predict)
sub3<-sub3 %>% rename(pobre=m5_predict)
write_csv(x = sub3,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/2.Entregables/Submission3.csv",)


predictores_modelo5<-c("Clase","Des_jefe" #Completa 
)
table(train_hogares$Pobre,train_hogares$Des_jefe)

m6<-glm(as.formula(
  paste("Pobre~",
        paste(predictores_modelo5, collapse = " * "))),data = train_hogares)

summary(m6)

train_hogares$m6_prob_predict<-predict(m6,newdata = train_hogares)
train_hogares$m6_predict<-ifelse(train_hogares$m6_prob_predict>=0.5,1,0)

matrix_predicciones6<-table(train_hogares$m6_predict,train_hogares$Pobre)
confusionMatrix(matrix_predicciones6)

#Fuera de muestra
test_hogares$m6_prob_predict<-predict(object = m6,newdata = test_hogares)
test_hogares$m6_predict<-ifelse(test_hogares$m6_prob_predict>0.5,1,0)

table(is.na(test_hogares$m6_predict))

sub4<-test_hogares %>% select(id,m6_predict)
sub4<-sub4 %>% rename(pobre=m6_predict)
write_csv(x = sub4,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/2.Entregables/Submission4.csv",)

table(sub1$pobre)
table(sub2$pobre)
table(sub4$pobre)

#NOTA TOCA ENCONTRAR MAS De 3.000 POBRES SINO NO SUBIR

predictores_modelo7<-c("educacion_jefe",
                        "sexo_jefe",
                        "Personas_habitacion"#Completa 
)


table(test_hogares$sexo_jefe)
table(test_hogares$regimen_jefe)

m7<-glm(as.formula(
  paste("Pobre~",
        paste(predictores_modelo7, collapse = " * "))),data = train_hogares)

summary(m7)

train_hogares$m7_prob_predict<-predict(m7,newdata = train_hogares)
train_hogares$m7_predict<-ifelse(train_hogares$m7_prob_predict>=0.5,1,0)

matrix_predicciones7<-table(train_hogares$m7_predict,train_hogares$Pobre)
confusionMatrix(matrix_predicciones7)

#Fuera de muestra
test_hogares$m7_prob_predict<-predict(object = m7,newdata = test_hogares)
test_hogares$m7_predict<-ifelse(test_hogares$m7_prob_predict>0.5,1,0)

table(is.na(test_hogares$m7_predict))

sub5<-test_hogares %>% select(id,m7_predict)
sub5<-sub5 %>% rename(pobre=m7_predict)
write_csv(x = sub5,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/2.Entregables/Submission5.csv",)
table(sub1$pobre)

##
predictores_modelo8<-c(
  "regimen_subsidiado_jefe",#Completa
  "educacion_jefe", #Completa 
  "sexo_jefe", #Completa
  "Clase", #Completa
  "P5090", #Completa
  #Completa 
  "Des_jefe", #Completa
  "Personas_habitacion"#Completa 
)


table(test_hogares$sexo_jefe)
table(test_hogares)

m8<-glm(as.formula(
  paste("Pobre~",
        paste(predictores_modelo8, collapse = " * "))),data = train_hogares)

summary(m8)

train_hogares$m8_prob_predict<-predict(m8,newdata = train_hogares)
train_hogares$m8_predict<-ifelse(train_hogares$m8_prob_predict>=0.5,1,0)

matrix_predicciones8<-table(train_hogares$m8_predict,train_hogares$Pobre)
confusionMatrix(matrix_predicciones8)

#Fuera de muestra
test_hogares$m8_prob_predict<-predict(object = m8,newdata = test_hogares)
test_hogares$m8_predict<-ifelse(test_hogares$m8_prob_predict>0.5,1,0)

table(is.na(test_hogares$m8_predict))
table(test_hogares$m8_predict)

sub6<-test_hogares %>% select(id,m8_predict)
sub6<-sub6 %>% rename(pobre=m8_predict)
write_csv(x = sub6,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/2.Entregables/Submission6.csv",)

#Ajustes al modelo 

train_hogares$pobre_texto<-ifelse(train_hogares$Pobre==1,"Pobre","No_Pobre")
train_hogares$pobre_texto<-as.factor(train_hogares$pobre_texto)
str(train_hogares$pobre_texto)
table(train_hogares$pobre_texto,train_hogares$Pobre)

predictores_modelo9<-c(#"regimen_subsidiado_jefe",
                       "educacion_jefe", #Completa 
                       "sexo_jefe", #Completa
                       "Clase", #Completa
                       "P5090", #Completa
                       "Personas_habitacion",
                       "Des_jefe"#Completa
                       )

#Train control
train_control<-trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = defaultSummary,
  savePredictions = TRUE
  )

modelo9<-train(
  formula(paste0("pobre_texto ~",paste0(predictores_modelo, collapse = " + "))),
  method='glm',
  data=train_hogares,
  family="binomial",
  trControl=train_control
)

lambda <- 10^seq(-1, -4, length = 100)
grid <- expand.grid("alpha" = 1, lambda = lambda)

glm_model_lasso <- train(
  formula(paste0("pobre_texto~",paste0(predictores_modelo9, collapse = " + "))),
  method = "glmnet",
  data = train_hogares,
  family = "binomial",
  trControl = train_control,
  tuneGrid = grid,
  preProcess = c("center", "scale")
)

glm_model_lasso

head(modelo9$pred)

matrix_confusion<-table(glm_model_lasso$pred$pred,glm_model_lasso$pred$obs)
confusionMatrix(matrix_confusion,
                positive="Pobre", mode = "prec_recall")

confusionMatrix(data = glm_model_lasso$pred$pred,
                reference = glm_model_lasso$pred$obs,
                positive="Pobre", mode = "prec_recall")


