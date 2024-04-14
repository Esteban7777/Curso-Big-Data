set.seed(1234)
library(devtools)
library(randomForest)
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
str(train_hogares$Personas_habitacion_round)
#str(train_hogares$Pobre)
#Las variables de arriba son las que vienen como factor, se convirtieron una a una en número
train_hogares$Personas_habitacion_round<-as.numeric(train_hogares$Personas_habitacion_round)

ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)


smote_output <- SMOTE(X = train_hogares[predictores],
                      target = train_hogares$Pobre)

smote_data<-smote_output$data

table(smote_data$class)

smote_data$class <- as.factor(smote_data$class)

levels(smote_data$class) <- make.names(levels(smote_data$class))
table(smote_data$class)
 pobre_smote_rf<-train(
  class ~.,
  method="rf",
  trControl=ctrl,
  data = smote_data,
  family="binomial")

library(ranger)
 
smote_RF<- ranger(formula =class ~., 
            data = smote_data,
            num.trees= 500, ## Numero de arboles a estimar
            mtry= 4,   # N. var aleatoriamente seleccionadas en cada partición. 
            min.node.size  = 1, ## Numero minimo de observaciones en un nodo 
            importance="impurity") 

str(smote_data$class)
cf_rf_smote<-confusionMatrix(data = smote_RF$predictions,
                reference = smote_data$class,
                positive = "X1",
                mode = "prec_recall")

predic_smote_RF<-predict(smote_RF,test_hogares)

test_hogares$predict_smote_RF<-predic_smote_RF$predictions
table(test_hogares$predict_smote_RF)
#X1=pobre

sub13<-test_hogares %>% select(id,predict_smote_RF)
sub13<-sub13 %>% rename(pobre=predict_smote_RF)
sub13$pobre<-ifelse(sub13$pobre=="X1",1,0)
table(sub13$pobre)
table(test_hogares$predict_smote_RF)
write_csv(x = sub13,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/2.Entregables/Submission13.csv",)


###Xgboost
library(xgboost)
grid_xbgoost <- expand.grid(nrounds = c(250,500),
                            max_depth = c(1, 5),
                            eta = c(0.1,  0.01), 
                            gamma = c(0, 1), 
                            min_child_weight = c(10, 25),
                            colsample_bytree = c(0.4, 0.7), 
                            subsample = c(0.7))
Xgboost_tree <- train(class~.,
                      data = smote_data, 
                      method = "xgbTree", 
                      trControl = ctrl,
                      tuneGrid=grid_xbgoost,
                      metric = "ROC",
                      verbosity = 0)   

Xgboost_pred<-Xgboost_tree$pred

cf_xgb_smote<-confusionMatrix(data = Xgboost_pred$pred,
                             reference = Xgboost_pred$obs,
                             positive = "X1",
                             mode = "prec_recall")

cf_xgb_smote

test_hogares$informalidad_jefe<-as.numeric(test_hogares$informalidad_jefe)
test_hogares$sexo_jefe<-as.numeric(test_hogares$sexo_jefe)
test_hogares$Clase<-as.numeric(test_hogares$Clase)
test_hogares$tipo_casa<-as.numeric(test_hogares$tipo_casa)
test_hogares$edad_jefe_joven<-as.numeric(test_hogares$edad_jefe_joven)
test_hogares$Personas_habitacion_round<-as.numeric(test_hogares$Personas_habitacion_round)

test_hogares$Personas_habitacion_round<-ifelse(test_hogares$Personas_habitacion<=2.5,1,0)

#Identificar un NA
table(is.na(test_hogares$informalidad_jefe))

test_hogares$sexo_jefe<-as.numeric(test_hogares$sexo_jefe)
test_hogares$Clase<-as.numeric(test_hogares$Clase)
test_hogares$tipo_casa<-as.numeric(test_hogares$tipo_casa)
test_hogares$edad_jefe_joven<-as.numeric(test_hogares$edad_jefe_joven)
test_hogares$Personas_habitacion_round<-as.numeric(test_hogares$Personas_habitacion_round)


table(is.na(test_hogares$desempleo_jefe))

table(is.na(test_hogares$ocupacion_jefe))
table(test_hogares$ocupacion_jefe)
respaldo<-test_hogares$ocupacion_jefe
test_hogares$ocupacion_jefe<-ifelse(is.na(test_hogares$ocupacion_jefe),sample(c(1,2,3,4,5,6),1),test_hogares$ocupacion_jefe)

table(is.na(test_hogares$ocupacion_jefe))
table(test_hogares$desempleo_jefe)
respaldo<-test_hogares$desempleo_jefe
test_hogares$desempleo_jefe<-ifelse(is.na(test_hogares$desempleo_jefe),sample(c(0,1),1),test_hogares$desempleo_jefe)


predic_smote_xgboost<-predict(Xgboost_tree,newdata = test_hogares)

table(predic_smote_xgboost)

length(predic_smote_xgboost)

test_hogares$predic_xgboost<-predic_smote_xgboost

sub14<-test_hogares %>% select(id,predic_xgboost)
sub14<-sub14 %>% rename(pobre=predic_xgboost)
sub14$pobre<-ifelse(sub14$pobre=="X1",1,0)
table(sub14$pobre)
table(test_hogares$predic_xgboost)
write_csv(x = sub14,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/2.Entregables/Submission14.csv",)

table(test_hogares$predic_xgboost,predict_smote_RF)