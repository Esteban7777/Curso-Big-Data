#Modelos aplicando Regularización
set.seed(1234)

library(devtools)
library(caret)
library(glmnet)

source_url("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%202/1.Procesamiento%20y%20sintaxis/Creaci%C3%B3n%20de%20variables%20de%20inter%C3%A9s.R")

train_hogares$sexo_jefe<-as.numeric(train_hogares$sexo_jefe)
train_hogares$sexo_jefe<-ifelse(train_hogares$sexo_jefe==1,1,0)
predictores_modelo3<-c("desempleo_jefe", #Completa
                        "educacion_jefe", #Completa 
                        "sexo_jefe", #Completa
                        "Clase", #Completa
                        "P5090", #Completa
                        "Ina_jefe", #Completa 
                        "Des_jefe", #Completa
                        "Oc_jefe" #Completa
)

X<-as.matrix(train_hogares[,predictores_modelo3])

str(train_hogares$sexo_jefe)
Y<-train_hogares[,"Ingtotugarr"]

#Ridge

cv_ridge <- cv.glmnet(x = X,
                      y = Y,alpha = 0) 
coef(cv_ridge, s = "lambda.min")

test_hogares$sexo_jefe<-as.numeric(test_hogares$sexo_jefe)
test_hogares$sexo_jefe<-ifelse(test_hogares$sexo_jefe==1,1,0)

X_test<-as.matrix(test_hogares[,predictores_modelo3])

test_hogares$predict_ingreso_rige<-predict(cv_ridge, newx = X_test, s = "lambda.min")

test_hogares$predict_rige<-ifelse(
  test_hogares$predict_ingreso_rige<test_hogares$Lp*test_hogares$Npersug,1,0)

table(test_hogares$predict_rige)

#Lasso

cv_lasso <- cv.glmnet(x = X,
                      y = Y,alpha = 1) 
coef(cv_lasso, s = "lambda.min")


X_test<-as.matrix(test_hogares[,predictores_modelo3])

test_hogares$predict_ingreso_lasso<-predict(cv_lasso, newx = X_test, s = "lambda.min")

test_hogares$predict_lasso<-ifelse(
  test_hogares$predict_ingreso_lasso<test_hogares$Lp*test_hogares$Npersug,1,0)

table(test_hogares$predict_lasso)

#Elastic net

cv_en <- cv.glmnet(x = X,
                      y = Y,alpha = 0.75) 
coef(cv_en, s = "lambda.min")


test_hogares$predict_ingreso_en<-predict(cv_en, newx = X_test, s = "lambda.min")

test_hogares$predict_en<-ifelse(
  test_hogares$predict_ingreso_en<test_hogares$Lp*test_hogares$Npersug,1,0)

test_hogares$predict_ingreso_en


#Con Caret
tc_10 <- trainControl(method = "cv", number = 10)

en_caret <- train(
  x=X,
  y=Y,
  method = "glmnet",
  trControl = tc_10,
  tuneLength=100
)

test_hogares$predict_ingreso_glmCaret<-predict(en_caret, newdata = X_test)
test_hogares$predict_glmCaret<-ifelse(
  test_hogares$predict_ingreso_glmCaret<test_hogares$Lp*test_hogares$Npersug,1,0)

table(test_hogares$predict_glmCaret)