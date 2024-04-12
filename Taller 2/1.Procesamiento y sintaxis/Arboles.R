#Ramdom forest
set.seed(1234)

library(devtools)
library(caret)
library(rpart)
library(pacman)
library(rpart.plot)

source_url("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%202/1.Procesamiento%20y%20sintaxis/Creaci%C3%B3n%20de%20variables%20de%20inter%C3%A9s.R")

train_hogares$pobre_texto<-ifelse(train_hogares$Pobre==1,"Pobre","No_Pobre")
train_hogares$pobre_texto<-as.factor(train_hogares$pobre_texto)

predictores_modelo<-c("educacion_jefe",
                       "P5090",
                       "Personas_habitacion",
                       "Des_jefe",
                       "sexo_jefe",
                       "Clase")

arbol1<-rpart(formula = as.formula(paste("pobre_texto~",
        paste(predictores_modelo, collapse = " + "))),
        data=train_hogares,
        method = "class",
        parms = list(split="Gini"))
prp(arbol1)

train_hogares$predic_arbol1<-predict(arbol1,newdata =train_hogares,type = "class")

confusionMatrix(data = train_hogares$predic_arbol1,
                reference = train_hogares$pobre_texto,
                positive="Pobre", mode = "prec_recall")

test_hogares$predic_arbol1<-predict(arbol1,newdata =test_hogares,type = "class")
table(test_hogares$predic_arbol1)

predictores_modelo<-c("posicion_jefe",
                      "Ina_jefe",
                      "Des_jefe",
                      "Oc_jefe",
                      "Pet_jefe",
                      "ocupacion_jefe",
                      "educacion_jefe",
                      "regimen_jefe",
                      "sexo_jefe",
                      "Clase",
                      "Dominio",
                      "P5000",
                      "P5010",
                      "P5090",
                      "P5100",
                      "P5130",
                      "P5140",
                      "Nper",
                      "Npersug",
                      "Depto",                  
                      "regimen_subsidiado_jefe",
                      "desempleo_jefe",
                      "Personas_habitacion"
  
)

train_hogares$regimen_jefe<-as.factor(train_hogares$regimen_jefe)
train_hogares$posicion_jefe<-as.factor(train_hogares$posicion_jefe)
train_hogares$ocupacion_jefe<-as.factor(train_hogares$ocupacion_jefe)
test_hogares$ocupacion_jefe<-as.factor(test_hogares$ocupacion_jefe)
arbol2<-rpart(formula = as.formula(paste("pobre_texto~",
                                         paste(predictores_modelo, 
                                               collapse = " + "))),
              data=train_hogares,
              method = "class",
              parms = list(split="Gini"))

prp(arbol2)

train_hogares$predic_arbol2<-predict(arbol2,newdata =train_hogares,type = "class")

confusionMatrix(data = train_hogares$predic_arbol2,
                reference = train_hogares$pobre_texto,
                positive="Pobre", mode = "prec_recall")

test_hogares$predic_arbol2<-predict(arbol2,newdata =test_hogares,type = "class")
table(test_hogares$predic_arbol2)