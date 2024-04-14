#Ramdom forest
set.seed(1234)

library(devtools)
library(caret)
library(rpart)
library(pacman)
library(rpart.plot)
library(boot)
source_url("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%202/1.Procesamiento%20y%20sintaxis/Creaci%C3%B3n%20de%20variables%20de%20inter%C3%A9s.R")


train_hogares$rural<-ifelse(train_hogares$Dominio=="RURAL",1,0)
test_hogares$rural<-ifelse(test_hogares$Dominio=="RURAL",1,0)
train_hogares$rural<-as.factor(train_hogares$rural)
test_hogares$rural<-as.factor(test_hogares$rural)
table(train_hogares$rural,train_hogares$Dominio)
table(test_hogares$rural,test_hogares$Dominio)

predictores_modelo<-c( "informalidad_jefe",
                       #"zona_jefe",
                      # "subsidio_jefe",
                       "edad_jefe",
                       #"posicion_jefe",
                       "Ina_jefe",
                       "Des_jefe",
                       "Oc_jefe",
                       "Pet_jefe",
                       #"ocupacion_jefe",
                       "educacion_jefe",
                       #"regimen_jefe",
                       "sexo_jefe",
                       "Clase",
                       "Dominio",
                       "P5000",                   
                       "P5010",
                       "P5090",
                       #"P5100",                    
                       #"P5130",
                       #"P5140",
                       "Nper",                   
                       "Npersug",
                       "Depto",
                       #"regimen_subsidiado_jefe",  
                       "desempleo_jefe",
                       "Personas_habitacion",
                       "tipo_casa",
                       "edad_jefe_joven",
                       "jefe_joven",
                       "jefe_menor",
                       "Personas_habitacion_round"#,
                       #"rural"
                      )

arbol1<-rpart(formula = as.formula(paste("pobre_texto~",
        paste(predictores_modelo, collapse = " + "))),
        data=train_hogares,
        method = "class",
        minbucket=30
        )
prp(arbol1)

train_hogares$predic_arbol1<-predict(arbol1,newdata =train_hogares,type = "class")

confusionMatrix(data = train_hogares$predic_arbol1,
                reference = train_hogares$pobre_texto,
                positive="Pobre", mode = "prec_recall")

test_hogares$predic_arbol1<-predict(arbol1,newdata =test_hogares,type = "class")
table(test_hogares$predic_arbol1)

predictores_modelo<-c("posicion_jefe",
                      "Ina_jefe","tipo_casa",
                      "Des_jefe","jefe_joven",
                      "Oc_jefe",
                      "Pet_jefe",
                      "ocupacion_jefe",
                      "educacion_jefe",
                      "regimen_jefe",
                      "sexo_jefe",
                      "Clase",
                      #"Dominio",
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
                      "Personas_habitacion_round"
  )


train_hogares$Depto<-as.factor(train_hogares$Depto)
test_hogares$Depto<-as.factor(test_hogares$Depto)
train_hogares$regimen_jefe<-as.factor(train_hogares$regimen_jefe)
train_hogares$posicion_jefe<-as.factor(train_hogares$posicion_jefe)
train_hogares$ocupacion_jefe<-as.factor(train_hogares$ocupacion_jefe)
test_hogares$ocupacion_jefe<-as.factor(test_hogares$ocupacion_jefe)

train_hogares_sin_bogota<-train_hogares %>% filter(Dominio!="BOGOTA")

arbol2<-rpart(formula = as.formula(paste("pobre_texto~",
                                         paste(predictores_modelo, 
                                               collapse = " + "))),
              data=train_hogares_sin_bogota,
              method = "class",
              parms = list(split="Gini"))

prp(arbol2)

train_hogares_sin_bogota$predic_arbol2<-predict(arbol2,newdata =train_hogares_sin_bogota,type = "class")

confusionMatrix(data = train_hogares_sin_bogota$predic_arbol2,
                reference = train_hogares_sin_bogota$pobre_texto,
                positive="Pobre", mode = "prec_recall")

test_hogares$predic_arbol2<-predict(arbol2,newdata =test_hogares,type = "class")
table(test_hogares$predic_arbol2)

sub9<-test_hogares %>% select(id,predic_arbol2)
sub9<-sub9 %>% rename(pobre=predic_arbol2)
sub9$pobre<-ifelse(sub9$pobre=="Pobre",1,0)
table(test_hogares$predic_arbol2)
table(sub9$pobre)
write_csv(x = sub9,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/2.Entregables/Submission9.csv",)

#####

arbol3<-rpart(formula = as.formula(paste("Ingtotugarr~",
                                                 paste(predictores_modelo, 
                                                       collapse = " + "))),
                      data=train_hogares_sin_bogota,
                      parms = list(split="Gini"))

prp(arbol3)

train_hogares_sin_bogota$predic_ingreso_arbol3<-predict(arbol3,newdata =train_hogares_sin_bogota)

train_hogares_sin_bogota$predict_arbol3<-ifelse(
  train_hogares_sin_bogota$predic_ingreso_arbol3<
    train_hogares_sin_bogota$Lp*train_hogares_sin_bogota$Npersug,
  1,0)

confusionMatrix(data = train_hogares_sin_bogota$predic_arbol2,
                reference = train_hogares_sin_bogota$pobre_texto,
                mode = "prec_recall")


test_hogares$predic_ingreso_arbol3<-predict(arbol3,newdata =test_hogares)

test_hogares$predict_arbol3<-ifelse(
  test_hogares$predic_ingreso_arbol3<
    test_hogares$Lp*test_hogares$Npersug,
  1,0)

table(test_hogares$predict_arbol3)

sub10<-test_hogares %>% select(id,predict_arbol3)
sub10<-sub10 %>% rename(pobre=predict_arbol3)
table(test_hogares$predict_arbol3)
table(sub10$pobre)
write_csv(x = sub10,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/2.Entregables/Submission10.csv",)

####
predictores_modelo<-c("posicion_jefe",
                      "Des_jefe",
                      "Oc_jefe",
                      "Pet_jefe",
                      "ocupacion_jefe",
                      "educacion_jefe",
                      "regimen_jefe",
                      "sexo_jefe",
                      "Clase",
                      "rural",
                      "P5000",
                      "P5010",
                      "P5090",
                      "P5100",
                      "P5130",
                      "P5140",
                      "Nper",
                      "Npersug",
                      "jefe_menor",
                      "Depto",
                      "regimen_subsidiado_jefe",
                      "desempleo_jefe",
                      "Personas_habitacion_round"
)


arbol4<-rpart(formula = as.formula(paste("pobre_texto~",
                                         paste(predictores_modelo, 
                                               collapse = " * "))),
              data=train_hogares_sin_bogota,
              method = "class",
              parms = list(split="Gini"))

prp(arbol4)

table(train_hogares_sin_bogota$Pobre,train_hogares_sin_bogota$regimen_subsidiado_jefe)
train_hogares_sin_bogota$predic_arbol2<-predict(arbol2,newdata =train_hogares_sin_bogota,type = "class")

confusionMatrix(data = train_hogares_sin_bogota$predic_arbol2,
                reference = train_hogares_sin_bogota$pobre_texto,
                positive="Pobre", mode = "prec_recall")

test_hogares$predic_arbol2<-predict(arbol2,newdata =test_hogares,type = "class")
table(test_hogares$predic_arbol2)

sub9<-test_hogares %>% select(id,predic_arbol2)
sub9<-sub9 %>% rename(pobre=predic_arbol2)
sub9$pobre<-ifelse(sub9$pobre=="Pobre",1,0)
table(test_hogares$predic_arbol2)
table(sub9$pobre)
write_csv(x = sub9,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/2.Entregables/Submission9.csv",)

