#Modelo de regresión lineal 
set.seed(1234)
#Cargamos la data con las variables construidas para el modelo
library(devtools)
library(caret)
source_url("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%202/1.Procesamiento%20y%20sintaxis/Creaci%C3%B3n%20de%20variables%20de%20inter%C3%A9s.R")

#Exploramos las posibles variables dependientes
summary(train_hogares$Ingtotug)
table(is.na(train_hogares$Ingtotug)) #la variable está completa en train

summary(train_hogares$Ingtotugarr)
table(is.na(train_hogares$Ingtotugarr)) #la variable está completa en train


#Modelos
predictores_modelo1<-c("posicion_jefe", #No está completa en Test 19145 NA
                        "desempleo_jefe", #Completa
                        "educacion_jefe", #Completa
                        "regimen_subsidiado_jefe", #No esta completa en Test 3837 NA
                        "sexo_jefe", #Completa
                        "Clase", #Completa
                        "Dominio", #Completa
                        "P5090", #Completa
                        "Ina_jefe", #Completa
                        "Des_jefe", #Completa
                        "Oc_jefe", #Completa
                        "Pet_jefe") #Completa. Pero solo tiene una categoria 



m1<-lm(as.formula(
  paste("Ingtotug~",
        paste(predictores_modelo1, collapse = " + "))),
       data = train_hogares)

summary(m1)

test_hogares$m1_ingreso<-predict(m1,newdata = test_hogares)
test_hogares$m1_predict<-ifelse(test_hogares$m1_ingreso<test_hogares$Lp,1,0)
table(is.na(test_hogares$m1_predict))
# Las predicciones no sirven con NA. Se procede a eliminar las variables con faltantes

#Ya que a desempleo le falta un solo dato se decide reemplazarlo aleatoriamente.


test_hogares$desempleo_jefe[is.na(test_hogares$desempleo_jefe)] <- sample(0:1, 1)

predictores_modelo2<-c("desempleo_jefe", #Completa
                          "educacion_jefe", #Completa 
                          "sexo_jefe", #Completa
                          "Clase", #Completa
                          "Dominio", #Completa
                          "P5090", #Completa
                          "Ina_jefe", #Completa 
                          "Des_jefe", #Completa
                          "Oc_jefe" #Completa
                          )

m2<-lm(as.formula(
  paste("Ingtotugarr~",
        paste(predictores_modelo2, collapse = " + "))),
  data = train_hogares %>% filter(Dominio!="BOGOTA")) #como no hay datos de Dominio=Bogotá en Test, se entrena sin Bogota

#Parámetros del modelo
summary(m2)

#Ajuste dentro de muestra
#No olvidar que train tiene filtrado Bogotá
train_hogares_sin_bogota<-train_hogares %>% 
                             filter(Dominio!="BOGOTA")
train_hogares_sin_bogota$m2_ingreso<-predict(object = m2,
                    newdata = train_hogares_sin_bogota)

train_hogares_sin_bogota$m2_predict<-ifelse(train_hogares_sin_bogota$m2_ingreso<train_hogares_sin_bogota$Lp,
                   1,0)

matrix_predicciones2<-table(train_hogares_sin_bogota$m2_predict,train_hogares_sin_bogota$Pobre)
confusionMatrix(matrix_predicciones)

#Fuera de muestra
test_hogares$m2_ingreso<-predict(object = m2,newdata = test_hogares)
test_hogares$m2_predict<-ifelse(test_hogares$m2_ingreso<test_hogares$Lp,1,0)

sub1<-test_hogares %>% select(id,m2_predict)
sub1<-sub1 %>% rename(pobre=m2_predict)
write_csv(x = sub1,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/2.Entregables/Submission1.csv",)


#Retiramos Dominio ya que perdemos mucha información de Train
predictores_modelo3<-c("desempleo_jefe", #Completa
                        "educacion_jefe", #Completa 
                        "sexo_jefe", #Completa
                        "Clase", #Completa
                        "P5090", #Completa
                        "Ina_jefe", #Completa 
                        "Des_jefe", #Completa
                        "Oc_jefe" #Completa
)

m3<-lm(as.formula(
  paste("Ingtotugarr~",
        paste(predictores_modelo3, collapse = " + "))),
  data = train_hogares) 

#Parámetros del modelo
summary(m3)

#Ajuste dentro de muestra
train_hogares$m3_ingreso<-predict(object = m3,
                                             newdata = train_hogares)

train_hogares$m3_predict<-ifelse(
  train_hogares$m3_ingreso<train_hogares$Lp*train_hogares$Npersug,
  1,0)

matrix_predicciones3<-table(train_hogares$m3_predict,train_hogares$Pobre)
confusionMatrix(matrix_predicciones3)

#Fuera de muestra
test_hogares$m3_ingreso<-predict(object = m3,newdata = test_hogares)
test_hogares$m3_predict<-ifelse(
  test_hogares$m3_ingreso<test_hogares$Lp*test_hogares$Npersug,1,0)

table(is.na(test_hogares$m3_predict))

#La predicción tiene un NA que se asigna aleatoriamente.
test_hogares$m3_predict <- replace(test_hogares$m3_predict,
                                        is.na(test_hogares$m3_predict),
                                        sample(c(0,1),1))

table(is.na(test_hogares$m3_predict))
table(test_hogares$m3_predict)
sub2<-test_hogares %>% select(id,m3_predict)
sub2<-sub2 %>% rename(pobre=m3_predict)
write_csv(x = sub2,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/2.Entregables/Submission2.csv",)
table(sub2$pobre)

#####

#Retirando los predictores que no son significativos.
predictores_modelo7<-c("desempleo_jefe", #Completa
                       "educacion_jefe", #Completa 
                       "sexo_jefe", #Completa
                       "Clase", #Completa
                       "P5090", #Completa
                       "Ina_jefe", #Completa 
                       "Des_jefe", #Completa
                       "Oc_jefe", #Completa
                       "Personas_habitacion"#Completa
                       )

m7<-lm(as.formula(
  paste("Ingtotugarr~",
        paste(predictores_modelo7, collapse = " + "))),
  data = train_hogares) 

#Parámetros del modelo
summary(m7)

#Ajuste dentro de muestra
train_hogares$m7_ingreso<-predict(object = m7,
                                  newdata = train_hogares)

train_hogares$m7_predict<-ifelse(
  train_hogares$m7_ingreso<train_hogares$Lp*train_hogares$Npersug,
  1,0)

matrix_predicciones7<-table(train_hogares$m7_predict,train_hogares$Pobre)
confusionMatrix(matrix_predicciones7)

#Fuera de muestra
test_hogares$m7_ingreso<-predict(object = m7,newdata = test_hogares)
test_hogares$m7_predict<-ifelse(
  test_hogares$m7_ingreso<test_hogares$Lp*test_hogares$Npersug,1,0)

table(is.na(test_hogares$m7_predict))

#La predicción tiene un NA que se asigna aleatoriamente.
test_hogares$m7_predict <- replace(test_hogares$m7_predict,
                                   is.na(test_hogares$m7_predict),
                                   sample(c(0,1),1))

table(is.na(test_hogares$m7_predict))
table(test_hogares$m7_predict)
sub7<-test_hogares %>% select(id,m7_predict)
sub7<-sub7 %>% rename(pobre=m7_predict)
write_csv(x = sub7,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/2.Entregables/Submission7.csv",)
table(sub7$pobre)


#####

#Se realiza un ejercicio de seleccionar de forma parsimoniosa las variables que mayor cantidad de pobres predicen


predictores_modelo7<-c("educacion_jefe",
                       "P5090",
                       "Personas_habitacion",
                       "Des_jefe",
                       "sexo_jefe",
                       "Clase")

m7<-lm(as.formula(
  paste("Ingtotugarr~",
        paste(predictores_modelo7, collapse = " + "))),
  data = train_hogares) 

#Parámetros del modelo
summary(m7)

#Ajuste dentro de muestra
train_hogares$m7_ingreso<-predict(object = m7,
                                  newdata = train_hogares)

train_hogares$m7_predict<-ifelse(
  train_hogares$m7_ingreso<train_hogares$Lp*train_hogares$Npersug,
  1,0)

matrix_predicciones7<-table(train_hogares$m7_predict,train_hogares$Pobre)
confusionMatrix(matrix_predicciones7)

#Fuera de muestra
test_hogares$m7_ingreso<-predict(object = m7,newdata = test_hogares)
test_hogares$m7_predict<-ifelse(
  test_hogares$m7_ingreso<test_hogares$Lp*test_hogares$Npersug,1,0)

table(is.na(test_hogares$m7_predict))

#La predicción tiene un NA que se asigna aleatoriamente.
test_hogares$m7_predict <- replace(test_hogares$m7_predict,
                                   is.na(test_hogares$m7_predict),
                                   sample(c(0,1),1))

table(is.na(test_hogares$m7_predict))
table(test_hogares$m7_predict)
sub8<-test_hogares %>% select(id,m7_predict)
sub8<-sub8 %>% rename(pobre=m7_predict)
write_csv(x = sub8,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/2.Entregables/Submission8.csv",)
table(sub8$pobre)