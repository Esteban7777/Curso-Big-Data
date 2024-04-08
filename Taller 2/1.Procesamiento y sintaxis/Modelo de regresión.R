#Modelo de regresión lineal 
set.seed(123)
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
dependientes_modelo1<-c("posicion_jefe", #No está completa en Test 19145 NA
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
        paste(dependientes_modelo1, collapse = " + "))),
       data = train_hogares)

summary(m1)

test_hogares$m1_ingreso<-predict(m1,newdata = test_hogares)
test_hogares$m1_predict<-ifelse(test_hogares$m1_ingreso<test_hogares$Lp,1,0)
table(is.na(test_hogares$m1_predict))
# Las predicciones no sirven con NA. Se procede a eliminar las variables con faltantes

#Ya que a desempleo le falta un solo dato se decide reemplazarlo aleatoriamente.


test_hogares$desempleo_jefe[is.na(test_hogares$desempleo_jefe)] <- sample(0:1, 1)

dependientes_modelo2<-c("desempleo_jefe", #Completa
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
  paste("Ingtotug~",
        paste(dependientes_modelo2, collapse = " + "))),
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
dependientes_modelo3<-c("desempleo_jefe", #Completa
                        "educacion_jefe", #Completa 
                        "sexo_jefe", #Completa
                        "Clase", #Completa
                        "P5090", #Completa
                        "Ina_jefe", #Completa 
                        "Des_jefe", #Completa
                        "Oc_jefe" #Completa
)

m3<-lm(as.formula(
  paste("Ingtotug~",
        paste(dependientes_modelo3, collapse = " + "))),
  data = train_hogares) 

#Parámetros del modelo
summary(m3)

#Ajuste dentro de muestra
train_hogares$m3_ingreso<-predict(object = m3,
                                             newdata = train_hogares)

train_hogares$m3_predict<-ifelse(train_hogares$m3_ingreso<train_hogares$Lp,
                                            1,0)

matrix_predicciones3<-table(train_hogares$m3_predict,train_hogares$Pobre)
confusionMatrix(matrix_predicciones3)

#Fuera de muestra
test_hogares$m3_ingreso<-predict(object = m3,newdata = test_hogares)
test_hogares$m3_predict<-ifelse(test_hogares$m3_ingreso<test_hogares$Lp,1,0)

table(is.na(test_hogares$m3_ingreso))

sub2<-test_hogares %>% select(id,m3_predict)
sub2<-sub2 %>% rename(pobre=m3_predict)
write_csv(x = sub2,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/2.Entregables/Submission2.csv",)

#Modeo Logit

m4<-glm(as.formula(
  paste("Pobre~",
        paste(dependientes_modelo3, collapse = " + "))),data = train_hogares)

summary(m4)

train_hogares$m4_predict<-predict(m4,newdata = train_hogares)

matrix_predicciones4<-table(train_hogares$m4_predict,train_hogares$Pobre)
confusionMatrix(matrix_predicciones3)

#Fuera de muestra
test_hogares$m4_predict<-predict(object = m4,newdata = test_hogares)

table(is.na(test_hogares$m4_predict))
#Sigue saltando multicolinealidad.

#Se depuran variables
dependientes_modelo4<-c("desempleo_jefe", #Completa
                        "educacion_jefe", #Completa 
                        "sexo_jefe", #Completa
                        "Clase", #Completa
                        "P5090" #Completa
                        )


m5<-glm(as.formula(
  paste("Pobre~",
        paste(dependientes_modelo4, collapse = " + "))),data = train_hogares)

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


dependientes_modelo5<-c("Clase","Des_jefe" #Completa 
                                            )
table(train_hogares$Pobre,train_hogares$Des_jefe)

m6<-glm(as.formula(
  paste("Pobre~",
        paste(dependientes_modelo5, collapse = " * "))),data = train_hogares)

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

dependientes_modelo7<-c("educacion_jefe",
                        "sexo_jefe",
                        "Personas_habitacion"#Completa 
)


table(test_hogares$sexo_jefe)
table(test_hogares$regimen_jefe)

m7<-glm(as.formula(
  paste("Pobre~",
        paste(dependientes_modelo7, collapse = " * "))),data = train_hogares)

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