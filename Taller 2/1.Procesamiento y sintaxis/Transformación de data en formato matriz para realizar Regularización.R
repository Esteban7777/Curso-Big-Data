X<-train_hogares_sin_bogota %>% select(predictores)
X$sexo_jefe<-as.numeric(X$sexo_jefe)
X$Clase<-as.numeric(X$Clase)
#X<-as.matrix(train_hogares_sin_bogota[,predictores])
X<-as.matrix(X)
Y<-train_hogares_sin_bogota[,"Ingtotugarr"]

#X_test<-as.matrix(test_hogares[,predictores])
X_test<-test_hogares %>% select(predictores)
X_test$sexo_jefe<-as.numeric(X_test$sexo_jefe)
X_test$Clase<-as.numeric(X_test$Clase)
X_test<-as.matrix(X_test)