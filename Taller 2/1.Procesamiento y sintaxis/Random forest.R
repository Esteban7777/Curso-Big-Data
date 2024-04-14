set.seed(1234)

library(devtools)
library(caret)
library(rpart)
library(pacman)
library(rpart.plot)
library(boot)
source_url("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%202/1.Procesamiento%20y%20sintaxis/Creaci%C3%B3n%20de%20variables%20de%20inter%C3%A9s.R")

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

library(ranger)

RF<- ranger(formula = as.formula(paste("pobre_texto~",
                                       paste(predictores_modelo, collapse = " + "))), 
            data = train_hogares,
            num.trees= 500, ## Numero de bootstrap samples y arboles a estimar. Default 500  
            mtry= 4,   # N. var aleatoriamente seleccionadas en cada partición. Baggin usa todas las vars.
            min.node.size  = 1, ## Numero minimo de observaciones en un nodo para intentar 
            importance="impurity") 

#Aumentamos la cantidad de arboles para identificar si se reduce el OOB predictor error

RF1000<- ranger(formula = as.formula(paste("pobre_texto~",
                                           paste(predictores_modelo, collapse = " + "))), 
                data = train_hogares,
                num.trees= 1000, ## Numero de bootstrap samples y arboles a estimar. Default 500  
                mtry= 4,   # N. var aleatoriamente seleccionadas en cada partición. Baggin usa todas las vars.
                min.node.size  = 1, ## Numero minimo de observaciones en un nodo para intentar 
                importance="impurity") 

#Observamos que no se reduce de manera significativa el OOB predictor error
#Se prueba aumentar la cantidad de observaciones por nodo
RF_NODE100<- ranger(formula = as.formula(paste("pobre_texto~",
                                               paste(predictores_modelo, collapse = " + "))), 
                    data = train_hogares,
                    num.trees= 500, ## Numero de bootstrap samples y arboles a estimar. Default 500  
                    mtry= 4,   # N. var aleatoriamente seleccionadas en cada partición. Baggin usa todas las vars.
                    min.node.size  = 100, ## Numero minimo de observaciones en un nodo para intentar 
                    importance="impurity") 
RF_NODE100
#Se identifica que la cantidad el OOB es indiferente a la cantidad minima de observaciones por nodo
#Miramos la importancia de las variables en el Ramdom Forest
imp<-importance(RF)
imp<-data.frame(variables=names(imp),importancia=imp)
ggplot(imp, aes(x = reorder(variables, importancia) , y =importancia )) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Variable ", x = "Importancia", y="Variable") +
  theme_minimal() +
  coord_flip() 

#Se identifica que hay cinco variables que son mucho más importantes que el resto.
#Se aumenta a cinco el número de variables por arbol
RF_5VAR<- ranger(formula = as.formula(paste("pobre_texto~",
                                            paste(predictores_modelo, collapse = " + "))), 
                 data = train_hogares,
                 num.trees= 500, ## Numero de bootstrap samples y arboles a estimar. Default 500  
                 mtry= 5,   # N. var aleatoriamente seleccionadas en cada partición. Baggin usa todas las vars.
                 min.node.size  = 1, ## Numero minimo de observaciones en un nodo para intentar 
                 importance="impurity") 
#Si le aumentamos una variable por arbol se reduce el OOB predictor error
RF_6VAR<- ranger(formula = as.formula(paste("pobre_texto~",
                                            paste(predictores_modelo, collapse = " + "))), 
                 data = train_hogares,
                 num.trees= 500, ## Numero de bootstrap samples y arboles a estimar. Default 500  
                 mtry=6 ,   # N. var aleatoriamente seleccionadas en cada partición. Baggin usa todas las vars.
                 min.node.size  = 1, ## Numero minimo de observaciones en un nodo para intentar 
                 importance="impurity") 
RF_5VAR$prediction.error
#Con cinco variables por arbol se logra el menor error de predicción
#Evaluamos las predicciones dentro de muestra
confusionMatrix(data = RF_5VAR$predictions,
                reference = train_hogares$pobre_texto,
                positive = "Pobre",
                mode = "prec_recall")

table(is.na(test_hogares$desmpleo_jefe)) #Tiene 1 solo NA que se le imputa aleatoriamente
test_hogares$desempleo_jefe[is.na(test_hogares$desempleo_jefe)] <- sample(0:1, 1)

X<-test_hogares %>% select(predictores_modelo)

predic_RF5<-predict(RF,X)
test_hogares$predic_RF<-predic_RF$predictions
table(test_hogares$predic_RF)

sub11<-test_hogares %>% select(id,predic_RF)
sub11<-sub11 %>% rename(pobre=predic_RF)
sub11$pobre<-ifelse(sub11$pobre=="Pobre",1,0)
table(sub11$pobre)
table(test_hogares$predic_RF)
write_csv(x = sub11,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/2.Entregables/Submission12.csv",)
