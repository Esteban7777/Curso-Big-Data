library(devtools)
library(tidyverse)
library(stringi)
library(tokenizers)
library(stopwords)
library(SnowballC)
library(wordcloud)


#Leemos la data
train<-read_csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/train_join.csv")
test<-read_csv("https://raw.githubusercontent.com/Esteban7777/Curso-Big-Data/main/Taller%203/0.Insumos/test_join.csv")

#Hacemos una tokenización de titulo y descripción de la vivienda para identificar variables

test_titulos<-stri_trans_general(str = test$title,id="Latin-ASCII")

#Eliminamos caracteres especiales
test_titulos<-gsub('[^A-Za-z0-9 ]+', ' ', test_titulos)
#Ponemos en minúsculas
test_titulos<-tolower(test_titulos)
#Eliminamos espacios
test_titulos <- gsub('\\s+', ' ', test_titulos)
#Se eliminan números y espacios no simples generados
test_titulos <- gsub("\\d+", "", test_titulos)
test_titulos <- gsub('\\s+', ' ', test_titulos)
test_titulos <- trimws(test_titulos)

test_titulos_token<-tokenize_words(test_titulos)

length(unique(test_titulos_token))

#Descargamos los stopwords
lista_palabras1 <- stopwords(language = "es", source = "snowball")
lista_palabras2 <- stopwords(language = "es", source = "nltk")
lista_palabras <- union(lista_palabras1, lista_palabras2)
lista_palabras


test_titulos_token<-lapply(test_titulos_token, function(x){
  x<-tolower(x)
  setdiff(x,lista_palabras)
})

#test_titulos_token<-wordStem(test_titulos_token,"spanish")
all_words <- unlist(test_titulos_token)

frecuencia <- data.frame(table(all_words))
names(frecuencia) <- c("Palabra", "Freq")
frecuencia <- frecuencia[order(-frecuencia$Freq),]


# frecuencia <- test_titulos_token %>%
#   table() %>%
#   data.frame() %>%
#   rename("Palabra" = ".") %>%
#   arrange(desc(Freq))
# 
set.seed(1234) 
png(filename = "wordcloud.png", width =400, height =400)
wordcloud(words = frecuencia$Palabra, freq = frecuencia$Freq, min.freq = 1,
          max.words = 200, random.order=  FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))
dev.off()

# Cerrar todos los dispositivos gráficos
while (dev.cur() > 1) {
  dev.off()
}


#Repetimos el proceso para las descripciones

test_descripciones<-stri_trans_general(str = test$description,id="Latin-ASCII")

#Eliminamos caracteres especiales
test_descripciones<-gsub('[^A-Za-z0-9 ]+', ' ', test_descripciones)
#Ponemos en minúsculas
test_descripciones<-tolower(test_descripciones)
#Eliminamos espacios
test_descripciones <- gsub('\\s+', ' ', test_descripciones)
#Se eliminan números y espacios no simples generados
test_descripciones <- gsub("\\d+", "", test_descripciones)
test_descripciones <- gsub('\\s+', ' ', test_descripciones)
test_descripciones <- trimws(test_descripciones)

test_descripciones_token<-tokenize_words(test_descripciones)

length(unique(test_descripciones_token))

#Descargamos los stopwords
lista_palabras1 <- stopwords(language = "es", source = "snowball")
lista_palabras2 <- stopwords(language = "es", source = "nltk")
lista_palabras <- union(lista_palabras1, lista_palabras2)
lista_palabras


test_descripciones_token<-lapply(test_descripciones_token, function(x){
  x<-tolower(x)
  setdiff(x,lista_palabras)
})

#test_titulos_token<-wordStem(test_titulos_token,"spanish")
all_words <- unlist(test_descripciones_token)

frecuencia <- data.frame(table(all_words))
names(frecuencia) <- c("Palabra", "Freq")
frecuencia <- frecuencia[order(-frecuencia$Freq),]


set.seed(1234) 
png(filename = "wordcloud.png", width =400, height =400)
wordcloud(words = frecuencia$Palabra, freq = frecuencia$Freq, min.freq = 1,
          max.words = 200, random.order=  FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))
dev.off()

# Cerrar todos los dispositivos gráficos
while (dev.cur() > 1) {
  dev.off()
}


#####
#Repetimos para Train

train_titulos<-stri_trans_general(str = train$title,id="Latin-ASCII")

#Eliminamos caracteres especiales
train_titulos<-gsub('[^A-Za-z0-9 ]+', ' ', train_titulos)
#Ponemos en minúsculas
train_titulos<-tolower(train_titulos)
#Eliminamos espacios
train_titulos <- gsub('\\s+', ' ', train_titulos)
#Se eliminan números y espacios no simples generados
train_titulos <- gsub("\\d+", "", train_titulos)
train_titulos <- gsub('\\s+', ' ', train_titulos)
train_titulos <- trimws(train_titulos)

train_titulos_token<-tokenize_words(train_titulos)

length(unique(train_titulos_token))

#Descargamos los stopwords
lista_palabras1 <- stopwords(language = "es", source = "snowball")
lista_palabras2 <- stopwords(language = "es", source = "nltk")
lista_palabras <- union(lista_palabras1, lista_palabras2)
lista_palabras


train_titulos_token<-lapply(train_titulos_token, function(x){
  x<-tolower(x)
  setdiff(x,lista_palabras)
})

#train_titulos_token<-wordStem(train_titulos_token,"spanish")
all_words <- unlist(train_titulos_token)

frecuencia <- data.frame(table(all_words))
names(frecuencia) <- c("Palabra", "Freq")
frecuencia <- frecuencia[order(-frecuencia$Freq),]

set.seed(1234) 
png(filename = "wordcloud.png", width =400, height =400)
wordcloud(words = frecuencia$Palabra, freq = frecuencia$Freq, min.freq = 1,
          max.words = 200, random.order=  FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))
dev.off()

# Cerrar todos los dispositivos gráficos
while (dev.cur() > 1) {
  dev.off()
}


#Repetimos el proceso para las descripciones

train_descripciones<-stri_trans_general(str = train$description,id="Latin-ASCII")

#Eliminamos caracteres especiales
train_descripciones<-gsub('[^A-Za-z0-9 ]+', ' ', train_descripciones)
#Ponemos en minúsculas
train_descripciones<-tolower(train_descripciones)
#Eliminamos espacios
train_descripciones <- gsub('\\s+', ' ', train_descripciones)
#Se eliminan números y espacios no simples generados
train_descripciones <- gsub("\\d+", "", train_descripciones)
train_descripciones <- gsub('\\s+', ' ', train_descripciones)
train_descripciones <- trimws(train_descripciones)

train_descripciones_token<-tokenize_words(train_descripciones)

length(unique(train_descripciones_token))

#Descargamos los stopwords
lista_palabras1 <- stopwords(language = "es", source = "snowball")
lista_palabras2 <- stopwords(language = "es", source = "nltk")
lista_palabras <- union(lista_palabras1, lista_palabras2)
lista_palabras


train_descripciones_token<-lapply(train_descripciones_token, function(x){
  x<-tolower(x)
  setdiff(x,lista_palabras)
})

#train_titulos_token<-wordStem(train_titulos_token,"spanish")
all_words <- unlist(train_descripciones_token)

frecuencia <- data.frame(table(all_words))
names(frecuencia) <- c("Palabra", "Freq")
frecuencia <- frecuencia[order(-frecuencia$Freq),]

set.seed(1234) 
png(filename = "wordcloud.png", width =400, height =400)
wordcloud(words = frecuencia$Palabra, freq = frecuencia$Freq, min.freq = 1,
          max.words = 200, random.order=  FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))
dev.off()

# Cerrar todos los dispositivos gráficos
while (dev.cur() > 1) {
  dev.off()
}