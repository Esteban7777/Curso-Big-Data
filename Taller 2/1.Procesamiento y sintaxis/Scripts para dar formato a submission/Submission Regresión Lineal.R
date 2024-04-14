test_hogares$modelo_lm_ingreso<-predict(object = modelo_lm,
                                        newdata = test_hogares)

test_hogares$modelo_lm_predict<-ifelse(
  test_hogares$modelo_lm_ingreso<test_hogares$Lp,
  1,0) #Clasificamos los hogares en pobres o no según el ingreso predicho
table(test_hogares$modelo_lm_predict)