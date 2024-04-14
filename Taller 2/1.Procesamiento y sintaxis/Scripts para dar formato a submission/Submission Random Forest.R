X<-test_hogares %>% select(predictores)

predic_RF<-predict(RF,X)
test_hogares$predic_RF<-predic_RF$predictions
sub11<-test_hogares %>% select(id,predic_RF)
sub11<-sub11 %>% rename(pobre=predic_RF)
sub11$pobre<-ifelse(sub11$pobre=="Pobre",1,0)
table(sub11$pobre)