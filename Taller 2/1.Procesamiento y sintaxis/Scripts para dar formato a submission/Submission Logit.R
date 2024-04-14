test_hogares$prob_logit<-predict(object = logit,newdata = test_hogares)
test_hogares$logit_predict<-ifelse(test_hogares$prob_logit>0.5,1,0)
table(test_hogares$logit_predict)