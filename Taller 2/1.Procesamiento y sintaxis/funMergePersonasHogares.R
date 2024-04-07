traer_variable<-function(df1,df2,variable){
  aux<-unique(df2[,c("id",variable)])
  df3<-left_join(aux,df1,by="id")
  if (nrow(df3)==nrow(df1)) {
    return(df3)
  } else {"La variable creada no tiene un valor único por hogar"}
  
}