traer_variable<-function(df1,df2,variable){
  aux <- merge(df1, df2, by = "id", all.x = TRUE)
  aux2<-aux[,c("id",variable)]
  df1<-merge(df1,aux2,by="id",all.x=TRUE)
  return(df1)
}