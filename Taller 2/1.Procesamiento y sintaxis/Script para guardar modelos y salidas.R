writeLines(outputRF6,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/1.Procesamiento y sintaxis/RF_6VAR.txt")
outputRF7<-capture.output(RF_7VAR$prediction.error)
writeLines(outputRF7,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/1.Procesamiento y sintaxis/RF_7VAR.txt")

saveRDS(RF_5VAR,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/1.Procesamiento y sintaxis/Ramdom Forest 5 VARIABLES.rds")

outputlogit<-capture.output(stargazer(logit,type = "text"))
writeLines(outputlogit,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/1.Procesamiento y sintaxis/summary logit.txt")

outputcf_logit<-capture.output(cf_logit)
writeLines(outputcf_logit,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/1.Procesamiento y sintaxis/outputcf_logit.txt")

outputcf_arbol<-capture.output(cf_arbol)
writeLines(outputcf_arbol,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/1.Procesamiento y sintaxis/outputcf_arbol.txt")

outputcf_rf<-capture.output(cf_rf)
writeLines(outputcf_rf,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/1.Procesamiento y sintaxis/outputcf_rf.txt")

outputlm<-capture.output(stargazer(modelo_lm,type = "text"))
writeLines(outputlm,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/1.Procesamiento y sintaxis/summary lm.txt")

outputcf_lm<-capture.output(cf_lm)
writeLines(outputcf_lm,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/1.Procesamiento y sintaxis/outputcf_lm.txt")

outputcf_arbol_reg<-capture.output(cf_arbol_reg)
writeLines(outputcf_arbol_reg,"C:/Users/HP-Laptop/Documents/GitHub/Curso-Big-Data/Taller 2/1.Procesamiento y sintaxis/outputcf_arbol_reg.txt")