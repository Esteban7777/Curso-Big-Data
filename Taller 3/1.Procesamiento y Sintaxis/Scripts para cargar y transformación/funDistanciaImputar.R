# distancia_variable <-function(i,data,variable) {
#   # Calcular distancias entre la observación con NA y todas las observaciones sin NA
#   distances <- st_distance(paste0(data,"_with_na[i, ]"),paste0(data,"_without_na"))
#   
#   # Encontrar el índice de la observación más cercana
#   nearest_idx <- which.min(distances)
#   
#   # Devolver el valor del estrato de la observación más cercana
#   return(paste0(data,"test_without_na$",variable,"[nearest_idx]"))
# }


distancia_variable <- function(data_sf, variable, crs = 4326) {
  # Separar observaciones con NA y sin NA en la variable especificada
  data_with_na <- data_sf[is.na(data_sf[[variable]]), ]
  data_without_na <- data_sf[!is.na(data_sf[[variable]]), ]
  
  # Para cada observación con NA, encontrar el valor de la observación más cercana
  imputaciones <- sapply(1:nrow(data_with_na), function(i) {
    distances <- st_distance(data_with_na[i, ], data_without_na)
    nearest_idx <- which.min(distances)
    return(data_without_na[[variable]][nearest_idx])
  })
  
  # Asignar los valores imputados
  data_with_na[[variable]] <- imputaciones
  
  # Reintegrar el dataframe completo
  data_imputed <- rbind(data_without_na, data_with_na)
  
  return(data_imputed)
}