distancia_variable <-function(i,data,variable) {
  # Calcular distancias entre la observación con NA y todas las observaciones sin NA
  distances <- st_distance(paste0(data,"_with_na[i, ]"),paste0(data,"_without_na"))
  
  # Encontrar el índice de la observación más cercana
  nearest_idx <- which.min(distances)
  
  # Devolver el valor del estrato de la observación más cercana
  return(paste0(data,"test_without_na$",variable,"[nearest_idx]"))
}
