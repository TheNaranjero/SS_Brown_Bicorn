library(dplyr)
library(data.table)
library(survey)




asignar_no_respuesta = function(data, tipo = "MSA", tipo_no_respuesta = 'MCAR', tasa_no_respuesta = 0.0){
  # Crea una columna de ceros y unos, donde 1 representa responde y 0 no responde
  # Inputs:
  # data -- muestra (o poblacion)
  # tipo -- tipo de muestreo, por defecto simple al azar
  # tipo_no_respuesta:  
  #       MCAR -- asigna la respuesta al azar
  #       MAR -- SIN IMPLEMENTAR  asigna la respuesta basandose en un modelo con las variables explicativas
  #       MNAR -- SIN IMPLEMENTAR  asigna la respuesta basandose en la variable respuesta
  # tasa -- proporción de no respuesta
  n = nrow(data)
  
 if(tipo_no_respuesta == 'MCAR'){
   responde = (runif(n = n) > tasa_no_respuesta)
   
 }





  data[, Responde :=
      responde
    ]
  
}


ajustar_no_respuesta = function(data, tipo = "MSA", tipo_ajuste = 'MCAR'){
  # Crea una columna de ceros y unos, donde 1 representa responde y 0 no responde
  # Inputs:
  # data -- muestra o poblacion
  # tipo -- tipo de muestreo, por defecto simple al azar
  # tipo ajuste:  
  #       MCAR -- asume que la no respuesta fue completamente al azar, así que corrige por la tasa de no respuesta
  #       MAR -- SIN IMPLEMENTAR  asigna la respuesta basandose en un modelo con las variables explicativas
  n = nrow(data)
  
  if(tipo_ajuste == 'Ninguno'){
    data[, Ajuste_no_respuesta :=
      1
    ]   
 }

  if(tipo_ajuste == 'MCAR'){
    tasa_respuesta = sum( data$Responde) / n
    ajuste = as.integer(1/tasa_respuesta * 1e6) 
    data[, Ajuste_no_respuesta :=
      rep(ajuste, times = n)
    ]   
 }

  

}