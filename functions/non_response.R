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
  #       MCAR provincia -- asigna la respuesta al azar, con una tasa distinta en cada provincia
  #       MAR -- SIN IMPLEMENTAR  asigna la respuesta basandose en un modelo con las variables explicativas
  #       MNAR -- SIN IMPLEMENTAR  asigna la respuesta basandose en la variable respuesta
  # tasa -- proporción de no respuesta
  n = nrow(data)
  
  if(tipo_no_respuesta == 'MCAR'){
    responde = as.integer( runif(n = n) > tasa_no_respuesta )
  } else if(tipo_no_respuesta == 'MCAR provincia'){
    # tasa de cada provincia: binomial alrededor de tasa_no_respuesta
    n_provincias = max(data$Provincia)
    tasa_no_respuesta_provincia = (rbinom(n = n_provincias, size = 20, prob = tasa_no_respuesta) +1) / 22
    responde = as.integer( runif(n = n) > tasa_no_respuesta_provincia[data$Provincia] )
  } else {
    stop("tipo_no_respuesta debe ser 'MCAR' o 'MCAR provincia'")
  }

  data[, Responde := responde ]

  return(data[])
}


ajustar_no_respuesta = function(data, tipo = "MSA", tipo_ajuste = 'MCAR'){
  # Crea una columna con el factor de ajuste por no respuesta (Ajuste_no_respuesta)
  # Inputs:
  # data -- muestra o poblacion, con la columna Responde
  # tipo -- tipo de muestreo, por defecto simple al azar
  # tipo ajuste:  
  #       Ninguno -- no ajusta (factor 1)
  #       MCAR -- asume que la no respuesta fue completamente al azar, así que corrige por la tasa de no respuesta
  #       MCAR provincia -- igual que MCAR, pero con la tasa de respuesta de cada provincia
  #       MAR -- SIN IMPLEMENTAR  ajusta basandose en un modelo con las variables explicativas
  n = nrow(data)
  
  if(tipo_ajuste == 'Ninguno'){
    data[, Ajuste_no_respuesta := 1 ]   
  } else if(tipo_ajuste == 'MCAR'){
    tasa_respuesta = sum( data$Responde) / n
    data[, Ajuste_no_respuesta := 1 / tasa_respuesta ]   
  } else if(tipo_ajuste == 'MCAR provincia'){
    data[, Ajuste_no_respuesta := .N / sum(Responde), by = Provincia ]   
  } else {
    stop("tipo_ajuste debe ser 'Ninguno', 'MCAR' o 'MCAR provincia'")
  }

  if(any( is.infinite(data$Ajuste_no_respuesta))) stop("No hay respondentes (en alguna provincia)")

  return(data[])
}