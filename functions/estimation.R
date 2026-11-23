library(dplyr)
library(data.table)
library(survey)



estimar_total = function(muestra, tipo = "MSA"){
  # Estima el total de ingresos, usando las columnas seleccion y prob_seleccion, es decir el estimador de horvit thompson
  # total = ingresos * seleccion * 1 / prob_seleccion
  # inptus:
  # muestra -- muestra con las columnas ingreso, prob_seleccion y seleccion
  # tipo -- tipo de muestreo, por defecto MSA
  # outputs
  # estimacion del total
  # Manejo de errores:
  # seleccion debe ser 0 o 1 -- utiliza un censo



  total =sum( 1/ matriz$Prob_seleccion *  muestra$Ingresos * muestra$Responde * muestra$Ajuste_no_respuesta *muestra$Peso_calibracion  )
  return(total)
}


