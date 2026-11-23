
library(dplyr)
library(data.table)
library(survey)


asignar_probabilidad_de_seleccion = function(poblacion, n=n, tipo = "MSA"){
  # Añade una columna a matriz que contiene la probabilidad de seleccion n/N
  # inputs:
  # poblacion -- data.table que contiene la población
  # n -- tamaño de la muestra
  # tipo -- tipo de muestra, por defecto MSA
  # output:
  # data.table con la población más una columna adicional con la probabilidad de seleccion 'prob_seleccion'

  N = nrow(matriz)
  
  if(tipo == "MSA"){
    prob = as.integer(n/N * 1e6)
    poblacion[, Prob_seleccion :=
      rep( prob, times=N)
    ]
  }

}

seleccionar_muestra = function(poblacion, n=n , tipo = "MSA"){
  #Selecciona una muestra simple al azar.
  # inputs:
  # poblacion -- poblacion con la poblacion
  # n -- tamaño de la muestra
  # tipo -- tipo de muestreo a utilizar, por defecto MSA
  # output:
  # Un data.table con las mismas columnas que poblacion, pero con n filas.

  N = nrow(matriz)
  
  if(tipo == "MSA"){
    seleccion = sample.int(N, n)
  }

  return(poblacion[seleccion,])
}
