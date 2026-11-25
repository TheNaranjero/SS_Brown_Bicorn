
library(dplyr)
library(data.table)
library(survey)

asignar_probabilidad_de_seleccion = function(poblacion, n, tipo = "MSA"){
  # Añade una columna a matriz que contiene la probabilidad de seleccion.
  # inputs:
  # poblacion -- data.table que contiene la población
  # n -- tamaño de la muestra
  # tipo -- tipo de muestra, "MSA" (por defecto) o "EST igual"
  # output:
  # data.table con la población más una columna adicional con la probabilidad de seleccion 'Prob_seleccion'


  N = nrow(poblacion)
  
  if(tipo == "MSA"){
    poblacion[, Prob_seleccion := n / N ]
  } 
  
  ## Estratificado por provincia
  if (tipo == "EST igual provincia"){
    n_provincias = uniqueN(poblacion$Provincia)
    hogares_provincia = poblacion[, .N, keyby = Provincia]$N   # hogares en cada provincia

    # Si ni > hogares de la provincia, se toma un censo en esa provincia
    # y se recalcula ni con los hogares que quedan por seleccionar.
    # Se repite porque al recalcular ni, otra provincia puede pasar a necesitar censo.
    censo = integer(0)   # provincias con censo
    repeat{
      provincias_restantes = setdiff(1:n_provincias, censo)
      if(length(provincias_restantes) == 0) break
      ni = round( (n - sum(hogares_provincia[censo])) / length(provincias_restantes) )
      nuevas = provincias_restantes[ ni > hogares_provincia[provincias_restantes] ]
      if(length(nuevas) == 0) break
      censo = c(censo, nuevas)
    }

    poblacion[, Prob_seleccion := 1 ]
    poblacion[!(Provincia %in% censo), Prob_seleccion := ni / hogares_provincia[Provincia] ]
  } 

  if (tipo == "EST prop provincia"){
    hogares_provincia = poblacion[, .N, keyby = Provincia]$N
    ni = round( n * hogares_provincia / N )
    poblacion[, Prob_seleccion := ni[Provincia] / hogares_provincia[Provincia] ]
  } 

   ## Estratificado por ciudad
  if (tipo == "EST igual ciudad"){
    n_ciudad = uniqueN(poblacion$Ciudad)
    hogares_ciudad = poblacion[, .N, keyby = Ciudad]$N   # hogares en cada ciudad

    # Si ni > hogares de la provincia, se toma un censo en esa provincia
    # y se recalcula ni con los hogares que quedan por seleccionar.
    # Se repite porque al recalcular ni, otra provincia puede pasar a necesitar censo.
    censo = integer(0)   # ciudades con censo
    repeat{
      ciudades_restantes = setdiff(1:n_ciudad, censo)
      if(length(ciudades_restantes) == 0) break
      ni = round( (n - sum(hogares_ciudad[censo])) / length(ciudades_restantes) )
      nuevas = ciudades_restantes[ ni > hogares_ciudad[ciudades_restantes] ]
      if(length(nuevas) == 0) break
      censo = c(censo, nuevas)
    }

    poblacion[, Prob_seleccion := 1 ]
    poblacion[!(Ciudad %in% censo), Prob_seleccion := ni / hogares_ciudad[Ciudad] ]
  } 

  if (tipo == "EST prop ciudad"){
    hogares_provincia = poblacion[, .N, keyby = Provincia]$N
    ni = round( n * hogares_provincia / N )
    poblacion[, Prob_seleccion := ni[Provincia] / hogares_provincia[Provincia] ]
  } 

  return(poblacion[])
}
seleccionar_muestra = function(poblacion, n, tipo = "MSA"){
  #Selecciona una muestra simple al azar o estratificada (asiganción igual o proporcional)
  # inputs:
  # poblacion -- poblacion con la poblacion
  # n -- tamaño de la muestra
  # tipo -- tipo de muestreo a utilizar: "MSA" (por defecto), "EST igual" o "EST prop"
  # output:
  # Un data.table con las mismas columnas que poblacion, pero con n filas.
  # requiere haber corrido antes asignar_probabilidad_de_seleccion con el mismo tipo

  N = nrow(poblacion)
  
  if(tipo == "MSA"){
    seleccion = sample.int(N, n)
  } else if (tipo == "EST igual provincia" | tipo == "EST prop provincia"){
    if(!("Prob_seleccion" %in% names(poblacion))) stop("Primero hay que asignar Prob_seleccion")

    # en cada provincia se eligen ni = Prob_seleccion * Ni hogares (si Prob_seleccion = 1, es un censo)
    seleccion = poblacion[, .I[ sample.int(.N, round(Prob_seleccion[1] * .N)) ], by = Provincia ]$V1
  } else if (tipo == "EST igual ciudad" | tipo == "EST prop ciudad"){
    if(!("Prob_seleccion" %in% names(poblacion))) stop("Primero hay que asignar Prob_seleccion")

    # en cada ciudad se eligen ni = Prob_seleccion * Ni hogares (si Prob_seleccion = 1, es un censo)
    seleccion = poblacion[, .I[ sample.int(.N, round(Prob_seleccion[1] * .N)) ], by = Ciudad ]$V1
  } else {
    stop("tipo debe ser 'MSA', 'EST igual' o 'EST prop'")
  }

  return(poblacion[seleccion,])
}
