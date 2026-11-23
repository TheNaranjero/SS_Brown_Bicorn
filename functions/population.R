# Este modulo contiene todas las funciones asociadas a la generación de la población
#
# genera_poblacion: crea un data.table con las variables escondidas
# asigna_provincias: a cada hogar del genera_poblacion le asigna una provincia
# efectos: añade un efecto provincia, ciudad y barrio a las variables escondidas
# crea_variables: en función de las variables escondidas, crea las variables observables (Ingresos, Edad, Sexo,...)


library(dplyr)
library(data.table)
library(survey)

#+ Valores por defecto
N_POR_DEFECTO = 1000

#Valores por defecto
MEDIANA_INGRESOS = 2800  # en miles
MEDIA_INGRESOS = MEDIANA_INGRESOS/ log(2) # calcula la media en base a la mediana de una distribución exponencial. Usa la EPH como fuente
ERROR_INGRESOS = 0.05    # ruido. Error promedio, porcentual, a aplicarse sobre los ingresos
MEDIA_HABITANTES_POR_HOGAR = 2.5

generar_poblacion = function(N){
  # Genera un data.table con un id hogar y variables escondidas.
  # Inputs:
  # N -- tamaño poblacional
  # Outputs:
  # Matrix Nx3 con las variables id_hogar y las var_hidden 

  

  output = data.table(
    id_hogar = seq_len(N),
    var_hidden_1 = sample.int(1e6-1,size = N, replace=TRUE), # números del 1 al 999_999, asociado al ingreso
    var_hidden_2 = sample.int(1e6-1,size = N, replace=TRUE) # asociado al tipo de hogar/cantidad de habitantes
  )
  return(output)

}

asigna_provincias <- function(poblacion, n_provincias) {
  # Asigna cada hogar a una provincia, 
  # la proporción de hogares que se lleva una provincia se determina con uan uniforme

  # Proporcion de hogares aleatorias
  proporciones <- runif(n_provincias, min=0.01)
  proporciones <- proporciones / sum(proporciones)

  # Asignar provincia a cada hogar
  poblacion[, provincia := sample.int(
    n_provincias,
    size = .N,                  # .N  = nrow(poblacion)
    replace = TRUE,
    prob = proporciones
  )]
}

efectos = function(poblacion){
  # Añade un efecto 'provincia': atrae las var_hidden hacia un centro de cada provincia.
  # Modifica `poblacion` por referencia; las var_hidden quedan como integer.

  provincias   = unique(poblacion$provincia)
  n_provincias = length(provincias)
  var_hidden   = c("var_hidden_1", "var_hidden_2")

  prob_atraccion = runif(n_provincias, min = 0.3, max = 0.7)
  
  # Elige un valor central de cada var_hidden para cada provincia
  
  centro = sapply(var_hidden, function(var){
    runif(n_provincias, 
      min = min( provincias[[var]]), 
      max = max( provincias[[var]]))
  })
  
  # idx es el indice de cada provincia 
  idx = match( poblacion$provincia, provincias )
  a_i = prob_atraccion[idx]

  # Cada provincia tiene una probabilidad de atraer (promediar) 
  # sus valores de var_hidden con los del hogar
  random = runif(nrow(poblacion))
  for (var in var_hidden){
    poblacion[, (var) := as.integer(
      round(
        get(var) + (random < a_i) * 0.5 * (centro[idx, var] - get(var)) 
      )
    )]
  }

  
}

## Crea variables



crea_variables = function(poblacion){
  # Genera variables manifestas a través de las variables escondidas
  # input un data.table donde al menos una columna tenga el nombre "var_hidden_1"
  # output misma data.table, pero con una columna adicional llamada "ingresos"
  # generación de la variable 'ingresos': se utiliza la distribución exponencial, partiendo de los ingresos medianos por defecto
  # elimina las variables var_hidden_1


  N = nrow(poblacion)

  
  # funciones para cada variable
  var_ingresos = function(v1, v2){
    # utiliza var_hidden_1 y el ruido para calcular el ingreso de la familia
    # input v1: variable escondida a utilizar
    b2 = sample(c(-0.15,  -0.1, -0.05,0, 0.5, 0.1, 0.15),1)
    prop = (v1+b2*v2) / 1e6
    prop = pmax(pmin(prop, 0.9999), 0.0001)
    rate = 1/MEDIA_INGRESOS
    size = length(v1)
    ruido = runif( n = size, min = 1 - ERROR_INGRESOS  ,  max = 1+ERROR_INGRESOS) #error porcentual a la variable ingresos

    return( as.integer( 
      qexp(prop, rate = rate) * ruido
     ) 
    )
  }

  

var_grupo_edad <- function(v1, v2, 
  media_habitantes=2.5) {
  # Genera tipo de hogar y cantidad de habitantes por grupo de edad
  # Inputs:
  #   v1  -- variable escondida del ingreso (uniforme, entero entre 1 y 1e6); se usa para asociar tipo e ingreso
  #   v2  -- variable escondida del tipo de hogar (uniforme, entero entre 1 y 1e6)
  #   media_habitantes -- esperanza del tamaño del hogar (1 a 10)
  #   TH  -- tabla de tipos (columnas: ninos, adultos, mayores, jefe_adulto), ordenada como anillo
  #   amp -- ancho mínimo de cada tipo, en millonésimas del anillo
  # Output: lista (tipo, ninos 0-14, adultos 15-64, mayores 65-99)

  N <- length(v1)
  TH = readr::read_csv("TIPO_HOGAR.csv", show_col_types = FALSE)
  K <- nrow(TH)

  # perfiles normalizados por tipo
  tot <- TH$ninos + TH$adultos + TH$mayores
  p_n <- TH$ninos   / tot
  p_a <- TH$adultos / tot
  p_m <- TH$mayores / tot
  p_jefe_adulto <- TH$jefe_adulto            # P(jefe adulto) por tipo

  # cortes aleatorios ordenados
  cortes <- sort( sample.int(99, K - 1) ) /100

  # posición en el anillo: v1 mueve el tipo, da la vuelta con %% 1
  b1   = sample(c(-0.15, -0.10, -0.05, 0.05, 0.10, 0.15), 1)
  x    = ((v2 + b1 * v1)/1e6) %% 1
  x    = x + (x<0)  #si es negativo, da la vuelta -0.10 -->  -0.10 + 1 = 0.90
  tipo <- findInterval(x, c(0, cortes))      # 1..K

  # habitantes sin contar al jefe 
  # (el if_else suma +0.1  cuando media_habitantes = 1, para añadir ruido )
  lam <- media_habitantes - 1 + if_else(media_habitantes == 1, 0.1 , 0)

  # jefe: un único sorteo por hogar, puede ser adulto o mayor
  jefe_adulto = runif(N) < p_jefe_adulto[tipo]

  ninos   = rpois(N, lam * p_n[tipo])
  adultos = rpois(N, lam * p_a[tipo]) + jefe_adulto
  mayores = rpois(N, lam * p_m[tipo]) + !jefe_adulto

  return( list(ninos, adultos, mayores)  )      # el orden debe coincidir con := 
}

  # Crea las variables
  poblacion[, Ingresos :=
    var_ingresos(v1=var_hidden_1, v2=var_hidden_2)
  ]
 
  poblacion[,  
    c("Edad_0_14", "Edad_15_64", "Edad_65_99") :=
     var_grupo_edad(
      v1 = var_hidden_1, 
      v2 = var_hidden_2,
      media_habitantes = 2.5)]


  #elimina las variables escondidas

  matriz[,c("var_hidden_1", "var_hidden_2") := NULL]

}