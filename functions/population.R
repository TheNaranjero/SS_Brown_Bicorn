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

generar_poblacion = function(N, n_provincias, tipo_urbano, MEDIA_INGRESOS, MEDIA_HABITANTES_POR_HOGAR, seed = 1234){
  # Genera un data.table con un id hogar y variables escondidas.
  # Inputs:
  # N -- tamaño poblacional
  # seed -- semilla para reproducibilidad
  # Outputs:
  # Matrix Nx3 con las variables id_hogar y las var_hidden 

  set.seed(seed)
  
  poblacion = data.table(
    id_hogar = seq_len(N),
    var_hidden_1 = sample.int(1e3-1,size = N, replace=TRUE), 
    var_hidden_2 = sample.int(1e3-1,size = N, replace=TRUE), 
    var_hidden_3 = sample.int(1e3-1,size = N, replace=TRUE), 
    var_hidden_4 = sample.int(1e3-1,size = N, replace=TRUE), 
    var_hidden_5 = sample.int(1e3-1,size = N, replace=TRUE)
  )

  if(tipo_urbano == 1){
    # 90% de la población en una única ciudad, el resto en pequeños pueblos con 0.1% de la poblacion
    prob = 1:101
    prob[1] = 0.9
    prob[2:101] = 0.001
  }
  else if(tipo_urbano == 2){
    # 80% de la población en 4 ciudades grandes,
    # 15% en 5 ciudades medianas
    # el resto en pequeños pueblos con 0.1% de la poblacion
    prob = 1:61
    prob[1:4] = runif(4, min=0.15, max = 0.25)
    prob[(4+1):(4+5)] = runif(5, min=0.02, max = 0.04)
    prob[(9+1):61] = 0.001
    prob = prob / sum(prob)  #normaliza
    
  }
  else if(tipo_urbano == 3){
      # 70% de la población en ciudades medianas
      # 20% en ciudades pequeñas  
      # 10% en pequeños pueblos con 0.1% de la poblacion
      prob = 1:151
      prob[1:25] = runif(25, min=0.02, max = 0.04)
      prob[(25+1):(25+25)] = runif(25, min=0.005, max = 0.015)
      prob[51:151] = 0.001
      prob = prob / sum(prob)  #normaliza
      
  }
  else if(tipo_urbano == 4){
    # 30% de la población en ciudades medianas
    # 60% en ciudades pequeñas  
    # el resto en pequeños pueblos con 0.1% de la poblacion
    prob = 1:170
    prob[1:10] = runif(10, min=0.02, max = 0.04)
    prob[(10+1):(10+60)] = runif(60, min=0.005, max = 0.015)
    prob[(70+1):(70+100)] = 0.001
    prob = prob / sum(prob)  #normaliza
      
  }
  else if(tipo_urbano == 5){
    # 100% en pequeños pueblos con 0.1% de la poblacion
    # 1000 pueblos, cada uno con 100 a 1000 hogares
    prob = rep(0.001, times = 1000)  #normaliza
      
  }
  else{
    stop("tipo_urbano debe ser 1, 2, 3, 4 o 5")
  }

  n_ciudades = length(prob)


  poblacion[, Ciudad:= sample.int(n_ciudades, size = N, replace = TRUE, prob=prob) ]

  ## al menos 10 hogares por ciudad
  ## toma los primeros 10*n_ciudades elementos y asigna 10 a cada ciudad
  poblacion[1:(10*n_ciudades), Ciudad:=rep(1:n_ciudades, each=10)]


  # Asigna provincias

  # Asigna cada ciudad a una provincia, para garantizar que todas las provincias tengan al menos 1 ciudad
  # se asigna primero una ciudad a cada provincia y el resto al azar, luego se mezcla
  
  provincia_ciudad = sample( c(1:n_provincias, sample.int(n_provincias, n_ciudades - n_provincias, replace = TRUE)) )


  # Asignar provincia a cada hogar
  poblacion[, Provincia :=   provincia_ciudad[Ciudad]]


  # Efecto Provincia

  for(var in c("var_hidden_1", "var_hidden_2", "var_hidden_3", "var_hidden_4","var_hidden_5")){
    efecto_provincia = sample(c(-200, -150, -100, -50, 0, 50, 100, 150, 200), n_provincias,replace=TRUE)
    efecto_ciudad = sample(c(-100, -50, -25, 0, 0, 0, 25, 50, 100), n_ciudades,replace=TRUE)
    poblacion[, (var) :=   get(var) + efecto_provincia[Provincia] + efecto_ciudad[Ciudad]]
}
  #CREA VARIABLES

  N = nrow(poblacion)

  
  #Ingreso
  # Combinacion Lineal de las var_hidden --> quantiles normales --> gamma --> añade ruido +- 5%
  # (la combinacion lineal se estandariza con su media y desvio empiricos)
  # gamma con shape = 4: media = MEDIA_INGRESOS, y pocos valores cercanos a cero

  b = runif( 5, min= -1, max= 1 ) # coeficientes

  poblacion[, Ingreso := b[1]*var_hidden_1 + b[2]*var_hidden_2 + b[3]*var_hidden_3 + b[4]*var_hidden_4 + b[5]*var_hidden_5  ]
  poblacion[, Ingreso := qgamma( pnorm( Ingreso, mean = mean(Ingreso), sd = sd(Ingreso)), shape = 4, rate = 4/MEDIA_INGRESOS )* runif(.N, min=0.95, max = 1.05)  ]
  poblacion[, Ingreso := as.numeric(Ingreso)  ]
  # EDADES
  poblacion[ , Edad_0_14  := 0 ]
  poblacion[ , Edad_15_64 := 0 ]
  poblacion[ , Edad_65_99 := 0 ]

  # Jefe de hogar adulto (15 a 64 años) o mayor (65 a 99 años)
  # Combinacion Lineal de las var_hidden --> quantiles normales --> proporcion de una bernoulli 

  b = runif( 5, min= -1, max= 1 ) # coeficientes

  poblacion[, jefe_adulto := b[1]*var_hidden_1 + b[2]*var_hidden_2 + b[3]*var_hidden_3 + b[4]*var_hidden_4 + b[5]*var_hidden_5  ]
  poblacion[, jefe_adulto := rbinom( n=.N, size=1, prob= pnorm( jefe_adulto, mean = mean(jefe_adulto), sd = sd(jefe_adulto)) ) ]

  ## Edad 0-14 
  # Combinacion Lineal de las var_hidden --> quantiles normales --> poisson
  # media de la poisson --> (MEDIA_HABITANTES_POR_HOGAR - 1) / 3
  # (el -1 es por el jefe de hogar, que se suma aparte, asi el promedio del hogar es MEDIA_HABITANTES_POR_HOGAR)
  
  b = runif( 5, min= -1, max= 1 ) # coeficientes

  poblacion[, Edad_0_14 := b[1]*var_hidden_1 + b[2]*var_hidden_2 + b[3]*var_hidden_3 + b[4]*var_hidden_4 + b[5]*var_hidden_5  ]
  poblacion[, Edad_0_14 := qpois( pnorm( Edad_0_14, mean = mean(Edad_0_14), sd = sd(Edad_0_14)), lambda = (MEDIA_HABITANTES_POR_HOGAR - 1) / 3 )  ]

    ## Edad 15-64 

  b = runif( 5, min= -1, max= 1 ) # coeficientes

  poblacion[, Edad_15_64 := b[1]*var_hidden_1 + b[2]*var_hidden_2 + b[3]*var_hidden_3 + b[4]*var_hidden_4 + b[5]*var_hidden_5  ]
  poblacion[, Edad_15_64 := qpois( pnorm( Edad_15_64, mean = mean(Edad_15_64), sd = sd(Edad_15_64)), lambda = (MEDIA_HABITANTES_POR_HOGAR - 1) / 3 ) + jefe_adulto ]

## Edad 65-99 

  b = runif( 5, min= -1, max= 1 ) # coeficientes

  poblacion[, Edad_65_99 := b[1]*var_hidden_1 + b[2]*var_hidden_2 + b[3]*var_hidden_3 + b[4]*var_hidden_4 + b[5]*var_hidden_5  ]
  poblacion[, Edad_65_99 := qpois( pnorm( Edad_65_99, mean = mean(Edad_65_99), sd = sd(Edad_65_99)), lambda = (MEDIA_HABITANTES_POR_HOGAR - 1) / 3 ) + 1 - jefe_adulto ]
# Total habitantes y adultos

 poblacion[, Total := Edad_0_14 + Edad_15_64 + Edad_65_99]
 poblacion[, Adultos := Edad_15_64 + Edad_65_99]

  ## Sexo
  # la probabilidad se acota entre 0.25 y 0.75

  b = runif( 5, min= -1, max= 1 ) # coeficientes

  poblacion[, Sexo_femenino := b[1]*var_hidden_1 + b[2]*var_hidden_2 + b[3]*var_hidden_3 + b[4]*var_hidden_4 + b[5]*var_hidden_5  ]
  poblacion[, Sexo_femenino := rbinom( n= .N, 
    size= Total, 
    prob= pmin( pmax( pnorm( Sexo_femenino, mean = mean(Sexo_femenino), sd = sd(Sexo_femenino)), 0.25), 0.75) )
   ]
  poblacion[, Sexo_masculino :=  Total - Sexo_femenino ]

  ## Educacion (solo adultos)

  # Primario
  b = runif( 5, min= -1, max= 1 ) # coeficientes

  poblacion[, Educacion_primario := b[1]*var_hidden_1 + b[2]*var_hidden_2 + b[3]*var_hidden_3 + b[4]*var_hidden_4 + b[5]*var_hidden_5  ]
  poblacion[, Educacion_primario := rbinom( n= .N, 
    size= Adultos, 
    prob= pnorm( Educacion_primario, mean = mean(Educacion_primario), sd = sd(Educacion_primario)) )
   ]
  
  # Secundario

  b = runif( 5, min= -1, max= 1 ) # coeficientes

  poblacion[, Educacion_secundario := b[1]*var_hidden_1 + b[2]*var_hidden_2 + b[3]*var_hidden_3 + b[4]*var_hidden_4 + b[5]*var_hidden_5  ]
  poblacion[, Educacion_secundario := rbinom( n= .N, 
    size= Adultos - Educacion_primario, 
    prob= pnorm( Educacion_secundario, mean = mean(Educacion_secundario), sd = sd(Educacion_secundario)) )
   ]
  
  # Terciario

  poblacion[, Educacion_terciario := Adultos - Educacion_primario - Educacion_secundario ]







  #elimina las variables escondidas

  poblacion[,c("var_hidden_1", "var_hidden_2", "var_hidden_3", "var_hidden_4", "var_hidden_5", "jefe_adulto", "Adultos") := NULL]

  return(poblacion[])

}