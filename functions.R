library(dplyr)
library(data.table)

#+ Valores por defecto
N_POR_DEFECTO = 1000

#Valores por defecto
MEDIANA_INGRESOS = 2800  # en miles
MEDIA_INGRESOS = MEDIANA_INGRESOS/ log(2) # calcula la media en base a la mediana de una distribución exponencial. Usa la EPH como fuente
ERROR_INGRESOS = 0.05    # ruido. Error promedio, porcentual, a aplicarse sobre los ingresos
MEDIA_HABITANTES_POR_HOGAR = 2.5
#+ Constantes para evitar bugs

MILLON = 10^6
MIL = 10^3


#+ Funciones para generar la poblacion y muestra

generar_poblacion = function(N){
  # Genera una matriz con un id hogar y una variable escondida.
  # Inputs:
  # N -- tamaño poblacional
  # Outputs:
  # Matrix Nx3 con las variables id_hogar y var_hidden_1 

  

  output = data.table(
    id_hogar = seq_len(N),
    var_hidden_1 = sample.int(MILLON-1,size = N, replace=TRUE), # números del 1 al 999_999
    var_hidden_2 = sample.int(MILLON-1,size = N, replace=TRUE) # asociado al tipo de hogar/cantidad de habitantes

  )
  return(output)

}


asigna_provincias <- function(matriz, n_provincias) {

  # Proporcion de hogares aleatorias
  proporciones <- runif(n_provincias)
  proporciones <- proporciones / sum(proporciones)

  # Asignar provincia a cada hogar
  matriz[, provincia := sample.int(
    n_provincias,
    size = .N,
    replace = TRUE,
    prob = proporciones
  )]
}


efectos = function(matriz){
  # Añade un efecto 'provincia': atrae las var_hidden hacia un centro de cada provincia.
  # Modifica `matriz` por referencia; las var_hidden quedan como integer.

  setDT(matriz) # convierte a data.table por si acaso

  provincias   = unique(matriz$provincia)
  n_provincias = length(provincias)
  var_hidden   = c("var_hidden_1", "var_hidden_2")

  prob_atraccion = runif(n_provincias, min = 0.3, max = 0.7)

  centro = sapply(var_hidden, function(v){
    runif(n_provincias, min = min(matriz[[v]]), max = max(matriz[[v]]))
  })

  idx = match(matriz$provincia, provincias)
  a_i = prob_atraccion[idx]

  for (v in var_hidden){
    matriz[, (v) := as.integer(round(get(v) + a_i * (centro[idx, v] - get(v))))]
  }

  invisible(matriz)
}
  











crea_variables = function(matriz){
  # Genera variables manifestas a través de las variables escondidas
  # input una matriz donde al menos una columna tenga el nombre "var_hidden_1"
  # output misma matriz, pero con una columna adicional llamada "ingresos"
  # generación de la variable 'ingresos': se utiliza la distribución exponencial, partiendo de los ingresos medianos por defecto
  # elimina las variables var_hidden_1


  N = nrow(matriz)

  
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

    return( as.integer( qexp(prop, rate = rate) * ruido ) )
  }

  

var_grupo_edad <- function(v1, v2, media_habitantes=2.5,
                                TH = readr::read_csv("TIPO_HOGAR.csv", show_col_types = FALSE)) {
  # Genera tipo de hogar y cantidad de habitantes por grupo de edad
  # Inputs:
  #   v1  -- variable escondida del ingreso (uniforme en [0,1]); se usa para asociar tipo e ingreso
  #   v2  -- variable escondida del tipo de hogar (uniforme en [0,1])
  #   media_habitantes -- esperanza del tamaño del hogar (1 a 10)
  #   TH  -- tabla de tipos (columnas: ninos, adultos, mayores, jefe_adulto), ordenada como anillo
  #   amp -- ancho mínimo de cada tipo, en millonésimas del anillo
  # Output: lista (tipo, ninos 0-14, adultos 15-64, mayores 65-99)

  N <- length(v1)
  K <- nrow(TH)

  # perfiles normalizados por tipo
  tot <- TH$ninos + TH$adultos + TH$mayores
  p_n <- TH$ninos   / tot
  p_a <- TH$adultos / tot
  p_m <- TH$mayores / tot
  p_jefe_adulto <- TH$jefe_adulto            # P(jefe adulto) por tipo

  # cortes aleatorios ordenados, con ancho mínimo amp (anillo = 1e6)
  cortes <- sort( sample.int(99, K - 1) ) /100

  # posición en el anillo: v1 mueve el tipo, da la vuelta con %% 1
  b1   <- sample(c(-0.15, -0.10, -0.05, 0.05, 0.10, 0.15), 1)
  x    <- ((v2 + b1 * v1)/1e6) %% 1
  tipo <- findInterval(x, c(0, cortes))      # 1..K

  # exceso medio sobre el jefe (el if_else sum 0.1  cuando media_habitantes = 1 )
  lam <- media_habitantes - 1 + if_else(media_habitantes == 1, 0.1 , 0)

  # jefe: un único sorteo por hogar
  jefe_adulto <- runif(N) < p_jefe_adulto[tipo]

  ninos   <- rpois(N, lam * p_n[tipo])
  adultos <- rpois(N, lam * p_a[tipo]) + jefe_adulto
  mayores <- rpois(N, lam * p_m[tipo]) + !jefe_adulto

  return( list(ninos, adultos, mayores)  )      # el orden debe coincidir con := 
}

  # Crea las variables
  matriz[, Ingresos :=
    var_ingresos(v1=var_hidden_1, v2=var_hidden_2)
  ]
 
  matriz[,  c("Edad_0_14", "Edad_15_64", "Edad_65_99") :=
     var_grupo_edad(v1 = var_hidden_1, v2 = var_hidden_2,
                         media_habitantes = 2.5)]


  #elimina las variables escondidas

  matriz[,c("var_hidden_1", "var_hidden_2") := NULL]



  return(matriz)
}


asignar_probabilidad_de_seleccion = function(matriz, n=n, tipo = "MSA"){
  # Añade una columna a matriz que contiene la probabilidad de seleccion n/N
  # inputs:
  # matriz -- matriz que contiene la población
  # n -- tamaño de la muestra
  # tipo -- tipo de muestra, por defecto MSA
  # output:
  # matriz con la población más una columna adicional con la probabilidad de seleccion 'prob_seleccion'
  # Manejo de errores:
  # n menor a uno o mayor a N -- convierte en N
  # n decimal -- trunca (floor())
  # tipo no considerado en la formula -- convierte a MSA
  N = nrow(matriz)
  
  if(n<1){
    print( "n debe ser al menos 1, se convierte a censo n=N")
    n = N
  }
  if(n>N){
    print( "n debe ser a lo sumo igual a N, se convierte a censo n=N")
    n = N
  }
  if(n>floor(n)){
    print("n debe ser un número entero, se trunca")
    n = floor(n)
  }
  if( !(tipo %in% c("MSA") ) ){
    print("tipo de muestreo desconocido, se utilizará muestreo simple al azar")
    tipo = 'MSA'
  }  
  prob = as.integer(n/N * MILLON)
  matriz[, Prob_seleccion :=
      rep( prob, times=N)
    ]
  

  return(matriz)



}

seleccionar_muestra = function(matriz, n=n , tipo = "MSA"){
  #Selecciona una muestra simple al azar. La guarda como una columna 'seleccionada' con valor 1 si fue seleccionada y 0 si no.
  # inputs:
  # matriz -- matriz con la poblacion
  # n -- tamaño de la muestra
  # tipo -- tipo de muestreo a utilizar, por defecto MSA
  # Manejo de errores:
  # n mayor a N, n menor a 1 -- se reemplaza por N
  # n decimal -- trunca (floor())
  # tipo no considerado en la formula -- convierte a MSA 
  N = nrow(matriz)
  
  if(n<1){
    print( "n debe ser al menos 1, se convierte a censo n=N")
    n = N
  }
  if(n>N){
    print( "n debe ser a lo sumo igual a N, se convierte a censo n=N")
    n = N
  }
  if(n>floor(n)){
    print("n debe ser un número entero, se trunca")
    n = floor(n)
  }
  if( !(tipo %in% c("MSA") ) ){
    print("tipo de muestreo desconocido, se utilizará muestreo simple al azar")
    tipo = 'MSA'
  } 

  seleccion = sample.int(N, n)


  return(matriz[seleccion,])
}

asignar_no_respuesta = function(matriz, tipo = "MSA", tipo_no_respuesta = 'MCAR', tasa_no_respuesta = 0.0){
  # Crea una columna de ceros y unos, donde 1 representa responde y 0 no responde
  # Inputs:
  # matriz -- muestra o poblacion
  # tipo -- tipo de muestreo, por defecto simple al azar
  # tipo_no_respuesta:  
  #       MCAR -- asigna la respuesta al azar
  #       MAR -- SIN IMPLEMENTAR  asigna la respuesta basandose en un modelo con las variables explicativas
  #       MNAR -- SIN IMPLEMENTAR  asigna la respuesta basandose en la variable respuesta
  # tasa -- proporción de no respuesta
  n = nrow(matriz)
  
 if(tipo_no_respuesta == 'MCAR'){
   responde = (runif(n = n) > tasa_no_respuesta)
   
 }

  matriz[, Responde :=
      responde
    ]
  

  return(matriz)}


ajustar_no_respuesta = function(matriz, tipo = "MSA", tipo_ajuste = 'MCAR'){
  # Crea una columna de ceros y unos, donde 1 representa responde y 0 no responde
  # Inputs:
  # matriz -- muestra o poblacion
  # tipo -- tipo de muestreo, por defecto simple al azar
  # tipo ajuste:  
  #       MCAR -- asume que la no respuesta fue completamente al azar, así que corrige por la tasa de no respuesta
  #       MAR -- SIN IMPLEMENTAR  asigna la respuesta basandose en un modelo con las variables explicativas
  n = nrow(matriz)
  
 if(tipo_ajuste == 'MCAR'){
   tasa_respuesta = sum( matriz$Responde) / n
   
 }
  ajuste = as.integer(1/tasa_respuesta * MILLON) 
  matriz[, Ajuste_no_respuesta :=
      rep(ajuste, times = n)
    ]
  

  return(matriz)}



calibrar = function( muestra, poblacion, edad=TRUE, tipo="MSA"){
  # Calcula los totales conocidos de la poblacion, calibra los pesos para ajustar a esos totales.
  # Inputs:
  # muestra -- la muestra obtenida a calibrar.
  # poblacion -- la poblacion
  # edad -- booleano calibra por grupo de edad (0 a 14 años, 15 a 64 años, 65 o más años)
  # sexo -- (sin aplicar) booleano calibra por sexo (femenino, masculion, otro)
  # tipo -- tipo de muestreo utilizado, por ahora solo MSA, dentro de poco Estratificado


  total_0_14 = sum(poblacion[,Habitantes_0_14])
  total_15_64 = sum(poblacion[,Habitantes_15_64])
  total_65_99 = sum(poblacion[,Habitantes_65_99])

  pesos_0_14  = rep(1, times=N)
  pesos_15_64 = rep(1, times=N)
  pesos_64_99 = rep(1, times=N)

  # funcion a minimizar
  # el menor cambio posible a los pesos, penalizando mucho las grandes diferencias
  quad_diff = function(p1, p2, p3){
    return( sum( (p1 - 1)^2 + (p2 -1)^2 + (p3-1)^2 )  )
  }

  # Igualdades a mantener

  eq = function(p1, p2, p3){
   return(c(
    sum(p1) -  length(p1),
    sum(p2) -  length(p2),
    sum(p3) -  length(p3),

   )) 

  }

  # Desigualdades a mantener

  uneq = function(p1, p2, p3){
    return( 
      c(
        (p1<=0) / length(p1) - 0.1 ,
        (p2<=0) / length(p2) - 0.1 ,
        (p3<=0) / length(p3) - 0.1 ,


      )


    )


  }


  total =sum( 1/1e6 *  matriz$Ingresos * matriz$Responde * matriz$Ajuste_no_respuesta / matriz$Prob_seleccion * 1e6  )










}
#+ Estimaciones

estimar_total = function(matriz, tipo = "MSA"){
  # Estima el total de ingresos, usando las columnas seleccion y prob_seleccion, es decir el estimador de horvit thompson
  # total = ingresos * seleccion * 1 / prob_seleccion
  # inptus:
  # matriz -- poblacion con las columnas ingreso, prob_seleccion y seleccion
  # tipo -- tipo de muestreo, por defecto MSA
  # outputs
  # estimacion del total
  # Manejo de errores:
  # seleccion debe ser 0 o 1 -- utiliza un censo



  total =sum( 1/MILLON *  matriz$Ingresos * matriz$Responde * matriz$Ajuste_no_respuesta / matriz$Prob_seleccion * MILLON  )
  return(total)
}







#+ MAPA



# Suaviza una matriz promediando cada celda con sus 8 vecinas (bordes replicados)
# Suaviza un vector (promedio móvil circular, varias pasadas)

suaviza_1d = function(v, k = 15, pasadas = 3){
  for (i in seq_len(pasadas)) v = as.numeric(stats::filter(v, rep(1/k, k), circular = TRUE))
  v
}

# Perfil de un límite: profundidad (en celdas) del "afuera" a lo largo del lado
# sinuosidad: fracción del largo del límite que es sinuoso (0, 0.5 o 1)



perfil_limite = function(n, amp, sinuosidad){
  ondas = suaviza_1d(rnorm(n))
  ondas = ondas / max(abs(ondas))                    # en [-1, 1]

  if (sinuosidad >= 1) {
    mascara = rep(1, n)
  } else if (sinuosidad <= 0) {
    mascara = rep(0, n)
  } else {
    azar = suaviza_1d(rnorm(n), k = 25, pasadas = 2) # tramos largos
    mascara = as.numeric(azar >= quantile(azar, 1 - sinuosidad))
    mascara = suaviza_1d(mascara, k = 9, pasadas = 1) # transición sin saltos
  }
  round(amp + amp * mascara * ondas)                 # 0 .. 2*amp
}

genera_mapa = function(n = 100, n_ciudades = 10, semilla = NULL, amp = 10,
                       tipos      = NULL,
                       sinuosidad = NULL){
  if (!is.null(semilla)) set.seed(semilla)
  
  if(is.null(tipos)){
    sample = sample(c("terreste", "maritimo"), size = 4 , replace =TRUE)
    tipos      = c(N =sample[1], E = sample[2], S = sample[3], O = sample[4])
  }
  if(is.null(sinuosidad)){
    sample = sample(c(0, 0.3, 0.5, 1,1,1,1, 1, 1), size = 4 , replace =FALSE)
    sinuosidad      = c(N =sample[1], E = sample[2], S = sample[3], O = sample[4])
  }

  # Los marítimos son siempre 100% sinuosos
  sinuosidad[tipos == "maritimo"] = 1

  # Un perfil por lado
  d = lapply(names(tipos), function(l) perfil_limite(n, amp, sinuosidad[[l]]))
  names(d) = names(tipos)

  # Coordenadas: x = columna (oeste->este), y = fila (sur->norte)
  X = matrix(rep(1:n, times = n), n, n)
  Y = matrix(rep(1:n, each  = n), n, n)

  # Margen respecto a cada límite (>0 = adentro del país)
  margenes = list(
    N = (n + 1 - Y) - matrix(d$N[X], n, n),
    E = (n + 1 - X) - matrix(d$E[Y], n, n),
    S = Y           - matrix(d$S[X], n, n),
    O = X           - matrix(d$O[Y], n, n)
  )
  arr = simplify2array(margenes)                     # n x n x 4
  min_margen = apply(arr, c(1, 2), min)
  lado_critico = apply(arr, c(1, 2), which.min)      # 1=N 2=E 3=S 4=O

  # Códigos: 0 = país, 1 = mar, 2 = otro país
  tipo_lado = ifelse(tipos[c("N", "E", "S", "O")] == "maritimo", 1, 2)
  codigo = ifelse(min_margen > 0, 0, tipo_lado[lado_critico])
  codigo = matrix(codigo, n, n)

  # Ciudades: una por celda, solo dentro del país
  celdas = which(codigo == 0, arr.ind = TRUE)
  elegidas = celdas[sample(nrow(celdas), n_ciudades), , drop = FALSE]

  list(codigo = codigo, ciudades = elegidas, n = n)
}

dibuja_mapa = function(mapa){
  n = mapa$n
  image(1:n, 1:n, mapa$codigo,
        col = c("khaki", "lightblue", "grey70"), zlim = c(0, 2),
        asp = 1, axes = FALSE, xlab = "", ylab = "")
  abline(h = (0:n) + 0.5, v = (0:n) + 0.5, col = rgb(0, 0, 0, 0.08))
  points(mapa$ciudades[, 1], mapa$ciudades[, 2], pch = 19, col = "black", cex = 1.5)
}



genera_provincias <- function(mapa, n_provincias) {

  n <- mapa$n

  # Celdas que pertenecen al país
  celdas <- which(mapa$codigo == 0, arr.ind = TRUE)

  if (n_provincias < 1 || n_provincias > nrow(celdas)) {
    stop("Cantidad de provincias inválida")
  }

  # Seleccionar centros provinciales
  centros <- celdas[
    sample(nrow(celdas), n_provincias),
    ,
    drop = FALSE
  ]

  # Coordenadas de todas las celdas
  coordenadas <- cbind(
    x = celdas[, 2],
    y = celdas[, 1]
  )

  # Distancia al centro provincial más cercano
  distancias <- sapply(seq_len(n_provincias), function(i) {

    dx <- coordenadas[, 1] - centros[i, 2]
    dy <- coordenadas[, 2] - centros[i, 1]

    dx^2 + dy^2
  })

  # Provincia más cercana
  asignacion <- max.col(-distancias)

  # Matriz provincial
  provincias <- matrix(NA_integer_, n, n)
  provincias[celdas] <- asignacion

  # Guardar resultados
  mapa$provincias <- provincias
  mapa$centros_provinciales <- centros
  mapa$n_provincias <- n_provincias

  mapa
}



dibuja_provincias <- function(mapa) {

  n <- mapa$n
  k <- mapa$n_provincias

  # Matriz para graficar
  z <- mapa$provincias + 2

  # Recuperar mar y países vecinos
  z[mapa$codigo == 1] <- 1
  z[mapa$codigo == 2] <- 2

  colores <- c(
    "lightblue",
    "grey70",
    hcl.colors(k, "Dynamic")
  )

  image(
    1:n, 1:n, z,
    col = colores,
    zlim = c(1, k + 2),
    asp = 1,
    axes = FALSE,
    xlab = "",
    ylab = ""
  )

  # Cuadrícula
  abline(
    h = (0:n) + 0.5,
    v = (0:n) + 0.5,
    col = rgb(0, 0, 0, 0.08)
  )

  # Centros provinciales
  points(
    mapa$centros_provinciales[, 2],
    mapa$centros_provinciales[, 1],
    pch = 4,
    col = "black",
    cex = 1.5,
    lwd = 2
  )
}




dibuja_mapa_completo <- function(mapa) {

  n <- mapa$n
  k <- mapa$n_provincias

  # Matriz de visualización
  z <- mapa$provincias + 2

  # Recuperar mar y países vecinos
  z[mapa$codigo == 1] <- 1
  z[mapa$codigo == 2] <- 2

  # Colores
  colores <- c(
    "lightblue", 
    "grey70",
    hcl.colors(k, "Dynamic")
  )

  # Dibujar mapa
  image(
    1:n, 1:n, z,
    col = colores,
    zlim = c(1, k + 2),
    asp = 1,
    axes = FALSE,
    xlab = "",
    ylab = ""
  )

  # Cuadrícula
  abline(
    h = (0:n) + 0.5,
    v = (0:n) + 0.5,
    col = rgb(0, 0, 0, 0.08)
  )

  # Centros provinciales
  points(
    mapa$centros_provinciales[, 2],
    mapa$centros_provinciales[, 1],
    pch = 4,
    col = "black",
    cex = 1.5,
    lwd = 2
  )

  invisible(mapa)
}
