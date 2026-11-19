library(dplyr)
library(data.table)

#Valores por defecto en caso de error
N_POR_DEFECTO = 1000

#Valores por defecto
MEDIANA_INGRESOS = 2800  # en miles
MEDIA_INGRESOS = MEDIANA_INGRESOS/ log(2) # calcula la media en base a la mediana de una distribución exponencial. Usa la EPH como fuente
ERROR_INGRESOS = 0.05    # ruido. Error promedio, porcentual, a aplicarse sobre los ingresos

#Constantes para evitar bugs

MILLON = 10^6
MIL = 10^3


#Funciones

generar_poblacion = function(N){
  # Genera una matriz con un id hogar y una variable escondida.
  # Inputs:
  # N -- tamaño poblacional
  # Outputs:
  # Matrix Nx3 con las variables id_hogar y var_hidden_1 
  # Manejo de errores:
  # N decimal: trunca (floor())
  # N negativo, cero o NA: utiliza N_POR_DEFECTO
  
  #Validacion
  if(is.na(N) | N < 1  ){
    print("N invalido, pasando a valores por defecto: N = 1000")
    N = N_POR_DEFECTO
  } else if(N > floor(N)){
    print("N debe ser un número entero, se redondea a", floor(N))
    N = floor(N)
  }
  

  output = data.table(
    id_hogar = seq_len(N),
    var_hidden_1 = sample.int(MILLON-1,size = N, replace=TRUE) # números del 1 al 999_999
  )
  return(output)

}

crea_variables = function(matriz){
  # Genera variables manifestas a través de las variables escondidas
  # input una matriz donde al menos una columna tenga el nombre "var_hidden_1"
  # output misma matriz, pero con una columna adicional llamada "ingresos"
  # generación de la variable 'ingresos': se utiliza la distribución exponencial, partiendo de los ingresos medianos por defecto
  # elimina las variables var_hidden_1


  N = nrow(matriz)

  
  # funciones para cada variable
  var_ingresos = function(v1){
    # utiliza var_hidden_1 y el ruido para calcular el ingreso de la familia
    # input v1: variable escondida a utilizar
    prop = v1/MILLON
    rate = 1/MEDIA_INGRESOS
    size = length(v1)
    ruido = runif( n = size, min = 1 - ERROR_INGRESOS  ,  max = 1+ERROR_INGRESOS) #error porcentual a la variable ingresos

    return( as.integer( qexp(prop, rate = rate) * ruido ) )
  }

  # Crea las variables
  matriz[, Ingresos :=
    var_ingresos(v1=var_hidden_1)
  ]


  #elimina las variables escondidas

  matriz[,c("var_hidden_1") := NULL]



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

## MAPA



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

