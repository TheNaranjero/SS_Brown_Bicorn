library(dplyr)
library(data.table)
library(survey)


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
