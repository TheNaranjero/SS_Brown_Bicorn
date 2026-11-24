library(dplyr)
library(data.table)
library(survey)

#+ MAPA
# =============================================================================
#  Convención de coordenadas (la misma que usa image()):
#    x = primer índice de la matriz (fila), y = segundo índice (columna).

# =============================================================================


# -----------------------------------------------------------------------------
# 1) GENERACIÓN DEL PAÍS 
# -----------------------------------------------------------------------------

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


# -----------------------------------------------------------------------------
# 2) UTILIDADES DE SUAVIZADO 2D
# -----------------------------------------------------------------------------

# Suaviza una matriz con un promedio móvil separable (ventana (2*radio+1)^2,
# bordes replicados). Con varias pasadas se aproxima a un filtro gaussiano.
suaviza_2d = function(m, radio = 2, pasadas = 2){
  radio = round(radio)
  if (radio < 1) return(m)
  k = 2 * radio + 1
  nucleo = rep(1 / k, k)
  n1 = nrow(m); n2 = ncol(m)
  for (p in seq_len(pasadas)) {
    pad = rbind(m[rep(1, radio), , drop = FALSE], m, m[rep(n1, radio), , drop = FALSE])
    m = apply(pad, 2, function(v) as.numeric(stats::filter(v, nucleo))[radio + seq_len(n1)])
    pad = cbind(m[, rep(1, radio), drop = FALSE], m, m[, rep(n2, radio), drop = FALSE])
    m = t(apply(pad, 1, function(v) as.numeric(stats::filter(v, nucleo))[radio + seq_len(n2)]))
  }
  m
}

# Sube la resolución de una matriz por interpolación bilineal (factor f).
# La malla nueva cubre el mismo rango 1..n, con n*f puntos por eje.
sube_resolucion = function(m, f){
  n1 = nrow(m); n2 = ncol(m)
  xs = seq(1, n1, length.out = n1 * f)
  ys = seq(1, n2, length.out = n2 * f)
  a = apply(m, 2, function(v) approx(1:n1, v, xout = xs)$y)       # (n1*f) x n2
  t(apply(a, 1, function(v) approx(1:n2, v, xout = ys)$y))        # (n1*f) x (n2*f)
}

# Texto con halo blanco (legible sobre cualquier color de fondo)
texto_halo = function(x, y, txt, col = "black", halo = "white", d = 0.3, ...){
  for (a in seq(0, 2 * pi, length.out = 9)[-9])
    text(x + d * cos(a), y + d * sin(a), txt, col = halo, ...)
  text(x, y, txt, col = col, ...)
}


# -----------------------------------------------------------------------------
# 3) DICCIONARIOS Y NOMBRES AL AZAR
#    DICCIONARIO[[idioma]]$adjetivos[[lista]]   y   $sustantivos[[lista]]
#    (agrega idiomas o listas libremente)
# -----------------------------------------------------------------------------

DICCIONARIO = list(
  espanol = list(
    adjetivos = list(
      colores = c(
        "Rojo", "Blanco", "Negro", "Verde", "Azul",
        "Dorado", "Gris", "Violeta", "Amarillo", "Naranja",
        "Rosa", "Marrón", "Celeste", "Turquesa", "Carmesí",
        "Índigo", "Beige", "Plateado", "Escarlata", "Oliva"
      ),
      rasgos = c(
        "Grande", "Alto", "Ancho", "Antiguo", "Bravo",
        "Sereno", "Fiel", "Austral", "Pequeño", "Profundo",
        "Noble", "Valiente", "Hermoso", "Fértil", "Lejano",
        "Sagrado", "Luminoso", "Oscuro", "Silvestre", "Eterno"
      )
    ),
    sustantivos = list(
      naturaleza = c(
        "Valle", "Río", "Monte", "Bosque", "Lago",
        "Llano", "Cerro", "Costa", "Sierra", "Cordillera",
        "Desierto", "Glaciar", "Cañón", "Laguna", "Pantano",
        "Cascada", "Estepa", "Selva", "Pradera", "Volcán"
      ),
      animales = c(
        "Puma", "Cóndor", "Zorro", "Jaguar", "Ñandú",
        "Guanaco", "Yacaré", "Vizcacha", "Lobo", "Águila",
        "Ciervo", "Halcón", "Venado", "Carpincho", "Tigre",
        "Oso", "Pecarí", "Nutria", "Cóndor", "Armadillo"
      )
    )
  ),

  quechua = list(
    adjetivos = list(
      colores = c(
        "Puka", "Yuraq", "Yana", "Qumir", "Anqas",
        "Q'illu", "Ch'umpi", "Kulli", "Ch'iqchi", "Q'illu",
        "Panti", "Ch'ixi", "Q'omer", "Q'ello", "Uqi",
        "Q'umir", "Yana-yana", "Puka-puka", "Anqas-anqas", "Yuraq-yuraq"
      ),
      rasgos = c(
        "Hatun", "Uchuy", "Sumaq", "Machu", "Ch'aki",
        "Sinchi", "Hanaq", "Uray", "Musuq", "Ñawpa",
        "Kallpa", "Allin", "Yuyay", "K'acha", "Llamp'u",
        "Ch'ulla", "Apu", "Karu", "Qhapaq", "Kusisqa"
      )
    ),
    sustantivos = list(
      naturaleza = c(
        "Mayu", "Urqu", "Qucha", "Sach'a", "Pampa",
        "Wayq'u", "Rit'i", "Inti", "Killa", "Rumi",
        "Pukyu", "Yaku", "Sacha", "Puna", "Q'asa",
        "Wayra", "Nina", "Allpa", "Phuyu", "Ch'aska"
      ),
      animales = c(
        "Puma", "Kuntur", "Atuq", "Quwi", "Waman",
        "Llama", "Ukuku", "Taruka", "Añas", "K'enti",
        "K'usillu", "Ch'uspi", "Wallpa", "K'uchi", "Misi",
        "Allqu", "Kuntur", "Ch'iwchi", "K'ita", "Wisk'acha"
      )
    )
  ),

  guarani = list(
    adjetivos = list(
      colores = c(
        "Pytã", "Morotĩ", "Hũ", "Hovy", "Sa'yju",
        "Pytãngy", "Hovyũ", "Pytãju", "Hũngy", "Morotĩngy",
        "Pytãporã", "Hovyete", "Sa'yjuete", "Hũete", "Pytãite",
        "Hovyũngy", "Morotĩete", "Pytãhũ", "Hovyhũ", "Sa'yjuporã"
      ),
      rasgos = c(
        "Guasu", "Mi", "Puku", "Porã", "Tuja",
        "Pyahu", "Mbarete", "Michĩ", "Yvate", "Ijyvate",
        "Pya'e", "Kirirĩ", "Hory", "Pochy", "Marangatu",
        "Katupyry", "Hekoha", "Poti", "Rory", "Pukuete"
      )
    ),
    sustantivos = list(
      naturaleza = c(
        "Ysyry", "Yvyty", "Ka'aguy", "Ñu", "Y",
        "Yvy", "Yvága", "Kuarahy", "Jasy", "Yvoty",
        "Itakua", "Ykua", "Amambái", "Tata", "Ara",
        "Yvytu", "Arapy", "Ka'aguyrusu", "Yvyra", "Itá"
      ),
      animales = c(
        "Jaguarete", "Aguara", "Mbói", "Tapiti", "Kuati",
        "Jagua", "Karumbe", "Guasu", "Tatu", "Mainumby",
        "Tukã", "Pira", "Teju", "Yryvu", "Taguato",
        "Mburika", "Kavaju", "Kururu", "Jakare", "Manduvi"
      )
    )
  ),

  latin = list(
    adjetivos = list(
      colores = c(
        "Rubra", "Alba", "Nigra", "Viridis", "Caerulea",
        "Aurea", "Argentea", "Purpurea", "Flava", "Rosea",
        "Coccinea", "Lutea", "Brunnea", "Atra", "Candida",
        "Rutila", "Fulva", "Glauca", "Pallida", "Violacea"
      ),
      rasgos = c(
        "Magna", "Parva", "Alta", "Longa", "Antiqua",
        "Fortis", "Pulchra", "Nova", "Vetus", "Lata",
        "Profunda", "Nobilis", "Clara", "Aeterna", "Sacra",
        "Silvestris", "Fertilis", "Luminosa", "Bona", "Digna"
      )
    ),
    sustantivos = list(
      naturaleza = c(
        "Vallis", "Mons", "Silva", "Lacus", "Campus",
        "Flumen", "Collis", "Litus", "Desertum", "Glacies",
        "Cascata", "Palus", "Pratum", "Saltus", "Insula",
        "Rupes", "Fons", "Rivus", "Oceanus", "Vulcanus"
      ),
      animales = c(
        "Lupus", "Ursus", "Aquila", "Leo", "Cervus",
        "Equus", "Taurus", "Vulpes", "Canis", "Felis",
        "Corvus", "Falco", "Lepus", "Aper", "Capra",
        "Ovis", "Elephas", "Tigris", "Serpens", "Mustela"
      )
    )
  ),

  ingles = list(
    adjetivos = list(
      colores = c(
        "Red", "White", "Black", "Green", "Blue",
        "Golden", "Grey", "Silver", "Yellow", "Orange",
        "Pink", "Brown", "Purple", "Turquoise", "Crimson",
        "Indigo", "Beige", "Scarlet", "Olive", "Amber"
      ),
      rasgos = c(
        "Great", "Little", "High", "Broad", "Old",
        "Brave", "Silent", "Southern", "Deep", "Noble",
        "Mighty", "Ancient", "Fertile", "Distant", "Sacred",
        "Bright", "Dark", "Wild", "Eternal", "Golden"
      )
    ),
    sustantivos = list(
      naturaleza = c(
        "Valley", "River", "Mountain", "Forest", "Lake",
        "Plain", "Hill", "Coast", "Desert", "Glacier",
        "Canyon", "Lagoon", "Swamp", "Waterfall", "Steppe",
        "Jungle", "Meadow", "Volcano", "Island", "Cliff"
      ),
      animales = c(
        "Wolf", "Bear", "Eagle", "Fox", "Stag",
        "Falcon", "Heron", "Otter", "Lion", "Tiger",
        "Deer", "Hawk", "Bison", "Elk", "Badger",
        "Lynx", "Raven", "Owl", "Horse", "Boar"
      )
    )
  )
)

# Genera n nombres únicos:  idioma_adj[lista_adj][i]  +  idioma_sus[lista_sus][j]
# Los argumentos en NULL se eligen al azar.
genera_nombres = function(n,
                          idioma_adj = NULL, lista_adj = NULL,
                          idioma_sus = NULL, lista_sus = NULL,
                          diccionario = DICCIONARIO, sep = " "){
  if (is.null(idioma_adj)) idioma_adj = sample(names(diccionario), 1)
  if (is.null(idioma_sus)) idioma_sus = sample(names(diccionario), 1)
  if (is.null(lista_adj))  lista_adj  = sample(names(diccionario[[idioma_adj]]$adjetivos), 1)
  if (is.null(lista_sus))  lista_sus  = sample(names(diccionario[[idioma_sus]]$sustantivos), 1)

  adj = diccionario[[idioma_adj]]$adjetivos[[lista_adj]]
  sus = diccionario[[idioma_sus]]$sustantivos[[lista_sus]]
  if (is.null(adj)) stop("No existe la lista de adjetivos '", lista_adj, "' en '", idioma_adj, "'")
  if (is.null(sus)) stop("No existe la lista de sustantivos '", lista_sus, "' en '", idioma_sus, "'")
  if (length(adj) * length(sus) < n)
    stop("Solo hay ", length(adj) * length(sus), " combinaciones posibles y se piden ", n)

  combos   = expand.grid(i = seq_along(adj), j = seq_along(sus))
  elegidos = combos[sample(nrow(combos), n), ]
  paste(adj[elegidos$i], sus[elegidos$j], sep = sep)
}


# -----------------------------------------------------------------------------
# 4) POBLACIÓN: Provincia / Ciudad / habitantes
#    Espera columnas: Provincia, Ciudad, Edad_0_14, Edad_15_64, Edad_65_99
#    (si hay varias filas por ciudad, se suman)
# -----------------------------------------------------------------------------

prepara_poblacion = function(poblacion){
  req = c("Provincia", "Ciudad", "Edad_0_14", "Edad_15_64", "Edad_65_99")
  falta = setdiff(req, names(poblacion))
  if (length(falta)) stop("Faltan columnas en poblacion: ", paste(falta, collapse = ", "))

  poblacion$Provincia  = as.character(poblacion$Provincia)
  poblacion$Ciudad     = as.character(poblacion$Ciudad)
  poblacion$Habitantes = poblacion$Edad_0_14 + poblacion$Edad_15_64 + poblacion$Edad_65_99

  ciudades   = aggregate(Habitantes ~ Provincia + Ciudad, data = poblacion, FUN = sum)
  provincias = aggregate(Habitantes ~ Provincia, data = ciudades, FUN = sum)

  # Ciudad más grande de cada provincia
  ciudades$es_mayor = ave(ciudades$Habitantes, ciudades$Provincia,
                          FUN = function(v) as.numeric(seq_along(v) == which.max(v))) == 1

  list(ciudades = ciudades, provincias = provincias, total = sum(ciudades$Habitantes))
}


# -----------------------------------------------------------------------------
# 5) PROVINCIAS CON BORDES REALISTAS
#
#  - Se sube la resolución de la grilla (factor x factor por celda original).
#  - La costa se suaviza: se difuminan los indicadores país/mar/otro país
#    y se asigna cada celda de alta resolución a la clase dominante.
#  - Las fronteras provinciales son un Voronoi "deformado": antes de buscar el
#    centro más cercano, se desplaza cada punto con ruido suave de dos escalas
#    (domain warping). Da límites sinuosos y orgánicos, no rectas.
#  - Al dibujar, los bordes se trazan con contour() sobre el campo suavizado.
#
#  poblacion        data.frame (ver prepara_poblacion)
#  factor           resolución interna (3 a 5 es buen rango)
#  deformacion      amplitud del ruido como fracción de n (0 = Voronoi recto)
#  umbral_ciudad    mínimo de habitantes para graficar una ciudad que no es la
#                   mayor de su provincia (fracción del total del país)
#  nombres_azar     TRUE = nombres inventados; FALSE = los de poblacion$Provincia
#  opciones_nombres lista de argumentos para genera_nombres(), por ejemplo
#                   list(idioma_adj="quechua", lista_adj="colores",
#                        idioma_sus="espanol", lista_sus="naturaleza")
# -----------------------------------------------------------------------------

genera_provincias = function(mapa, poblacion, factor = 3, deformacion = 0.07,
                             umbral_ciudad = 0.01, nombres_azar = TRUE,
                             opciones_nombres = list(), semilla = NULL){
  if (!is.null(semilla)) set.seed(semilla)

  n   = mapa$n
  pob = prepara_poblacion(poblacion)
  k   = nrow(pob$provincias)                 # nº de provincias = las de la población
  N   = n * factor
  xs  = seq(1, n, length.out = N)

  celdas = which(mapa$codigo == 0, arr.ind = TRUE)   # col 1 = x, col 2 = y
  if (k < 1 || k > nrow(celdas)) stop("Cantidad de provincias inválida")

  # --- Costa y límites con otros países, suavizados -------------------------
  campos = lapply(0:2, function(v)
    sube_resolucion(suaviza_2d((mapa$codigo == v) * 1, radio = 2, pasadas = 2), factor))
  codigo_hr = apply(simplify2array(campos), c(1, 2), which.max) - 1

  # --- Ruido suave de dos escalas para deformar las fronteras ---------------
  ruido = function(radio, amp){
    r = suaviza_2d(matrix(rnorm(n * n), n, n), radio = radio, pasadas = 3)
    sube_resolucion(r / sd(r), factor) * amp
  }
  amp = deformacion * n
  dx  = ruido(8, amp) + ruido(3, amp / 3)
  dy  = ruido(8, amp) + ruido(3, amp / 3)
  X   = matrix(rep(xs, times = N), N, N) + dx
  Y   = matrix(rep(xs, each  = N), N, N) + dy

  # --- Centros y asignación (se reintenta si alguna provincia queda casi vacía)
  intentos = 0
  repeat {
    centros = celdas[sample(nrow(celdas), k), , drop = FALSE]
    dmin = matrix(Inf, N, N)
    prov = matrix(1L, N, N)
    for (i in seq_len(k)) {
      d = (X - centros[i, 1])^2 + (Y - centros[i, 2])^2
      mejor = d < dmin
      prov[mejor] = i
      dmin[mejor] = d[mejor]
    }
    prov[codigo_hr != 0] = NA
    tam = tabulate(prov[!is.na(prov)], nbins = k)
    if (all(tam > 0.1 * mean(tam))) break
    intentos = intentos + 1
    if (intentos > 50) stop("No se pudo repartir el territorio; baja 'deformacion' o las provincias")
  }

  # --- Nombres y tabla de provincias ----------------------------------------
  nombres = if (nombres_azar) do.call(genera_nombres, c(list(n = k), opciones_nombres)) else pob$provincias$Provincia

  tabla = data.frame(Provincia = pob$provincias$Provincia, Nombre = nombres,
                     Habitantes = pob$provincias$Habitantes,
                     cx = NA_real_, cy = NA_real_, stringsAsFactors = FALSE)

  # Etiqueta en el centroide real de la región (si cae afuera, la celda más cercana)
  for (i in seq_len(k)) {
    reg = which(!is.na(prov) & prov == i, arr.ind = TRUE)
    px = xs[reg[, 1]]; py = xs[reg[, 2]]
    cx = mean(px); cy = mean(py)
    ix = which.min(abs(xs - cx)); iy = which.min(abs(xs - cy))
    if (is.na(prov[ix, iy]) || prov[ix, iy] != i) {
      j = which.min((px - cx)^2 + (py - cy)^2)
      cx = px[j]; cy = py[j]
    }
    tabla$cx[i] = cx; tabla$cy[i] = cy
  }

  mapa$provincias_hr       = prov
  mapa$codigo_hr           = codigo_hr
  mapa$xs                  = xs
  mapa$factor              = factor
  mapa$n_provincias        = k
  mapa$centros_provinciales = centros
  mapa$tabla_provincias    = tabla

  # --- Ciudades a graficar ---------------------------------------------------
  ciudades = pob$ciudades
  ciudades$id_prov = match(ciudades$Provincia, tabla$Provincia)
  ciudades = ciudades[ciudades$es_mayor | ciudades$Habitantes >= umbral_ciudad * pob$total, ]
  mapa$ciudades_mapa = ubica_ciudades(mapa, ciudades)
  mapa$poblacion_total = pob$total

  mapa
}


# -----------------------------------------------------------------------------
# 6) UBICACIÓN DE LAS CIUDADES DENTRO DE SU PROVINCIA
#
#  Cada ciudad se sortea entre las celdas de SU provincia, dejando un margen
#  respecto de los bordes. Es el equivalente de tu chequeo "la distancia al
#  centro de su provincia es menor que a cualquier otro centro", pero usando
#  la misma regla (centro más cercano, con el ruido aplicado) con la que se
#  dibujaron las fronteras. Con bordes deformados, el chequeo con distancia
#  euclídea simple ya no coincidiría con lo que se ve en el mapa.
#  La ciudad mayor se sortea cerca del centro; el resto, en cualquier parte,
#  intentando no apilarse (separación mínima = sep * n).
# -----------------------------------------------------------------------------

ubica_ciudades = function(mapa, ciudades, sep = 0.05){
  xs = mapa$xs; prov = mapa$provincias_hr; f = mapa$factor; n = mapa$n
  tabla = mapa$tabla_provincias

  ciudades = ciudades[order(-ciudades$Habitantes), ]
  ciudades$x = NA_real_
  ciudades$y = NA_real_
  colocadas = matrix(numeric(0), 0, 2)

  for (i in seq_len(mapa$n_provincias)) {
    idx = which(ciudades$id_prov == i)       # ya ordenadas de mayor a menor
    if (!length(idx)) next

    region   = !is.na(prov) & prov == i
    interior = suaviza_2d(region * 1, radio = 2 * f, pasadas = 1) > 0.99
    cand     = which(interior & region, arr.ind = TRUE)
    if (nrow(cand) == 0) cand = which(region, arr.ind = TRUE)
    cxy   = cbind(xs[cand[, 1]], xs[cand[, 2]])
    d_ctr = sqrt((cxy[, 1] - tabla$cx[i])^2 + (cxy[, 2] - tabla$cy[i])^2)

    for (j in idx) {
      pool = if (j == idx[1]) which(d_ctr <= quantile(d_ctr, 0.3)) else seq_len(nrow(cxy))
      mejor = NULL; mejor_d = -1
      for (t in 1:60) {
        p = cxy[pool[sample.int(length(pool), 1)], ]
        dm = if (nrow(colocadas) == 0) Inf else
          min(sqrt((colocadas[, 1] - p[1])^2 + (colocadas[, 2] - p[2])^2))
        if (dm > mejor_d) { mejor_d = dm; mejor = p }
        if (dm >= sep * n) break
      }
      colocadas = rbind(colocadas, mejor)
      ciudades$x[j] = mejor[1]
      ciudades$y[j] = mejor[2]
    }
  }
  ciudades
}


# -----------------------------------------------------------------------------
# 7) DIBUJO (reemplaza a dibuja_provincias y dibuja_mapa_completo, que eran
#    casi idénticas)
#
#  escala   "lineal" o "log" para mapear habitantes -> color
#  paleta   cualquier paleta secuencial de hcl.colors() ("YlOrRd", "Blues"...)
# -----------------------------------------------------------------------------

dibuja_mapa_completo = function(mapa, escala = c("lineal", "log"), paleta = "Blues",
                                etiquetas_ciudades = FALSE){
  escala = match.arg(escala)
  k  = mapa$n_provincias
  xs = mapa$xs
  tp = mapa$tabla_provincias

  # Color por población
  tr  = if (escala == "log") log else identity
  v   = tr(tp$Habitantes)
  r   = range(v)
  t01 = if (diff(r) > 0) (v - r[1]) / diff(r) else rep(0.5, k)
  pal = hcl.colors(100, paleta, rev = TRUE)          # claro = pocos, oscuro = muchos
  col_prov = pal[1 + round(99 * t01)]

  z = mapa$provincias_hr
  z[mapa$codigo_hr == 1] = k + 1                     # mar
  z[mapa$codigo_hr == 2] = k + 2                     # otro país

  op = par(no.readonly = TRUE)
  on.exit({ layout(1); par(op) })
  layout(matrix(1:2, 1), widths = c(6, 1))

  # --- Mapa -----------------------------------------------------------------
  par(mar = c(1, 1, 1, 1))
  image(xs, xs, z, col = c(col_prov, "lightblue", "grey70"),
        breaks = seq(0.5, k + 2.5, by = 1),
        asp = 1, axes = FALSE, xlab = "", ylab = "")

  # Fronteras provinciales (curvas de nivel 0.5 del indicador suavizado)
  for (i in seq_len(k)) {
    ind = suaviza_2d((!is.na(mapa$provincias_hr) & mapa$provincias_hr == i) * 1,
                     radio = mapa$factor, pasadas = 1)
    contour(xs, xs, ind, levels = 0.5, drawlabels = FALSE, add = TRUE,
            col = "grey30", lwd = 0.8)
  }
  # Costa y límites internacionales
  pais = suaviza_2d((mapa$codigo_hr == 0) * 1, radio = mapa$factor, pasadas = 1)
  contour(xs, xs, pais, levels = 0.5, drawlabels = FALSE, add = TRUE,
          col = "grey15", lwd = 1.6)

  # Nombres de provincias
  texto_halo(tp$cx, tp$cy, tp$Nombre, font = 2, cex = 0.75, d = 0.4)

  # Ciudades: punto negro; la mayor de cada provincia, punto dentro de círculo
  cs = mapa$ciudades_mapa
  points(cs$x, cs$y, pch = 19, cex = 0.8, col = "black")
  mayores = cs[cs$es_mayor, ]
  points(mayores$x, mayores$y, pch = 1, cex = 1.9, lwd = 1.5, col = "black")
  if (etiquetas_ciudades)
    texto_halo(cs$x, cs$y, cs$Ciudad, cex = 0.6, pos = 4, offset = 0.7, d = 0.25)

  # --- Barra de color ---------------------------------------------------------
  par(mar = c(4, 0.5, 4, 4))
  image(1, seq(0, 1, length.out = 100), matrix(1:100, 1), col = pal,
        axes = FALSE, xlab = "", ylab = "")
  marcas = pretty(range(tp$Habitantes), n = 5)
  marcas = marcas[marcas >= min(tp$Habitantes) & marcas <= max(tp$Habitantes)]
  if (escala == "log") marcas = marcas[marcas > 0]
  if (!length(marcas)) marcas = range(tp$Habitantes)
  pos = if (diff(r) > 0) (tr(marcas) - r[1]) / diff(r) else rep(0.5, length(marcas))
  axis(4, at = pos, las = 1, cex.axis = 0.7,
       labels = format(marcas, big.mark = ".", decimal.mark = ",", scientific = FALSE))
  mtext("Habitantes por provincia", side = 4, line = 3, cex = 0.8)
  box()

  invisible(mapa)
}


# -----------------------------------------------------------------------------
# EJEMPLO DE USO
# -----------------------------------------------------------------------------

# Población de juguete para probar (reemplazala por la tuya)
ejemplo_poblacion = function(n_prov = 8, ciudades_por_prov = 6){
  filas = do.call(rbind, lapply(seq_len(n_prov), function(p){
    tam = exp(rnorm(1, 13, 0.6))                      # tamaño relativo de la provincia
    data.frame(Provincia = paste0("Prov_", p),
               Ciudad    = paste0("Ciudad_", p, "_", seq_len(ciudades_por_prov)),
               Edad_0_14   = round(tam * rexp(ciudades_por_prov) * 0.02),
               Edad_15_64  = round(tam * rexp(ciudades_por_prov) * 0.05),
               Edad_65_99  = round(tam * rexp(ciudades_por_prov) * 0.01))
  }))
  filas
}

if (FALSE) {
  set.seed(1)
  poblacion = ejemplo_poblacion()

  mapa = genera_mapa(n = 100, semilla = 42)
  mapa = genera_provincias(mapa, poblacion, factor = 3, deformacion = 0.07,
                           umbral_ciudad = 0.01,
                           opciones_nombres = list(idioma_adj = "quechua", lista_adj = "colores",
                                                   idioma_sus = "espanol", lista_sus = "naturaleza"))
  dibuja_mapa_completo(mapa)                          # escala lineal
  dibuja_mapa_completo(mapa, escala = "log")          # útil si hay una provincia gigante
}