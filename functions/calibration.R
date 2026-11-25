library(data.table)
library(survey)

calibrar = function(muestra, poblacion,
                    tipo      = "MSA",     # diseño de la muestra
                    nivel     = "pais",    # nivel de los totales: "pais" o "provincia"
                    edad      = FALSE,
                    sexo      = FALSE,
                    educacion = FALSE){
  # Calibra los pesos a totales conocidos de la población.
  # Añade a `muestra` (por referencia) la columna Peso_calibracion
  # (0 para los no respondentes).
  #
  # muestra   -- con Responde, Ajuste_no_respuesta, Prob_seleccion, Provincia, Ciudad,
  #              y las columnas de calibración
  # poblacion -- una fila por hogar, con Provincia, Ciudad, Edad_*, Sexo_*, Educacion_*
  # tipo      -- "MSA", "EST igual provincia", "EST prop provincia",
  #              "EST igual ciudad", "EST prop ciudad"
  #              (igual / prop no cambian el código: ya vienen en Prob_seleccion)
  # nivel     -- nivel geográfico de los totales (independiente de `tipo`)

  tipos = c("MSA", "EST igual provincia", "EST prop provincia",
            "EST igual ciudad", "EST prop ciudad")
  if (!(tipo %in% tipos))
    stop("tipo debe ser uno de: ", paste(tipos, collapse = ", "))
  if (!(nivel %in% c("pais", "provincia")))
    stop("nivel debe ser 'pais' o 'provincia'")

  # nivel de los estratos según el tipo
  nivel_estrato = if (tipo == "MSA") "ninguno"
             else if (grepl("ciudad", tipo)) "ciudad"
             else "provincia"

  # --- clave geográfica: la ciudad se identifica junto con su provincia
  clave = function(dt, niv){
    switch(niv,
      pais      = rep("Pais", nrow(dt)),
      provincia = as.character(dt$Provincia),
      ciudad    = paste(dt$Provincia, dt$Ciudad, sep = " | "))
  }

  # --- variables numéricas (una categoría menos por bloque, queda implícita)
  vars_num = "Habitantes"
  if (edad)      vars_num = c(vars_num, "Edad_0_14", "Edad_15_64")
  if (sexo)      vars_num = c(vars_num, "Sexo_femenino")
  if (educacion) vars_num = c(vars_num, "Educacion_primario", "Educacion_secundario")

  # --- datos de trabajo (copias, para no modificar las tablas del usuario)
  pop = copy(poblacion)
  pop[, Habitantes := Edad_0_14 + Edad_15_64 + Edad_65_99]

  es_resp = as.logical(muestra$Responde)
  resp    = copy(muestra[es_resp])
  resp[, Habitantes := Edad_0_14 + Edad_15_64 + Edad_65_99]
  resp[, peso_base  := Ajuste_no_respuesta / Prob_seleccion]

  # --- zonas de calibración
  zonas    = sort(unique(clave(pop, nivel)))
  zona_pop = factor(clave(pop,  nivel), levels = zonas)
  zona_res = factor(clave(resp, nivel), levels = zonas)

  vacias = zonas[table(zona_res) == 0]
  if (length(vacias))
    stop("Zonas sin respondentes: ", paste(vacias, collapse = "; "))

  # --- matriz de calibración: una columna por zona (hogares)
  #     y una por zona x variable (habitantes, edad, etc.)
  armar_matriz = function(dt, zona){
    G = outer(as.integer(zona), seq_along(zonas), "==") * 1   # dummies de zona
    colnames(G) = zonas
    bloques = list(G)
    for (v in vars_num) {
      B = G * as.numeric(dt[[v]])
      colnames(B) = paste(zonas, v, sep = ":")
      bloques[[v]] = B
    }
    do.call(cbind, bloques)
  }
  X_pop = armar_matriz(pop,  zona_pop)
  X_res = armar_matriz(resp, zona_res)

  # --- quitar restricciones que la muestra no puede sostener
  #     (columnas nulas o linealmente dependientes en los respondentes)
  esc = sqrt(colSums(X_res^2))
  esc[esc == 0] = 1
  q    = qr(sweep(X_res, 2, esc, "/"), tol = 1e-8)
  keep = sort(q$pivot[seq_len(q$rank)])

  if (length(keep) < ncol(X_res))
    message("Calibración: se descartaron ", ncol(X_res) - length(keep), " restricciones: ",
            paste(colnames(X_res)[-keep], collapse = "; "))

  X_res = X_res[, keep, drop = FALSE]
  X_pop = X_pop[, keep, drop = FALSE]

  # --- nombres simples (los de las zonas tienen espacios, "|" y ":", que no
  #     sirven en una fórmula)
  nombres_x = paste0("cal_x", seq_len(ncol(X_res)))
  colnames(X_res) = nombres_x
  colnames(X_pop) = nombres_x

  totales = colSums(X_pop)                       # mismos nombres que la fórmula

  # las columnas de la matriz pasan a ser variables de resp
  resp = cbind(resp, as.data.table(X_res))

  # --- diseño (los estratos dependen de `tipo`)
  if (nivel_estrato == "ninguno") {
    diseno = svydesign(ids = ~1, weights = ~peso_base, data = resp)
  } else {
    resp[, Estrato := clave(resp, nivel_estrato)]
    diseno = svydesign(ids = ~1, strata = ~Estrato, weights = ~peso_base, data = resp)
  }

  # --- calibrar
  formula = reformulate(nombres_x, intercept = FALSE)    # ~ 0 + cal_x1 + cal_x2 + ...
  cal = calibrate(diseno, formula = formula, population = totales, calfun = "linear")

  muestra[, Peso_calibracion := 0]
  muestra[es_resp, Peso_calibracion := weights(cal)]
  muestra[]
}