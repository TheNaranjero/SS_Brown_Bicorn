
library(dplyr)
library(data.table)
library(survey)
calibrar = function(muestra, poblacion, edad = TRUE, tipo = "MSA"){
  # Calibra los pesos de la muestra a totales conocidos de la población.
  # Añade a `muestra` (por referencia) la columna Peso_calibracion
  # (0 para los no respondentes).
  # Inputs:
  #   muestra   -- muestra a calibrar (con Responde, Ajuste_no_respuesta, Prob_seleccion, Provincia)
  #   poblacion -- población (con Provincia, Edad_0_14, Edad_15_64, Edad_65_99)
  #   edad      -- si TRUE, calibra también por grupo de edad
  #   tipo      -- tipo de muestreo: "MSA", "EST igual" o "EST prop"
  #                MSA calibra al total de hogares; los EST, al total de hogares de cada provincia

  if (!(tipo %in% c("MSA", "EST igual", "EST prop"))) stop("tipo debe ser 'MSA', 'EST igual' o 'EST prop'")

  # solo los respondentes entran en la calibración
  es_resp = as.logical(muestra$Responde)
  resp    = muestra[es_resp]
  resp[, peso_base := Ajuste_no_respuesta / Prob_seleccion]

  # totales conocidos de habitantes por grupo de edad (opcional)
  vars          = if (edad) c("Edad_0_14", "Edad_15_64", "Edad_65_99") else character(0)
  totales_edad  = colSums(poblacion[, ..vars])

  if (tipo == "MSA"){
    resp[, uno := 1]                       # para que los pesos sumen N
    totales = c(uno = nrow(poblacion), totales_edad)
    vars_formula = c("uno", vars)

    diseno = survey::svydesign(ids = ~1, weights = ~peso_base, data = resp)

  } else {   # EST igual, EST prop
    # un total por provincia: los pesos de cada provincia suman su N_h
    provincias = sort(unique(poblacion$Provincia))
    N_h        = poblacion[, .N, keyby = Provincia]$N
    resp[, Provincia := factor(Provincia, levels = provincias)]
    if (any(table(resp$Provincia) == 0)) stop("Hay provincias sin respondentes, no se puede calibrar")

    totales = c(setNames(N_h, paste0("Provincia", provincias)), totales_edad)
    vars_formula = c("Provincia", vars)

    diseno = survey::svydesign(ids = ~1, strata = ~Provincia, weights = ~peso_base, data = resp)
  }

  formula = reformulate(vars_formula, intercept = FALSE)   # ~ 0 + uno + ...  o  ~ 0 + Provincia + ...

  cal = survey::calibrate(
    diseno,
    formula    = formula,
    population = totales,       # los nombres deben coincidir con los de la fórmula
    calfun     = "linear",
    bounds     = c(0.1, Inf)    # factor de ajuste g = peso_calibrado / peso_base
  )

  muestra[, Peso_calibracion := 0]
  muestra[es_resp, Peso_calibracion := weights(cal)]

  return(muestra[])
}