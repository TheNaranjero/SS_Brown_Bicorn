
library(dplyr)
library(data.table)
library(survey)

calibrar = function(muestra, poblacion, edad = TRUE, tipo = "MSA"){
  # Calibra los pesos de la muestra a totales conocidos de la población.
  # Añade a `muestra` (por referencia) la columna peso_calibrado
  # (0 para los no respondentes).
  # Inputs:
  #   muestra   -- muestra a calibrar (con Responde, Ajuste_no_respuesta, Prob_seleccion)
  #   poblacion -- población (con Habitantes_0_14, Habitantes_15_64, Habitantes_65_99)
  #   edad      -- si TRUE, calibra también por grupo de edad
  #   tipo      -- tipo de muestreo; por ahora solo "MSA"

  if (tipo == "MSA"){
     # solo los respondentes entran en la calibración
  es_resp = as.logical(muestra$Responde)
  resp    = muestra[es_resp]
  resp[, peso_base := Ajuste_no_respuesta / Prob_seleccion]
  resp[, uno := 1]                       # para que los pesos sumen N

  # totales conocidos: N y, opcionalmente, habitantes por grupo de edad
  vars    = if (edad) c("Edad_0_14", "Edad_15_64", "Edad_65_99") else character(0)
  totales = c(uno = nrow(poblacion), colSums(poblacion[, ..vars]))

  formula = reformulate(c("uno", vars), intercept = FALSE)   # ~ 0 + uno + ...

  diseno = survey::svydesign(ids = ~1, weights = ~peso_base, data = resp)

  cal = survey::calibrate(
    diseno,
    formula    = formula,
    population = totales,       # los nombres deben coincidir con los de la fórmula
    calfun     = "linear",
    bounds     = c(0.1, Inf)    # factor de ajuste g = peso_calibrado / peso_base
  )

  muestra[, Peso_calibracion := 0]
  muestra[es_resp, Peso_calibracion := weights(cal)]
}
}