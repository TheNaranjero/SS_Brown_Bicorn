# Sampling Simulator Brown Bicorn



## Version  1
Este archivo tiene como objetivo convertise en el MVP. Cumpliendo las siguientes funciones:
- Simular una única variable escondida por unidad (hogar).
- A partir de esa variable escondida, calcular una única variable respuesta (ingresos).
- Simular una población de N hogares.
- Tomar una muestra al azar de n hogares.
- Estimar el total de ingresos.
- Subir a github.
- Desplegar como una página interactiva.

##  Version 2
Se añadieron:
- No respuesta (solo MCAR).
- Dar la opción de corregir por no respuesta (solo MCAR).
- Generar un mapa simple, solamente decorativo.
- Mostrar Muestreo y Resultados en la misma pestaña.
- Optimizar el uso de la RAM:
  - Uso de numeros enteros en lugar de doubles.
  - Descartar variables escondidas.
  - Reducir el máximo de hogares de 10,000,000 a 1,000,000
  Todo ello permitió pasar de 480 MB a 20MB de consumo en RAM.


  
##  Version 3
Añadir:
- Provincias
- Variables escondidas representando el efecto provincia.
- Personas por edad 
- Calibracion
- Separado las funciones en modulos


# Sobre las variables ocultas
Se generan 5 variables independientes, con números del 1 al 1000.

var_hidden_1 : asociada principalmente al percentil del ingreso
var_hidden_2 : asociada al tipo de hogar según la cantidad de adultos.
var_hidden_3 : 
var_hidden_4 : 
var_hidden_5 : 
# Variables manifestas:
Para cada variable manifesta se definen los valores:
bi = un valor entre -0.15 y +0.15
aux = b1 * var_hidden_1 + .... + b2 *var_hidden_5 

## Ingreso
x = abs(aux + var_hidden_1)
max = max(x) + 1
Ingresos: qexp( x / max , rate = 1 / 1e3  )

## Cantidad de habitantes del hogar
x = abs(aux + var_hidden_2)
max = max(x) + 1
Total: 1 + qpois( x / max , media = MEDIA_HABITANTES_HOGAR  )

## Cantidad por edad
x = abs(aux + var_hidden_3)
max = max(x) + 1

Edad_0_14: qpois( x/ max,  )
Edad_15_64:
Edad_65_99:


# Constantes

MEDIA_INGRESOS = Media de ingresos nacional por hogar, estimada con la EPH.
