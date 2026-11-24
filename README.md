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

## Version 4
- Extendidas las variables ocultas a 5
- Añadido el efecto provincia y ciudad
- Ahora Media_Habitantes_Por_Hogar y Tipo_urbano (de una única ciudad a varios pueblos) tiene efecto en el código.
- Añadido Sexo y Educacion (primario, secundario, terciario)
- No respuesta: añadir MAR y ajuste MAR
- Variables auxiliares: Permitir elegir que variable auxiliar usar: Edad, Sexo, Educacion y a que nivel Pais, Provincia, Ciudad
- 





# Sobre las variables ocultas
Se generan 5 variables independientes, con números del 1 al 999 (`var_hidden_1` ... `var_hidden_5`).

Luego se les suma un efecto por provincia (entre -200 y +200) y un efecto por ciudad (entre -100 y +100), distinto para cada variable. De esta forma los hogares de una misma provincia o ciudad se parecen entre sí.

Ya no hay una variable oculta asociada a una variable manifesta en particular: todas las variables ocultas participan de todas las variables manifestas, con coeficientes aleatorios.

Las variables ocultas se eliminan al final y no forman parte del resultado.

# Ciudades y provincias
El parámetro `tipo_urbano` define cómo se distribuyen los hogares entre ciudades:

1. 90% de los hogares en una única ciudad, el resto en 100 pueblos pequeños.
2. 4 ciudades grandes, 5 ciudades medianas y 52 pueblos pequeños.
3. 25 ciudades medianas, 25 ciudades pequeñas y 101 pueblos pequeños.
4. 10 ciudades medianas, 60 ciudades pequeñas y 100 pueblos pequeños.
5. 1000 pueblos pequeños, todos del mismo tamaño.

Cada ciudad tiene al menos 10 hogares. Cada ciudad se asigna a una provincia, y todas las provincias tienen al menos una ciudad (por eso se necesitan al menos tantas ciudades como provincias).

# Variables manifestas:
Para cada variable manifesta se definen los valores:

bi = un valor entre -1 y +1, distinto para cada variable manifesta
aux = b1 * var_hidden_1 + .... + b5 * var_hidden_5
u = pnorm( aux, mean = mean(aux), sd = sd(aux) )

`u` es un valor entre 0 y 1 (aproximadamente uniforme) que depende de las variables ocultas. Se usa como probabilidad (en las binomiales) o como cuantil (en las funciones `q...`) de cada variable manifesta. De este modo, las variables manifestas quedan correlacionadas con las ocultas, y por lo tanto entre sí, pero cada una con la distribución que se quiera.

La media y el desvío de `aux` se calculan con los datos (no de forma teórica), porque los efectos de provincia y ciudad cambian la distribución de las variables ocultas.

## Ingreso
Ingreso: qgamma( u , shape = 4, rate = 4 / MEDIA_INGRESOS ) * ruido

El ruido es un valor entre 0.95 y 1.05. La gamma con `shape = 4` tiene media MEDIA_INGRESOS y pocos valores cercanos a cero.

## Jefe de hogar
jefe_adulto: rbinom( 1, prob = u )

Vale 1 si el jefe de hogar tiene entre 15 y 64 años, y 0 si tiene entre 65 y 99. Es una variable auxiliar, se elimina al final.

## Cantidad por edad
Cada hogar tiene un jefe de hogar, que se suma de forma segura al grupo de edad que corresponde. El resto de los habitantes de cada grupo sigue una Poisson con media (MEDIA_HABITANTES_POR_HOGAR - 1) / 3.

Edad_0_14: qpois( u, lambda = (MEDIA_HABITANTES_POR_HOGAR - 1) / 3 )
Edad_15_64: qpois( u, lambda = (MEDIA_HABITANTES_POR_HOGAR - 1) / 3 ) + jefe_adulto
Edad_65_99: qpois( u, lambda = (MEDIA_HABITANTES_POR_HOGAR - 1) / 3 ) + 1 - jefe_adulto

Cada grupo usa su propio `u`.

## Cantidad de habitantes del hogar
Total = Edad_0_14 + Edad_15_64 + Edad_65_99

Todos los hogares tienen al menos 1 habitante, y en promedio MEDIA_HABITANTES_POR_HOGAR.

## Sexo
Sexo_femenino: rbinom( Total, prob = u acotado entre 0.25 y 0.75 )
Sexo_masculino = Total - Sexo_femenino

La probabilidad se acota con `pmin` y `pmax` para evitar hogares con casi todas mujeres o casi todos varones.

## Educación
Se aplica solamente a los adultos del hogar (Edad_15_64 + Edad_65_99).

Educacion_primario: rbinom( Adultos, prob = u )
Educacion_secundario: rbinom( Adultos - Educacion_primario, prob = u )
Educacion_terciario = Adultos - Educacion_primario - Educacion_secundario

Cada nivel usa su propio `u`.

# Parámetros y constantes

MEDIA_INGRESOS = Media de ingresos nacional por hogar, estimada con la EPH.

MEDIA_HABITANTES_POR_HOGAR = Media de habitantes por hogar.

seed = Semilla para que los resultados sean reproducibles (por defecto 1234).