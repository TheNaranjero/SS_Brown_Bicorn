# Imagen base con R
FROM rocker/r-ver:4.4.1

# Paquetes que usa la app 
RUN install2.r --error shiny dplyr ggplot2 data.table

WORKDIR /app
COPY app.R functions.R ./

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]