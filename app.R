library(shiny)
library(dplyr)
library(data.table)

source("functions.R")

# 1. Interfaz

ui <- navbarPage(

  title = "IXE",

  tabPanel(
    title = "Inicio",
    h2("Bienvenido al Instituto Xianoes de Estadistica"),
    hr(),
    h3("Genera tu país"),
    hr(),

    wellPanel(
      h4("Población"),
      sliderInput(
        inputId = "cantidad_hogares",
        label = "Cantidad de hogares",
        min = 100000,
        max = 1000000,
        step = 100000,
        value = 1000000
      ),
      p("LOS SIGUIENTES NO HACEN NADA:"),
      numericInput(
        inputId = "habitantes_hogar",
        label = "Cantidad de personas por hogar (promedio)",
        min = 1,
        max = 10,
        step = 0.5,
        value = 2.5
      ),
      numericInput(
        inputId = "cantidad_provincias",
        label = "Cantidad de provincias",
        min = 1,
        max = 100,
        step = 1,
        value = 10
      )
    ),
    br(),
    wellPanel(
      h4("No respuesta"),
      fluidRow(
        column(
          width = 3,
          radioButtons(
            inputId = "tasa_no_respuesta",
            label = "Tasa de no respuesta",
            choices = c("Muy baja" =  0.05, "Baja" = 0.10, "Moderada" = 0.20, "Alta" = 0.35, "Muy alta" = 0.50, "Extrema" = 0.75)
          )
        ),
        column(
          width = 3,
          radioButtons(
            inputId = "tipo_no_respuesta",
            label = "Tipo de no respuesta",
            choices = c("Completamente al azar (MCAR)" = "MCAR")
          )
        )
      )
    ),
    br(),
    p("LOS SIGUIENTES NO HACEN NADA:"),

    wellPanel(
      h4("Ciudades"),
      fluidRow(
        column(
          width = 2,
          numericInput(
            inputId = "cantidad_megametropolis",
            label = "Mega-Metropolis",
            min = 0, max = 5, step = 1, value = 0
          )
        ),
        column(
          width = 2,
          numericInput(
            inputId = "cantidad_metropolis",
            label = "Metropolis",
            min = 0, max = 20, step = 1, value = 0
          )
        ),
        column(
          width = 2,
          numericInput(
            inputId = "cantidad_grandes_ciudades",
            label = "Grandes ciudades",
            min = 0, max = 100, step = 1, value = 10
          )
        ),
        column(
          width = 2,
          numericInput(
            inputId = "cantidad_ciudades",
            label = "Ciudades",
            min = 0, max = 1000, step = 10, value = 10
          )
        ),
        column(
          width = 2,
          numericInput(
            inputId = "cantidad_pueblos",
            label = "Pueblos",
            min = 0, max = 10000, step = 100, value = 100
          )
        )
      )
    ),
    p("LOS SIGUIENTES NO HACEN NADA:"),

    wellPanel(
      h4("Geografía"),
      fluidRow(
        column(
          width = 4,
          radioButtons(
            inputId = "altura",
            label = "Altura",
            choices = c("Plano", "Montaña", "Cordillera")
          )
        ),
        column(
          width = 4,
          radioButtons(
            inputId = "vegetacion",
            label = "Vegetación",
            choices = c("Pasto", "Bosques", "Selvático")
          )
        ),
        column(
          width = 4,
          radioButtons(
            inputId = "hidrologia",
            label = "Hidrologia",
            choices = c("Rios", "Lagos", "Mesopotamico")
          )
        )
      )
    ),
    actionButton(
      inputId = "genera_poblacion",
      label = "Generar Poblacion"
    ),
    br(),
    br(),
    br(),
    br()
  ),

  tabPanel(
    "País",
    h2("Resumen del país"),
    hr(),
    plotOutput("mapa", height = "600px", width = "600px")
  ),

  tabPanel(
    "Muestreo",
    h2("Diseño muestral"),
    wellPanel(
      fluidRow(
        column(
          width = 3,
          selectInput(
            inputId = "tipo_muestreo",
            label = "Diseño muestral",
            choices = c("Muestreo Simple al Azar" = "MSA")
          )
        ),
        column(
          width = 3,
          numericInput(
            inputId = "tamano_muestral",
            label = "Tamaño muestral",
            min = 10, max = 1000000, step = 10, value = 1000
          )
        ),
        column(
          width = 3,
          selectInput(
            inputId = "tipo_ajuste_no_respuesta",
            label = "Ajustar por no respuesta",
            choices = c("Completamente al azar (MCAR)" = "MCAR")
          )
        )
      )
    ),
    actionButton(
      inputId = "tomar_muestra",
      label = "Tomar Muestra"
    ),
    wellPanel(
      title = "Resultados de la simulación",
      h3("Resultados de la muestra"),
      textOutput("resultado_estimador"),
      textOutput("resultado_error"),
      textOutput("resultado_respuesta"),
      textOutput("resultado_personas_por_hogar")

      
    )

  ),

  
)

# 2. Lógica
server <- function(input, output, session) {

  # Contenedores reactivos
  pob <- reactiveVal(NULL)
  resultados <- reactiveValues(total_millones = NULL, error = NULL)

  # Genera la población
  observeEvent(input$genera_poblacion, {
    print("¡Generando población!")

    tasa_no_respuesta = as.numeric(input$tasa_no_respuesta)

    poblacion <- generar_poblacion(N = input$cantidad_hogares) |>
      asigna_provincias(n_provincias = 5) |>
      crea_variables() |> 
      asignar_no_respuesta(tipo_no_respuesta = input$tipo_no_respuesta, tasa_no_respuesta = tasa_no_respuesta )

    pob(poblacion)
    print(head(pob()))
  })

  # Toma la muestra
  observeEvent(input$tomar_muestra, {
    req(pob())
    print("¡Tomando muestra!")

    n    <- input$tamano_muestral
    tipo <- input$tipo_muestreo
    ajuste_no_respuesta <- input$tipo_ajuste_no_respuesta

    muestra <- pob() |>
      asignar_probabilidad_de_seleccion(n = n, tipo = tipo) |>
      seleccionar_muestra(n = n, tipo = tipo) |>
      ajustar_no_respuesta(tipo = tipo, tipo_ajuste = ajuste_no_respuesta)

    total <- muestra |> estimar_total(tipo = tipo)
    real  <- pob() |> pull("Ingresos") |> sum()
    respondentes = muestra$Responde |> sum()
    Edad_0_14  = muestra$Edad_0_14  |> sum()
    Edad_15_64  = muestra$Edad_15_64  |> sum()
    Edad_65_99  = muestra$Edad_65_99  |> sum()
    personas_por_hogar = (Edad_0_14+Edad_15_64+Edad_65_99) / respondentes



    resultados$total_millones <- round(total / 1e3, 0)
    resultados$error          <- round((total - real) / real * 100, 2)
    resultados$pct_respuesta   <- round((respondentes) / n * 100, 2)
    resultados$personas_por_hogar   <- round((personas_por_hogar), 2)




    print(head(muestra))
    print(total)
  })

  # Outputs
  output$resumen <- renderText({
    paste("Pob:", input$cantidad_hogares * input$habitantes_hogar / 1000000, "millones")
  })

  output$resultado_estimador <- renderText({
    req(resultados$total_millones)
    paste0("Total: $", resultados$total_millones, " Millones")
  })

  output$resultado_error <- renderText({
    req(resultados$error)
    paste0("Error: ", resultados$error, "%")
  })

  output$resultado_respuesta <- renderText({
    req(resultados$pct_respuesta)
    paste0("Tasa de respuesta: ", resultados$pct_respuesta, "%")
  })

  output$resultado_personas_por_hogar <- renderText({
    req(resultados$personas_por_hogar)
    paste0("Personas porhogar: ", resultados$personas_por_hogar)
  })
  ## Generacion del mapa
    mapa = eventReactive(input$genera_poblacion, {

      genera_mapa(
        n = 100, n_ciudades = 5, amp = 10,
      ) |> 
        genera_provincias(n_provincias=105)
    })


    output$mapa = renderPlot({
      dibuja_mapa_completo(mapa())
    })
  

}

# 3. Aplicación
shinyApp(ui = ui, server = server)