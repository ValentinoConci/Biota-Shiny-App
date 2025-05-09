library(shiny)
library(shinyjs)
library(bs4Dash)
library(DBI)
library(RMariaDB)
library(DT)
library(bslib)
library(ggplot2)
library(dplyr)
library(echarts4r)
library(plotly)

#Conexión a la base de datos
con <- dbConnect(
  RMariaDB::MariaDB(),  # Usar MariaDB en vez de MySQL
  dbname = "Biota_83p_1_0",
  host = "localhost",
  user = "root",     # cambiá por tu user
  password = "Anclus_28" # cambiá por tu pass
)

# Obtener las 7 tablas y guardarlas
tablas <- list(
  pacientes = dbReadTable(con, "pacientes"),
  metadata = dbReadTable(con, "metadata"),
  qc_metrics = dbReadTable(con, "qc_metrics"),
  abundance_species = dbReadTable(con, "abundance_species"),
  gc_counts = dbReadTable(con, "gc_counts"),
  pathways_values = dbReadTable(con, "pathways_values"),
  pathways_description = dbReadTable(con, "pathways_description")
)

#module render tablas
render_tabla <- function(data) {
  datatable(data, filter = 'top', extensions = 'Buttons',
            options = list(dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel'), scrollX = TRUE, scrollY = '400px',
                           pageLength = 15, lengthMenu = c(5, 10, 15, 25, 50, 100), autoWidth = TRUE),
            class = 'display nowrap stripe hover', rownames = FALSE)
}


# INTERFACES --------------------------------------------------

# UI de login
login_ui <- fluidPage(
  useShinyjs(),
  
  # Fondo celeste claro
  tags$head(
    tags$style(HTML("
      .login-background {
        background-color: #e0f7fa;
        height: 100vh;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      #login_card {
        background-color: white;
        padding: 30px;
        border-radius: 15px;
        box-shadow: 0 4px 10px rgba(0,0,0,0.1);
        max-width: 400px;
        margin: auto;
        text-align: center;
      }
      #login_logo {
        max-width: 200px;
        margin-bottom: 20px;
      }
    "))
  ),
  
  # Contenedor centrado
  div(
    class = "login-background",
    div(
      id = "login_card",
      img(src = "biota_logo.png", id = "login_logo"),
      textInput("usuario", "Usuario"),
      passwordInput("clave", "Contraseña"),
      actionButton("login_btn", "Ingresar", class = "btn btn-primary")
    )
  )
)

# UI de BIOTA con bs4Dash
biota_ui <- bs4DashPage(
  title = "BIOTA Dashboard",
  
  header = bs4DashNavbar(
    skin = "light",
    status = "white",
    border = TRUE,
    controlbarIcon = NULL,
    rightUi = NULL,
    title = tags$img(src = "biota_logo.png", height = "50px", style = "margin-left: 15px;")
  ),
  
  sidebar = bs4DashSidebar(
    skin = "dark",
    status = "primary",
    title = "Menú",
    brandColor = "primary",
    elevation = 3,
    opacity = 0.9,
    bs4SidebarMenu(
      id = "biota_menu",
      bs4SidebarMenuItem("Indicadores", tabName = "indicadores", icon = icon("dashboard")),
      bs4SidebarMenuItem("QC Warnings", tabName = "qc", icon = icon("exclamation-triangle")),
      bs4SidebarMenuItem("Tablas", tabName = "tablas", icon = icon("table"))
    )
  ),
  
  body = bs4DashBody(
    tags$head(
      tags$style(HTML("
      body, .content-wrapper, .main-header, .main-sidebar {
        font-size: 16px !important;
      }

      .small-box h3 {
        font-size: 2em !important;
      }

      .small-box p {
        font-size: 1.2em !important;
      }

      .nav-sidebar .nav-link {
        font-size: 1.1em !important;
      }
      .card-title {
        font-size: 1.4em !important;
        font-weight: 600;
      }

      .plot-output {
        font-size: 1.1em;
      }
      
    "))
    ),
    bs4TabItems(
      bs4TabItem(
        tabName = "indicadores", h1("Dashboard de Indicadores"),
        fluidRow(
          bs4ValueBoxOutput("vb_total_pacientes"),
          bs4ValueBoxOutput("vb_hombres"),
          bs4ValueBoxOutput("vb_mujeres"),
        ),
        fluidRow(
          column(
            width = 4,
            bs4Card(
            title = "Distribución Rango Etario",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            echarts4rOutput("pie_rangos", height = "400px")
          )
          ),
          bs4Card(
            title = "Distribución Tipo de Piel",
            width = 4,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            echarts4rOutput("plot_piel", height = '400px')
          ),
          bs4Card(
            title = "Distribución de Edad por Sexo",
            width = 4,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            echarts4rOutput("plot_edad_promedio", height = '400px')
          )
        ),
        fluidRow(
          column(
            width = 6,
            # Filtro para el rango de fechas
            dateRangeInput("rango_fechas",
                           label = "Seleccionar rango de fechas",
                           start = as.Date("2023-01-01"),
                           end = Sys.Date(),
                           min = as.Date("2000-01-01"),
                           max = Sys.Date()),
            
            # Gráfico para mostrar la cantidad de pacientes por fecha
            plotlyOutput("plot_fecha_cita", height = "400px")
            
          ),
          column(
            width = 6,
            box(title = "Tabla de Pacientes", status = "primary", DTOutput("tabla_pacientes"), width = 12)
          )
        )
      ),
      bs4TabItem(tabName = "qc", h2("Aquí irán los gráficos de QC")),
      bs4TabItem(
        tabName = "tablas",
        bs4Card(
          title = "Tablas de Base de Datos",
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          width = 12,
          tabsetPanel(
            #tabPanel("Pacientes", DTOutput("tabla_pacientes")),
            tabPanel("Metadata", DTOutput("tabla_metadata")),
            tabPanel("QC Metrics", DTOutput("tabla_qc_metrics")),
            tabPanel("Abundance Species", DTOutput("tabla_abundance_species")),
            tabPanel("GC Counts", DTOutput("tabla_gc_counts")),
            tabPanel("Pathways Values", DTOutput("tabla_pathways_values")),
            tabPanel("Pathways Description", DTOutput("tabla_pathways_description"))
          )
        )
      )
    )
  ),
  
  controlbar = NULL,
  footer = bs4DashFooter(left = "BIOTA © 2025", right = "v1.0"),
  fullscreen = TRUE
)



# UI de paciente
paciente_ui <- fluidPage(
  title = "Portal Paciente",
  # Encabezado con logo y título
  fluidRow(
    column(4, tags$img(src = "biota_logo.png", height = "60px")),
    column(8, h2("Portal del Paciente ID = 37", style = "margin-top: 15px; font-weight:bold;"))
  ),
  hr(),
  
  # Información general
  fluidRow(
    column(12,
           tags$h3("Información General", style = "text-align:left; font-weight:bold;"),
           DTOutput("tabla_info_paciente")
    )
  ),
  hr(),
  
  # Imagen del perfil taxonómico
  fluidRow(
    column(12,
           tags$div(
             style = "border: 1px solid #ccc; padding: 15px; border-radius: 5px;",
             tags$h3("Perfil Taxonómico", style = "text-align:center; font-weight:bold;"),
             tags$img(src = "grafico_p37.png", width = "50%", style = "display:block; margin:auto;")
           )
    )
  ),
  hr(),
  
  # Tablas de Vías Metabólicas y Abundancia Bacteriana
  fluidRow(
    column(6,
           tags$h4("Vías Metabólicas", style = "text-align:center; font-weight:bold;"),
           DTOutput("tabla_pathways")
    ),
    column(6,
           tags$h4("Abundancia Bacteriana", style = "text-align:center; font-weight:bold;"),
           DTOutput("tabla_abundancia")
    )
  ),
  hr(),
  
  # Botón de descarga centrado y grande
  fluidRow(
    column(12, align = "center",
           downloadButton("descargar_reporte", "Descargar Reporte",
                          class = "btn btn-primary btn-lg")
    )
  ),
  hr(),
  hr()
)





# UI de doctor
doctor_ui <- fluidPage(
  h2("Resultados del doctor")
)

# APP UI general
ui <- fluidPage(
  useShinyjs(),
  uiOutput("main_ui")
)

#SERVER ----------------------------------------------

server <- function(input, output, session) {
  user_type <- reactiveVal(NULL)
  
  observeEvent(input$login_btn, {
    usr <- input$usuario
    pwd <- input$clave
    
    if (usr == "biota" && pwd == "admin") {
      user_type("biota")
    } else if (usr == "paciente_37" && pwd == "documento") {
      user_type("paciente")
    } else if (usr == "doctor" && pwd == "matricula") {
      user_type("doctor")
    } else {
      showModal(modalDialog("Usuario o contraseña incorrectos", easyClose = TRUE))
    }
  })
  
  output$main_ui <- renderUI({
    tipo <- user_type()
    if (is.null(tipo)) {
      login_ui
    } else if (tipo == "biota") {
      biota_ui
    } else if (tipo == "paciente") {
      paciente_ui
    } else if (tipo == "doctor") {
      doctor_ui
    }
  })
  
  
  # Render de tablas para BIOTA
  output$tabla_pacientes <- renderDT({ render_tabla(tablas$pacientes) })
  output$tabla_metadata <- renderDT({ render_tabla(tablas$metadata) })
  output$tabla_qc_metrics <- renderDT({ render_tabla(tablas$qc_metrics) })
  output$tabla_abundance_species <- renderDT({ render_tabla(tablas$abundance_species) })
  output$tabla_gc_counts <- renderDT({ render_tabla(tablas$gc_counts) })
  output$tabla_pathways_values <- renderDT({ render_tabla(tablas$pathways_values) })
  output$tabla_pathways_description <- renderDT({ render_tabla(tablas$pathways_description) })
  
  #cosas de server para TABITEM INDICADORES
  # 1. Total de pacientes
  output$vb_total_pacientes <- renderbs4ValueBox({
    total_pacientes <- nrow(tablas$pacientes)
    bs4ValueBox(
      value = total_pacientes,
      subtitle = "Total de Pacientes",
      icon = icon("users"),
      color = "primary"
    )
  })
  
  # 2. Número de hombres
  output$vb_hombres <- renderbs4ValueBox({
    hombres <- sum(tablas$pacientes$sexo == "Masculino", na.rm = TRUE)
    bs4ValueBox(
      value = hombres,
      subtitle = "Hombres",
      icon = icon("mars"),
      color = "lightblue"
    )
  })
  
  # 3. Número de mujeres
  output$vb_mujeres <- renderbs4ValueBox({
    mujeres <- sum(tablas$pacientes$sexo == "Femenino", na.rm = TRUE)
    bs4ValueBox(
      value = mujeres,
      subtitle = "Mujeres",
      icon = icon("venus"),
      color = "pink"
    )
  })
  
  
    
    # 4. Edad promedio de hombres y mujeres (usamos la tabla "pacientes" y "metadata")
  output$plot_edad_promedio <- renderEcharts4r({
    req(tablas$pacientes, tablas$metadata)
    
    # Unimos pacientes y metadata por ID
    datos_completos <- merge(tablas$pacientes, tablas$metadata, by = "id", all.x = TRUE)
    
    # Filtramos datos válidos
    datos_completos <- datos_completos %>%
      filter(!is.na(edad), edad >= 10, edad <= 90, !is.na(sexo))
    
    # Definimos los cortes de edad (bins de 10 a 90)
    datos_completos <- datos_completos %>%
      mutate(rango_edad = cut(
        edad,
        breaks = seq(10, 90, by = 10),
        include.lowest = TRUE,
        right = FALSE,
        labels = paste(seq(10, 80, 10), seq(19, 89, 10), sep = "-")
      ))
    
    # Contamos pacientes por rango y sexo
    datos_completos %>%
      count(rango_edad, sexo) %>%
      tidyr::pivot_wider(names_from = sexo, values_from = n, values_fill = 0) %>%
      e_charts(rango_edad) %>%
      e_bar(Masculino, name = "Hombres", stack = "grupo", color = "#66c2ff") %>%
      e_bar(Femenino, name = "Mujeres", stack = "grupo", color = "#ff99cc") %>%
      e_tooltip(trigger = "axis") %>%
      e_legend(right = "10%") %>%
      e_y_axis(name = "Cantidad") %>%
      e_x_axis(name = "Edad (rango)", axisLabel = list(rotate = 45)) %>%
      e_theme("infographic")
  })
  
    
    # 5. Distribución de tipos de piel (usamos la columna "tipo_de_piel" de la tabla "metadata")
    
    output$plot_piel <- renderEcharts4r({
      # Agrupamos y contamos por tipo de piel, excluyendo los NA
      df <- tablas$metadata %>%
        filter(!is.na(tipo_de_piel)) %>%
        count(tipo_de_piel, name = "cantidad")

      # Gráfico de barras horizontal con colores únicos por tipo
      df %>%
        e_charts(x = tipo_de_piel) %>%
        e_bar(cantidad, name = "Cantidad de pacientes") %>%
        e_color(c("#FFB6C1", "#ADD8E6", "#90EE90")) %>%  # Colores pastel
        e_labels(show = TRUE, position = "right") %>%
        e_flip_coords() %>%  # Barras horizontales
        e_tooltip(trigger = "item") %>%
        e_theme("infographic")
    })
    
    
    # #Gráfico: PIE CHART rangos etarios
    output$pie_rangos <- renderEcharts4r({
      req(tablas$pacientes)
      
      colores_coral <- c("#f9dcc4", "#f4a261", "#e76f51")
      
      tablas$pacientes %>%
        filter(!is.na(rango_etario)) %>%  # Aseguramos que no haya NA
        count(rango_etario, name = "Pacientes") %>%
        e_charts(rango_etario) %>%
        e_pie(
          Pacientes,
          radius = c("50%", "70%"),
          label = list(
            show = TRUE,
            position = "inside",
            formatter = "{d}%",
            fontSize = 12
          ),
          labelLine = list(show = FALSE),
          itemStyle = list(borderRadius = 5),
          color = colores_coral
        ) %>%
        e_tooltip(trigger = "item")
    })
    
    #plot fecha cita
    #Observador para actualizar el rango de fechas automáticamente
    observe({
      req(tablas$pacientes)
      updateDateRangeInput(session, "rango_fechas",
                           start = min(tablas$pacientes$fecha_cita, na.rm = TRUE),
                           end = max(tablas$pacientes$fecha_cita, na.rm = TRUE))
    })
    
    # Gráfico: Fecha de Carga
    output$plot_fecha_cita <- renderPlotly({
      req(input$rango_fechas, tablas$pacientes)

      # Filtramos los datos por el rango de fechas seleccionado
      datos_filtrados <- tablas$pacientes %>%
        filter(fecha_cita >= input$rango_fechas[1], fecha_cita <= input$rango_fechas[2]) %>%
        count(fecha_cita)

      # Creamos el gráfico
      plot_ly(data = datos_filtrados, x = ~fecha_cita, y = ~n, type = 'bar', marker = list(color = "#1f77b4")) %>%
        layout(
          title = list(text = "Pacientes cargados por fecha", x = 0.05, font = list(size = 18)),
          xaxis = list(title = "Fecha"),
          yaxis = list(title = "Cantidad de Pacientes")
        )
    })

    

  # #dona de cantidades
  #   output$plot_sexo_dona <- renderEcharts4r({
  #     pacientes <- tablas$pacientes
  #     total <- nrow(pacientes)
  #     
  #     hombres <- sum(pacientes$sexo == "Masculino", na.rm = TRUE)
  #     mujeres <- sum(pacientes$sexo == "Femenino", na.rm = TRUE)
  #     
  #     data <- data.frame(
  #       categoria = c(paste0("Hombres: ", hombres), paste0("Mujeres: ", mujeres)),
  #       valor = c(hombres, mujeres)
  #     )
  #     
  #     data |>
  #       e_charts(categoria) |>
  #       e_pie(
  #         valor,
  #         radius = c("50%", "70%"),
  #         label = list(show = FALSE)
  #       ) |>
  #       e_color(c("#66c2ff", "#ff99cc")) |>
  #       e_title(
  #         text = as.character(total),
  #         subtext = "Total de pacientes",
  #         left = "center",
  #         top = "40%",
  #         textStyle = list(fontSize = 30, fontWeight = "bold"),
  #         subtextStyle = list(fontSize = 16)
  #       ) |>
  #       e_legend(
  #         orient = "vertical",
  #         left = "right",
  #         top = "middle",
  #         textStyle = list(fontSize = 25)
  #       ) |>
  #       e_tooltip(trigger = "item")
  #   })
    
  #PARA UI DE PACIENTE!!!
    # Datos del paciente ID 37 (en el futuro, usar input$usuario_id)
    datos_paciente <- reactive({
      req(tablas$pacientes, tablas$metadata)
      
      paciente <- tablas$pacientes %>% filter(id == 37)
      meta <- tablas$metadata %>% select(id, peso, altura, afecciones_piel, cual_tratamiento_medico, tipo_de_piel, edad) %>% filter(id == 37)
      
      left_join(paciente, meta, by = "id")
    })
    
    output$tabla_info_paciente <- renderDT({
      datatable(datos_paciente(), options = list(scrollX = TRUE))
    })
    
    # Tabla: Vías Metabólicas (filtrada por ID)
    output$tabla_pathways <- renderDT({
      req(tablas$pathways_values)
      datos <- tablas$pathways_values %>% filter(id == 37)
      datatable(datos, options = list(scrollX = TRUE), rownames = FALSE)
    })
    
    # Tabla: Abundancia Bacteriana (filtrada por ID)
    output$tabla_abundancia <- renderDT({
      req(tablas$abundance_species)
      datos <- tablas$abundance_species %>% filter(id == 37)
      datatable(datos, options = list(scrollX = TRUE), rownames = FALSE)
    })
    
    # Descargar reporte PDF
    output$descargar_reporte <- downloadHandler(
      filename = function() {
        "REPORTE_FINAL_MEDICO_ID37.pdf"
      },
      content = function(file) {
        file.copy("www/REPORTE FINAL MEDICO ID37.pdf", file)
      }
    )
    
  
  onStop(function() {
    dbDisconnect(con)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
