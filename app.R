############################################################
# APP SHINY PREMIUM – ARISTAS MEDIA 2022
# INEEd – Uruguay
############################################################

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(data.table)
library(lme4)
library(lmerTest)
library(performance)
library(ggplot2)
library(plotly)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(DT)
library(scales)

#-----------------------------------------------------------
# 1. CARGA DE DATOS (ROBUSTA)
#-----------------------------------------------------------

# Definir datos_raw directamente desde el CSV
datos_raw <- read.csv("datos_liviano.csv")

datos_modelo <- datos_raw %>%
  select(
    Puntaje_Mat,
    INSE_est,
    Codigo_Centro,
    Region,
    ESCS_Centro
  ) %>%
  drop_na()
datos_modelo$Codigo_Centro <- as.factor(datos_modelo$Codigo_Centro)
datos_modelo$INSE_c <- scale(datos_modelo$INSE_est,
                             center = TRUE,
                             scale = FALSE)

#-----------------------------------------------------------
# 2. UI
#-----------------------------------------------------------

ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(title = "Aristas 2022 – Mag. José González"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Panel General", tabName = "panel", icon = icon("chart-line")),
      menuItem("Modelo Multinivel", tabName = "modelo", icon = icon("project-diagram")),
      menuItem("Mapa Territorial", tabName = "mapa", icon = icon("globe"))
    ),
    
    br(),
    
    selectInput("region",
                "Filtrar por Región:",
                choices = c("Todas", sort(unique(datos_modelo$Region))),
                selected = "Todas")
  ),
  
  dashboardBody(
    
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side {
        background-color: #F4F6F9;
      }
      .box {
        border-top: 3px solid #2C7FB8;
      }
    "))),
    
    tabItems(
      
      #---------------------------------------------------
      # PANEL GENERAL
      #---------------------------------------------------
      
      tabItem(tabName = "panel",
              
              fluidRow(
                valueBoxOutput("vb_n", width = 2),
                valueBoxOutput("vb_centros", width = 2),
                valueBoxOutput("vb_media", width = 2),
                valueBoxOutput("vb_icc", width = 2),
                valueBoxOutput("vb_r2m", width = 2),
                valueBoxOutput("vb_r2c", width = 2)
              ),
              
              fluidRow(
                box(width = 6,
                    title = "Distribución Puntaje Matemática",
                    status = "primary",
                    solidHeader = TRUE,
                    plotlyOutput("histograma")),
                
                box(width = 6,
                    title = "Puntaje por Región",
                    status = "primary",
                    solidHeader = TRUE,
                    plotlyOutput("box_region"))
              )
      ),
      
      #---------------------------------------------------
      # MODELO
      #---------------------------------------------------
      
      tabItem(tabName = "modelo",
              
              fluidRow(
                box(width = 12,
                    title = "Resumen del Modelo Multinivel",
                    status = "primary",
                    solidHeader = TRUE,
                    verbatimTextOutput("modelo_resumen"))
              ),
              
              fluidRow(
                box(width = 12,
                    title = "Efectos Aleatorios (Centros)",
                    status = "primary",
                    solidHeader = TRUE,
                    plotlyOutput("efectos_plot"))
              )
      ),
      
      #---------------------------------------------------
      # MAPA
      #---------------------------------------------------
      
      tabItem(tabName = "mapa",
              fluidRow(
                box(width = 12,
                    title = "Mapa Territorial – Puntaje Promedio",
                    status = "primary",
                    solidHeader = TRUE,
                    plotlyOutput("mapa_plot", height = "600px"))
              )
      )
    )
  )
)

#-----------------------------------------------------------
# 3. SERVER
#-----------------------------------------------------------

server <- function(input, output) {
  
  # FILTRO REACTIVO
  datos_filtrados <- reactive({
    
    df <- datos_modelo   # usamos siempre el dataset limpio
    
    if(input$region != "Todas"){
      df <- df %>% filter(Region == input$region)
    }
    
    # Si existe slider INSE
    if(!is.null(input$inse)){
      df <- df %>% 
        filter(INSE_est >= input$inse[1],
               INSE_est <= input$inse[2])
    }
    
    df
  })
  
  # MODELO REACTIVO
  modelo_final <- reactive({
    lmer(
      Puntaje_Mat ~ INSE_c + ESCS_Centro +
        (1 | Codigo_Centro),
      data = datos_filtrados(),
      REML = TRUE
    )
  })
  
  
  # r cuadrado
  r2_vals <- reactive({
    performance::r2(modelo_final())
  })
  
  # ICC dinámico
  icc_val <- reactive({
    var_comp <- as.data.frame(VarCorr(modelo_final()))
    var_entre <- var_comp$vcov[1]
    var_dentro <- var_comp$vcov[2]
    var_entre / (var_entre + var_dentro)
  })
  
  # VALUE BOXES
  output$vb_n <- renderValueBox({
    valueBox(nrow(datos_filtrados()),
             "Estudiantes",
             icon = icon("users"),
             color = "blue")
  })
  
  output$vb_centros <- renderValueBox({
    valueBox(length(unique(datos_filtrados()$Codigo_Centro)),
             "Centros",
             icon = icon("school"),
             color = "navy")
  })
  
  output$vb_media <- renderValueBox({
    valueBox(round(mean(datos_filtrados()$Puntaje_Mat),1),
             "Puntaje Promedio",
             icon = icon("calculator"),
             color = "light-blue")
  })
  
  output$vb_icc <- renderValueBox({
    valueBox(round(icc_val(),3),
             "ICC (Varianza entre Centros)",
             icon = icon("sitemap"),
             color = "purple")
  })
  
  # HISTOGRAMA INTERACTIVO
  output$histograma <- renderPlotly({
    
    media_val <- mean(datos_filtrados()$Puntaje_Mat, na.rm = TRUE)
    
    p <- ggplot(datos_filtrados(),
                aes(x = Puntaje_Mat)) +
      
      geom_histogram(
        aes(y = after_stat(density)),
        bins = 30,
        fill = "#2C7FB8",
        color = "white",
        alpha = 0.85
      ) +
      
      geom_density(
        color = "#08306B",
        linewidth = 1.2
      ) +
      
      geom_vline(
        xintercept = media_val,
        color = "#CB181D",
        linewidth = 1.2,
        linetype = "dashed"
      ) +
      
      annotate(
        "text",
        x = media_val,
        y = Inf,
        label = paste("Media =", round(media_val,1)),
        vjust = 2,
        hjust = -0.1,
        color = "#CB181D",
        size = 4
      ) +
      
      labs(
        x = "Puntaje (Media 300, DE 50)",
        y = "Densidad"
      ) +
      
      theme_minimal(base_size = 14)
    
    ggplotly(p)
  })
  
  # BOXPLOT INTERACTIVO
  output$box_region <- renderPlotly({
    p <- ggplot(datos_filtrados(),
                aes(x = Region,
                    y = Puntaje_Mat,
                    fill = Region)) +
      geom_boxplot() +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # RESUMEN LIMPIO SIN ENVIRONMENT
  output$modelo_resumen <- renderPrint({
    cat("Modelo estimado:\n\n")
    print(formula(modelo_final()))
    cat("\n\n")
    print(summary(modelo_final()), correlation = FALSE)
  })
  
  # EFECTOS ALEATORIOS INTERACTIVOS
  output$efectos_plot <- renderPlotly({
    
    efectos <- ranef(modelo_final())$Codigo_Centro %>%
      as.data.frame()
    
    efectos$Centro <- rownames(ranef(modelo_final())$Codigo_Centro)
    colnames(efectos)[1] <- "Intercepto"
    
    # Ordenar por efecto
    efectos <- efectos %>% arrange(Intercepto)
    
    n <- 25
    
    # 25 menores
    extremos_min <- head(efectos, n)
    
    # 25 mayores
    extremos_max <- tail(efectos, n)
    
    # 25 cercanos a 0
    cercanos_cero <- efectos %>%
      mutate(dist0 = abs(Intercepto)) %>%
      arrange(dist0) %>%
      slice(1:n)
    
    efectos_sel <- bind_rows(
      extremos_min,
      cercanos_cero,
      extremos_max
    ) %>%
      distinct(Centro, .keep_all = TRUE) %>%
      arrange(Intercepto)
    
    # Factor ordenado (IMPORTANTE para eje X)
    efectos_sel$Centro <- factor(
      efectos_sel$Centro,
      levels = efectos_sel$Centro
    )
    
    p <- ggplot(efectos_sel,
                aes(x = Centro,
                    y = Intercepto,
                    color = Intercepto > 0,
                    text = paste(
                      "Centro:", Centro,
                      "<br>Desvío:", round(Intercepto,2)
                    ))) +
      geom_point(size = 3) +
      geom_hline(yintercept = 0,
                 linetype = "dashed",
                 linewidth = 1) +
      scale_color_manual(values = c("#CB181D", "#08519C")) +
      labs(
        x = "Centros seleccionados",
        y = "Desviación del intercepto",
        color = "Sobre promedio"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1,
          size = 8
        ),
        legend.position = "none"
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # MAPA TERRITORIAL
  output$mapa_plot <- renderPlotly({
    
    uruguay <- ne_countries(scale = "medium",
                            country = "Uruguay",
                            returnclass = "sf")
    
    datos_region <- datos_filtrados() %>%
      group_by(Region) %>%
      summarise(Puntaje = mean(Puntaje_Mat))
    
    uruguay$Puntaje <- mean(datos_region$Puntaje)
    
    p <- ggplot(uruguay) +
      geom_sf(aes(fill = Puntaje)) +
      scale_fill_viridis_c() +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$vb_r2m <- renderValueBox({
    valueBox(
      round(r2_vals()$R2_marginal, 3),
      "R² Marginal",
      icon = icon("chart-line"),
      color = "teal"
    )
  })
  
  output$vb_r2c <- renderValueBox({
    valueBox(
      round(r2_vals()$R2_conditional, 3),
      "R² Condicional",
      icon = icon("layer-group"),
      color = "green"
    )
  })
}

shinyApp(ui, server)
