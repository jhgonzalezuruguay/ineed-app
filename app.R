############################################################
# APP SHINY – ARISTAS MEDIA 2022 (VERSIÓN LIGERA CORRECTA)
############################################################

library(shiny)
library(shinydashboard)
library(dplyr)
library(lme4)
library(lmerTest)
library(performance)
library(ggplot2)
library(plotly)
library(scales)

#-----------------------------------------------------------
# 1. DATOS
#-----------------------------------------------------------

datos_raw <- read.csv("datos_liviano.csv")

datos_modelo <- datos_raw %>%
  select(
    Puntaje_Mat,
    INSE_est,
    Codigo_Centro,
    Region,
    ESCS_Centro
  ) %>%
  na.omit()

datos_modelo$Codigo_Centro <- as.factor(datos_modelo$Codigo_Centro)

datos_modelo$INSE_c <- scale(
  datos_modelo$INSE_est,
  center = TRUE,
  scale = FALSE
)

regiones_totales <- sort(unique(datos_modelo$Region))

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
      menuItem("Análisis Regional", tabName = "regional", icon = icon("globe"))
    ),
    
    br(),
    
    selectInput("region",
                "Filtrar por Región:",
                choices = c("Todas", regiones_totales),
                selected = "Todas")
  ),
  
  dashboardBody(
    tabItems(
      
      # PANEL GENERAL
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
      
      # MODELO
      tabItem(tabName = "modelo",
              
              fluidRow(
                box(width = 12,
                    title = "Resumen del Modelo Multinivel",
                    status = "primary",
                    solidHeader = TRUE,
                    verbatimTextOutput("modelo_resumen"))
              ),
              actionButton("ver_estudio",
                           "Ver Investigación Académica",
                           icon = icon("external-link-alt"),
                           style = "background-color:red; color:white;"),
              br(),br(),
              
              fluidRow(
                box(width = 12,
                    title = "Efectos Aleatorios (Centros)",
                    status = "primary",
                    solidHeader = TRUE,
                    plotlyOutput("efectos_plot"))
              )
      ),
      
      # ANALISIS REGIONAL
      tabItem(tabName = "regional",
              fluidRow(
                box(width = 12,
                    title = "Puntaje Promedio por Macro-Región",
                    status = "primary",
                    solidHeader = TRUE,
                    plotlyOutput("regional_plot", height = "600px"))
              )
      )
    )
  )
)

#-----------------------------------------------------------
# 3. SERVER
#-----------------------------------------------------------

server <- function(input, output) {
  
  datos_filtrados <- reactive({
    if(input$region == "Todas"){
      datos_modelo
    } else {
      datos_modelo %>% filter(Region == input$region)
    }
  })
  
  modelo_final <- reactive({
    lmer(
      Puntaje_Mat ~ INSE_c + ESCS_Centro +
        (1 | Codigo_Centro),
      data = datos_filtrados(),
      REML = TRUE
    )
  })
  
  r2_vals <- reactive({
    performance::r2(modelo_final())
  })
  
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
  
  output$vb_r2m <- renderValueBox({
    valueBox(round(r2_vals()$R2_marginal,3),
             "R² Marginal",
             icon = icon("chart-line"),
             color = "teal")
  })
  
  output$vb_r2c <- renderValueBox({
    valueBox(round(r2_vals()$R2_conditional,3),
             "R² Condicional",
             icon = icon("layer-group"),
             color = "green")
  })
  
  # HISTOGRAMA ORIGINAL CON COLORES
  
  output$histograma <- renderPlotly({
    
    media_val <- mean(datos_filtrados()$Puntaje_Mat)
    
    p <- ggplot(datos_filtrados(),
                aes(x = Puntaje_Mat)) +
      
      geom_histogram(
        aes(
          text = paste(
            "Intervalo:",
            round(after_stat(x),1),
            "<br>Frecuencia:",
            round(after_stat(count),1)
          )
        ),
        bins = 30,
        fill = "#2C7FB8",
        color = "white"
      ) +
      
      geom_vline(
        aes(
          xintercept = media_val,
          text = paste("Media:", round(media_val,1))
        ),
        color = "#CB181D",
        linetype = "dashed",
        linewidth = 1
      ) +
      
      labs(
        x = "Puntaje",
        y = "Frecuencia"
      ) +
      
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(title = "Puntaje"),
        yaxis = list(title = "Frecuencia")
      )
  })
  #output$histograma <- renderPlotly({
    
  #   media_val <- mean(datos_filtrados()$Puntaje_Mat)
    
  #  p <- ggplot(datos_filtrados(),
  #             aes(x = Puntaje_Mat)) +
  #   geom_histogram(
  #     bins = 30,
  #     fill = "#2C7FB8",
  #     color = "white"
  #   ) +
  #   geom_vline(
  #     xintercept = media_val,
  #     linetype = "dashed",
  #     linewidth = 1
  #   ) +
  #   labs(
  #     x = "Puntaje",
  #     y = "Frecuencia"
  #   ) +
  #   theme_minimal()
    
  # ggplotly(p) %>%
  #   layout(
  #     xaxis = list(title = "Puntaje"),
  #     yaxis = list(title = "Frecuencia"),
  #     hoverlabel = list(font = list(family = "Arial"))
  #   )
  #})
  
  
  #box
  output$box_region <- renderPlotly({
    
    p <- ggplot(datos_filtrados(),
                aes(x = Region,
                    y = Puntaje_Mat,
                    fill = Region)) +
      geom_boxplot() +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p) %>%
      style(
        hovertemplate = paste(
          "<b>Región:</b> %{x}<br>",
          "<b>Puntaje:</b> %{y:.1f}",
          "<extra></extra>"
        )
      )
  })
  


  # RESUMEN LIMPIO SIN ENVIRONMENT
  output$modelo_resumen <- renderPrint({
    cat("Modelo estimado:\n\n")
    print(formula(modelo_final()))
    cat("\n\n")
    print(summary(modelo_final()), correlation = FALSE)
  }) 
   
  # EFECTOS ALEATORIOS CORRECTOS 25-25-25
  
  output$efectos_plot <- renderPlotly({
    
    efectos <- ranef(modelo_final())$Codigo_Centro %>%
      as.data.frame()
    
    efectos$Centro <- rownames(efectos)
    colnames(efectos)[1] <- "Intercepto"
    
    efectos <- efectos %>% arrange(Intercepto)
    
    n <- 25
    
    extremos_min <- head(efectos, n)
    extremos_max <- tail(efectos, n)
    
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
    
    efectos_sel$Centro <- factor(
      efectos_sel$Centro,
      levels = efectos_sel$Centro
    )
    
    p <- ggplot(efectos_sel,
                aes(x = Centro,
                    y = Intercepto,
                    color = Intercepto > 0,
                    text = paste("Centro:", Centro,
                                 "<br>Desvío:", round(Intercepto,2)))) +
      geom_point(size = 3) +
      geom_hline(yintercept = 0,
                 linetype = "dashed",
                 linewidth = 1) +
      scale_color_manual(values = c("#CB181D", "#08519C")) +
      labs(
        x = "Centros seleccionados",
        y = "Desviación del intercepto"
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
  
  # REGIONAL SIEMPRE COMPLETO
  
  output$regional_plot <- renderPlotly({
    
    datos_region <- datos_filtrados() %>%
      group_by(Region) %>%
      summarise(Puntaje = mean(Puntaje_Mat)) %>%
      right_join(
        data.frame(Region = regiones_totales),
        by = "Region"
      ) %>%
      arrange(Region)
    
    plot_ly(
      datos_region,
      x = ~Region,
      y = ~Puntaje,
      type = "bar",
      color = ~Region,   # <-- color distinto por región
      colors = c(
        "#08306B",  # azul oscuro
        "pink",  # azul medio
        "#2C7FB8",  # azul institucional
        "red",  # celeste
        "#7FCDBB"   # verde azulado
      ),
      hovertemplate = paste(
        "<b>Región:</b> %{x}<br>",
        "<b>Puntaje Promedio:</b> %{y:.1f}<extra></extra>"
      )
    ) %>%
      layout(
        xaxis = list(title = "Macro-Región"),
        yaxis = list(title = "Puntaje Promedio"),
        showlegend = FALSE
      )
  })
    observeEvent(input$ver_estudio, {
    browseURL("https://www.researchgate.net/publication/388192589_Investigacion_Educativa")
  })
}

shinyApp(ui, server)
