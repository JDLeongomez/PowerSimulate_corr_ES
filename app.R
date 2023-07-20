#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

Sys.setlocale("LC_ALL", "es_CO.UTF-8")
library(shiny)
library(thematic)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse) 
library(faux)
library(ggExtra)
library(ggpubr)
library(plyr)
library(scales)

input <<- tibble(
  alts = "Alguna correlación",
  meanx = 172.2,
  meany = 68.2,
  sdx = 6.4,
  sdy = 10.5,
  labelx = "Heigth (cm)",
  labely = "Weight (kg)",
  corrxy = 0.39
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("slate"),
  
  # Application title
  titlePanel(title = tags$link(rel = "icon",
                               type = "image",
                               href = "https://image.pngaaa.com/393/402393-middle.png"),
             "PowerSimulate (correlación)"),
  HTML("<center><img src='powersimulate.svg'' width='600'></center>"),
  tags$h3(HTML("<center>Correlación</center>")),
  p(HTML("<center>Código disponible en
      <a style=color:#ff5555;  href='https://github.com/JDLeongomez/PowerSimulate_corr_ES'>GitHub</a>
      - Creado por
      <a style=color:#ff5555;  href='https://jdleongomez.info/es/'>Juan David Leongómez</a>
      · 2023 · <a style=color:#4075de;  href='https://shiny.jdl-svr.lat/PowerSimulate_corr_EN/'>
      English version</a> 
      · <a style=color:#ff5555;  href='https://shiny.jdl-svr.lat/PowerSimulate_ind_t_EN'>PowerSimulate: Prueba <em>t</em> independiente.</a></center>")),
  hr(),
  p(HTML("<center>Análisis de poder estadístico basado en la simulación de una población y la probabilidad de 
         obtener un resultado significativo con una muestra aleatoria de un tamaño determinado.<br>Aunque existen herramientas 
         más directas para el análisis de poder en el caso de las pruebas de correlación, esta aplicación se basa en simulaciones 
         para ilustrar el concepto de poder estadístico.</center>")),
  fluidRow(
    column(2,
           tags$h2("Parámetros de las variables"),
           tags$h4("Variable X"),
           textInput(inputId = "labelx",
                     label = "Etiqueta para la variable X",
                     value = "Estatura (cm)",
                     width = '300px'),
           numericInput(inputId = "meanx",
                        label = "Media",
                        min = -Inf,
                        max = Inf,
                        value = 172.2,
                        step = 0.0001,
                        width = '300px'),
           numericInput(inputId = "sdx",
                        label = "Desviación estándar",
                        min = -Inf,
                        max = Inf,
                        value = 6.4,
                        step = 0.0001,
                        width = '300px'),
           hr(),
           tags$h4("Variable Y"),
           textInput(inputId = "labely",
                     label = "Etiqueta para la variable Y",
                     value = "Peso (kg)",
                     width = '300px'),
           numericInput(inputId = "meany",
                        label = "Media",
                        min = -Inf,
                        max = Inf,
                        value = 68.2,
                        step = 0.0001,
                        width = '300px'),
           numericInput(inputId = "sdy",
                        label = "Desviación estándar",
                        min = -Inf,
                        max = Inf,
                        value = 10.5,
                        step = 0.0001,
                        width = '300px')
    ),
    column(4,
           tags$h1("Tamaño del efecto en la población"),
           tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background:#ff5555}")),
           sliderInput(inputId = "corrxy",
                       label = "Coeficiente de correlación (Pearson)",
                       min = -1,
                       max = 1,
                       value = 0.39,
                       step = 0.001,
                       width = 'auto'),
           tags$h3("Si esta fuera la correlación en la población"),
           plotOutput("effectPlot") %>% 
             withSpinner(color = "#ff5555")
    ),
    column(2,
           tags$h2("Parámetros de simulación"),
           tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background:#ff5555}")),
           sliderInput(inputId = "sample_size",
                       label = "Tamaño de muestra",
                       min = 5,
                       max = 1000,
                       value = 50,
                       step = 1,
                       width = '300px'),
           tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background:#ff5555}")),
           sliderInput(inputId = "alpha",
                       label = HTML("Nivel de significación (tipicamente &alpha; = 0.05)"),
                       min = 0,
                       max = 1,
                       value = 0.05,
                       step = 0.001,
                       width = '300px'),
           selectInput(inputId = "alts",
                       label = "Hypothesis",
                       choices = c("Alguna correlación", 
                                   "Correlación positiva",
                                   "Correlación negativa"
                       )),
           numericInput(inputId = "reps",
                        label = HTML("Número de simulaciones<br>
                                     <span style='font-weight:normal'>Números más grandes aumentan la precisión 
                                     pero requieren más tiempo. Por defecto ejecuta sólo 100 simulaciones, 
                                     pero una vez que hayas comprobado todos los parámetros, te sugiero que ejecutes 
                                     1000+ simulaciones para aumentar la precisión.</span>"),
                        min = 1,
                        max = 10000000,
                        value = 100,
                        step = 1,
                        width = '300px')
    ),
    column(4,
           tags$h1("Poder estadístico"),
           tags$h3("Este es el poder estadístico que alcanzarías"),
           plotOutput("powerPlot") %>% 
             withSpinner(color = "#ff5555"),
           htmlOutput("powText")
    )
  )
)

server <- function(input, output, session) {
  
  # Simulate population
  dat <- reactive({
    datos <- rnorm_multi(n = 10000, 
                         mu = c(input$meanx, input$meany),
                         sd = c(input$sdx, input$sdy),
                         r = input$corrxy, 
                         varnames = c("Xvar", "Yvar"),
                         empirical = TRUE)
    return(datos)
  })
  
  # Population distribution plot 
  output$effectPlot <- renderPlot({
    p <- ggplot(dat(), aes(x = Xvar, y = Yvar)) +
      geom_point(alpha = 0.2, color = "#ff555560") +
      geom_smooth(method = "lm") +
      annotate("text", x = -Inf, y = Inf, 
               hjust = -0.2, vjust = 2, size = 6,
               label = paste0("r = ", input$corrxy)) +
      labs(x = input$labelx, y = input$labely)
    ggMarginal(p, type = "density", fill = "#ff5555")
  })
  
  # Create object with selected hypothesis alternative
  altern <<- reactive({
    dplyr::case_when(
      input$alts == "Alguna correlación" ~ "two.sided",
      input$alts == "Correlación positiva" ~ "greater",
      TRUE ~ "less")
  })
  
  sig.lev <<- reactive({
    input$alpha
  })
  
  # Simulate samples and test significance in each
  dat.sim <- reactive({
    req(input$alts)
    dato <- ddply(map_dfr(seq_len(input$reps), ~dat() %>%
                            sample_n(input$sample_size) %>%
                            mutate(sample = as.factor(.x))),
                  .(sample), summarise,
                  p = round(cor.test(x = Xvar, y = Yvar,
                                     alternative = altern())$p.value, 3),
                  "Significance" = ifelse(p <= sig.lev(), "Significant", "Non-significant"))
    return(dato)
  })
  
  # Power simulation plot 
  output$powerPlot <- renderPlot({
    ggplot(dat.sim(), aes(x = p, fill = Significance)) +
      scale_fill_hue(direction = -1) +
      geom_histogram(bins = 1/input$alpha, breaks = seq(0, 1, input$alpha), 
                     alpha = 0.8) +
      scale_fill_manual(values = c("#4075de", "#ff5555")) +
      labs(y = "Count", x = "p-value") +
      scale_x_continuous(breaks = pretty_breaks(n = 20)) +
      annotate("text", x = 0.5, y = Inf, size = 7, vjust = 2,
               label = paste0("Power (1 - β) = ", round(sum(dat.sim()$Significance == "Significant") / input$reps, 3))) +
      annotate("text", x = 0.5, y = Inf, vjust = 5,
               label = paste0("Sample size = ", input$sample_size)) +
      annotate("text", x = 0.5, y = Inf, vjust = 6.5,
               label = paste0("α = ", input$alpha)) +
      theme(legend.position="bottom", 
            legend.title=element_text(size=14),
            legend.text = element_text(size = 12)) +
      guides(fill = guide_legend(reverse=TRUE))
  })
  
  output$powText <- renderText({
    paste("<b style=color:#ff5555;>INTERPRETATION: </b>
          El poder no es más que la proporción de resultados significativos  
          (<em>p</em> < α). Así, si la diferencia real en la población fuera <font color=\'#ff5555\'><b><em>r</em> = ",
          input$corrxy, "</b></font>, con una muestra aleatoria de <font color=\'#ff5555\'><b>", input$sample_size, 
          "</b></font>, obtendrías un resultado significativo en aproximadamente el <font color=\'#ff5555\'><b>", 
          percent(round(sum(dat.sim()$Significance == "Significant") / input$reps, 2)),
          "</b></font> de los casos.")
  })
}

# Same theme for plots
thematic_shiny()

# Run the application 
shinyApp(ui = ui, server = server)
