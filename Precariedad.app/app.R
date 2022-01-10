library(tidyverse)
library(ggthemes)
library(colorspace)
library(plotly)
library(shiny)
library(DT)

####Paleta####
azul <- diverge_hcl(n = 12,h = c(255,330),
                    l = c(40,90))[c(4,2,1)]
verde <- diverge_hcl(n = 12,h = c(130,43),
                     c = 100,
                     l = c(70,90))[c(4,2,1)]

naranja <- diverge_hcl(n = 12,h = c(130,43),
                       c = 100,
                       l = c(70,90))[c(10,11,12)]
paleta <- c(azul,
            naranja,
            verde)

paleta3 <- c(azul[1],
             naranja[1],
             verde[1])


####Data####
load("datashiny.RDATA")
metadata <- read.csv("Metadata.csv",
                     sep = ";",
                     encoding = "UTF-8") %>% 
    mutate(
    across(
    .cols = 1:ncol(.),
    .fns = ~factor(.x))) %>%
  ungroup()

tabla <- perfiles %>%
  bind_rows(agregado)

tabla <- tabla %>% 
  mutate(
     across(
     .cols = 7:ncol(.),
     .fns = ~round(.x, digits = 2))) %>%
ungroup()

resultados <- perfiles.tidy
###### SHINY ######
ui <-  fluidPage(
  titlePanel(title = "Precariedad Mundial"),
  tabsetPanel(
    id = 'Display',
    tabPanel("Tabla", DTOutput("table")),
    tabPanel("Metadata", DTOutput("table2")),
    tabPanel("Grafico",fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "Pais",
                   label =  "Paises: ",
                   choices = unique(resultados$Pais),
                   multiple = T,
                   selected = unique(resultados$Pais)),
                 selectInput(
                   inputId = "tipograf",
                   label =  "Tipo de Grafico: ",
                   choices = c("Barra apilada","Barra horizontal","Linea"),
                   multiple = F,
                   selected = "Barra horizontal"),
                 selectizeInput(
                   inputId = "Serie",
                   label =  "Serie:",
                   choices = unique(resultados$Serie),
                   selected = "tasa.seguridad.social.asal",
                   multiple = FALSE,
                   width = 400),
                 selectInput(
                   inputId = "Ejex",
                   label =  "Eje x: ",
                   choices = c("Paises","Perfiles"),
                   multiple = F,
                   selected = "Paises"),
                 radioButtons(
                   inputId = "texto.eje.x",
                   label =  "Texto en eje x: ",
                   choices = c("Si","No"),
                   selected = "Si"),
                 selectInput(
                   inputId = "Relleno",
                   label =  "Color de Relleno: ",
                   choices = c("Paises","Perfiles"),
                   multiple = F,
                   selected = "Perfiles"),
                 selectInput(
                   inputId = "Faceta",
                   label =  "Variable Faceta: ",
                   choices = c("No","Paises","Perfiles"),
                   multiple = F,
                   selected = "No"),
                 selectInput(
                   inputId = "tipo.faceta",
                   label =  "Tipo Faceta: ",
                   choices = c("Normal","Eje y libre","Eje x libre",
                               "Ambos libres"),
                   multiple = F,
                   selected = "Normal")
               ),
               mainPanel(plotlyOutput(outputId = "Grafico"))
             )
    )
  )
)

server <- function(input, output) {
  
  tabla <- tabla
  df <- resultados
  
  output$table   <- renderDT({
    
    datatable(tabla,
              filter="top",
              selection="multiple",
              escape=FALSE,
              rownames = FALSE)
    
  })
  
  output$table2   <- renderDT({
    
    datatable(metadata,
              filter="top",
              selection="multiple",
              escape=FALSE,
              rownames = FALSE)
    
  })
  
  output$Grafico <- renderPlotly({
    
    paises.elegidos <- input$Pais
    series.elegidas <- input$Serie
    tipo.grafico <- input$tipograf
    eje.x <- input$Ejex
    texto.eje.x <- input$texto.eje.x
    variable.faceta <- input$Faceta
    tipo.faceta <- input$tipo.faceta
    relleno <- input$Relleno
    
    
    data_graf <-  df %>%
      filter(Pais %in% paises.elegidos, Serie %in% series.elegidas)
    
    if(tipo.grafico == "Barra horizontal"){
      tipo  <- geom_col(position = position_dodge())
    }
    if(tipo.grafico == "Barra apilada"){
      tipo  <- geom_col()
    }
    if(tipo.grafico == "Linea"){
      tipo  <- geom_line()
    }
    
    ejex <- case_when(eje.x == "Paises" ~  "Pais",
                      eje.x == "Perfiles" ~  "tamanio.calif")
    
    relleno <- case_when(relleno == "Paises" ~  "Pais",
                         relleno == "Perfiles" ~  "tamanio.calif")
    
    options(scipen = 999)
    a <- ggplot(data_graf,aes_string(x= ejex,
                                     y = "Valor",
                                     fill = relleno,
                                     group = relleno))+
      tipo+
      theme_tufte()+
      theme(axis.text.x = element_text(angle = 45),
            panel.grid.minor = element_line(),
            panel.grid.major.y = element_line(colour = "grey90"),
            legend.position = "bottom")+
      scale_fill_manual(values = paleta)
    
    escala.faceta <- case_when(tipo.faceta == "Eje y libre" ~  "free_y",
                               tipo.faceta == "Eje x libre" ~  "free_x",
                               tipo.faceta == "Ambos libres" ~  "free_both",
                               TRUE ~ "fixed")
    
    if(variable.faceta == "Paises"){
      a  <- a + facet_wrap(~{{Pais}},scales = escala.faceta)
    }
    
    if(texto.eje.x == "No"){
      a  <- a + theme(axis.text.x = element_blank())
    }
    
    
    
    ggplotly(a)
    
  })
}


shinyApp(ui = ui, server = server)

