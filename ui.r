library(plotly)
library(leaflet)
library(raster)

x = seq.Date(
  from = as.Date("1986-02-01"),
  by = "month",
  length.out = 418
)

y = c(as.Date("2021-01-01"), 
      as.Date("2022-01-01"), 
      as.Date("2023-01-01"), 
      as.Date("2024-01-01"), 
      as.Date("2025-01-01"))

z = c(x, y)

choices_month = format(z, "%Y-%m")

ui <- fluidPage(
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                type = "text/javascript")
  ),
  
  # Header
  titlePanel(
    "Gelman Site Groundwater Contamination Plume Modelling and Prediction"
  ),
  helpText("Yifan Luo's SEAS Capstone Project"),
  fluidRow(column(
    12,
    navbarPage(
      "Menu",
      id = "plot_tabs",
      
      tabPanel("Time series",
               
               fluidRow(
                 column(
                   6,
                   
                   conditionalPanel(condition = "input.plot_tabs!='User guide' && input.plot_tabs!='Spatial movement'",
                                    tabsetPanel(
                                      id = "ui_tab",
                                      tabPanel("Map",
                                               column(
                                                 12,
                                                 h4("Click a site"),
                                                 shinycssloaders::withSpinner(
                                                   leaflet::leafletOutput("map", height = "600px"),
                                                   size = 2,
                                                   color = "#0080b7"
                                                 )
                                               )),
                                      tabPanel("Table",
                                               column(
                                                 12,
                                                 h4("Click a site"),
                                                 div(DT::dataTableOutput("table_input"), style = "font-size:70%")
                                               ))
                                    )),
                   conditionalPanel(condition = "input.plot_tabs=='Spatial movement'", column(12)),
                   conditionalPanel(condition = "input.plot_tabs=='User guide'",
                                    column(12))
                 ),
                 column(6,
                        
                        
                        fluidRow(column(
                          12,
                          conditionalPanel(condition = TRUE,
                                           plotlyOutput("ggPlot"))
                          
                          
                        )))
               )),
      tabPanel("Spatial movement",
               fluidRow(
                 column(
                   10,
                   leafletOutput("mymap"),
                   p(),
                   shinyWidgets::sliderTextInput(
                     inputId = "month_slider",
                     label = "Dates:",
                     choices = choices_month,
                     selected = choices_month[168],
                     animate = TRUE,
                   ),
                 )
               )),
      tabPanel("User guide",
               fluidRow(column(
                 10,
                 includeMarkdown('./user_guide/user_guide.rmd')
               )))
    )
  ))
)
