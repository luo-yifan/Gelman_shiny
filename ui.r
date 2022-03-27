
library(plotly)
library(leaflet)
library(raster)

choices_month <- format(seq.Date(from = as.Date("1986-02-01"), by = "month", length.out = 468), "%Y-%m")

ui <- fluidPage(
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                type = "text/javascript")
  ),
  
  # Header
  titlePanel("Gelman Site Groundwater Contamination Plume Modelling and Prediction"),
  helpText("Yifan Luo's SEAS Capstone Project"),
  fluidRow(
    column(
      12,
      navbarPage(
        "Menu",
        id = "plot_tabs",
        
        tabPanel("Time series",
                 
                 fluidRow(
                   column(
                     5,
                     
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
                   column(
                     7,
                     
                     
                     fluidRow(
                       column(
                         8,
                         conditionalPanel(condition = TRUE,
                                          plotlyOutput("ggPlot"))
                         
                         
                       )
                       
                     )
                   )
                 )
                 
        ),
        tabPanel("Spatial movement",
                 fluidRow(
                   column(
                     8,
                     leafletOutput("mymap"),
                     p(),
                     # custom slider function
                     shinyWidgets::sliderTextInput(
                       inputId = "month_slider",
                       label = "Dates:",
                       choices = choices_month,
                       selected = choices_month[120],
                       animate = TRUE,
                     ),
                   )
                 )),
        tabPanel("User guide",
                 fluidRow(column(
                   8,
                   includeMarkdown('./user_guide/user_guide.rmd')
                 )))
        )
      )
    )
  )
      
  


