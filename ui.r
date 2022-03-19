
library(plotly)
library(leaflet)
library(raster)

ui <- fluidPage(
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                type = "text/javascript")
  ),
  
  # Header
  titlePanel("EAS 501.19 Advanced Geovisualization Final Project"),
  helpText("Gelman 1,4-Dioxane Plume Groundwater Contaminant Profile"),
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
                     
                     conditionalPanel(condition = "input.plot_tabs!='User guide' && input.plot_tabs!='GeoTif'",
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
                     conditionalPanel(condition = "input.plot_tabs=='User guide'",
                                      column(12)),
                     conditionalPanel(condition = "input.plot_tabs=='GeoTif'", column(12))
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
        tabPanel("User guide",
                 fluidRow(column(
                   8,
                   includeMarkdown('./user_guide/user_guide.rmd')
                 ))),
        tabPanel("GeoTif",
                 fluidRow(
                   column(
                     8,
                     leafletOutput("mymap"),
                     p(),
                     sliderInput(
                       "slider",
                       "Dates:",
                       min = as.Date("1986-02-01", "%Y-%m-%d"),
                       max = as.Date("2022-11-01", "%Y-%m-%d"),
                       value = as.Date("2016-12-01"),
                       timeFormat = "%Y-%m"
                     )
                   )
                 ))
        )
      )
    )
  )
      
  


