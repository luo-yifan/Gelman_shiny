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
    "Gelman Site Groundwater Contamination Plume Modeling and Prediction"
  ),
  helpText("Disclaimer: The information contained in this application is for general information purposes only ",
  "and should not be used for navigation, regulatory, permitting, or other legal purposes.",
  "We have used our reasonable efforts to ensure that the data analysis we release is complete, accurate, and useful.", 
  "However, because we do not create the data and because the processing required to make the data useful is complex,",
  "we cannot be liable for omissions or inaccuracies. Both the time series and spatial movement prediction are provided ‘as is’." ,
  "And some uncertainty and limitations are associated with this data analysis, which will be described in detail in the report."),
  fluidRow(column(
    12,
    navbarPage(
      "Menu",
      id = "plot_tabs",
      
      tabPanel("Well data analysis",
               
               fluidRow(
                 column(
                   6,
                   
                   conditionalPanel(condition = "input.plot_tabs!='User guide' && input.plot_tabs!='Plume Projection'",
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
                                                 div(DT::dataTableOutput("table_input"),style = "font-size:70%")
                                               )),
      
                                    )),
                   conditionalPanel(condition = "input.plot_tabs=='Plume Projection'", column(12)),
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
      tabPanel("Plume projection",
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
               ))),
      tabPanel("Table output",
               column(
                 12,
                 div(DT::dataTableOutput("table_with_prediction"),style = "font-size:70%")
               )),
    )
  ))
)
