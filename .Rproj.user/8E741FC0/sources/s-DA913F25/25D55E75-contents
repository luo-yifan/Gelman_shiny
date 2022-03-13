library(wqTools)
library(magrittr)
library(readxl)
library(ggplot2)
library(plotly)
library(gganimate)
library(leaflet)
library(raster)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
projectPath = getwd()

# heatmap_param_choices = c(
#   "Minimum Dissolved Oxygen",
#   "Temperature, water",
#   "pH",
#   "DO-temperature habitat profile width"
# )
# names(heatmap_param_choices) = c("Dissolved oxygen", "Temperature", "pH", "DO/temperature lens")

ui <- fluidPage(
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                type = "text/javascript")
  ),
  
  # Header
  titlePanel("EAS 501.19 Advanced Geovisualization Final Project"),
  helpText("Gelman 1,4-Dioxane Plume Groundwater Contaminant Profile"),
  
  # headerPanel(
  #   title=tags$a(href='https://deq.utah.gov/division-water-quality/',tags$img(src='deq_dwq_logo_draft.png', height = 125, width = 100*2.85*1.75), target="_blank"),
  #   tags$head(tags$link(rel = "icon", type = "image/png", href = "dwq_logo_small.png"), windowTitle="Lake profile dashboard")
  # ),
  
  #,
  
  # Input widgets
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
      tabsetPanel(
        id = "plot_tabs",
        tabPanel("Time series",
                 fluidRow(
                   column(
                     8,
                     uiOutput("date_slider"),
                     conditionalPanel(condition = TRUE,
                                      plotlyOutput("ggPlot"))
                     
                     
                   )
                 )),
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
                 )),
      )
    )
  )
)

server <- function(input, output, session) {
  # Loading modal to keep user out of trouble while map draws...
  showModal(
    modalDialog(
      title = "MAP LOADING - PLEASE WAIT...",
      "Please wait for map to draw before proceeding.",
      size = "l",
      footer = NULL
    )
  )
  
  # Remove modal when app is ready
  observe({
    req(map, wells_mlid_param_asmnts)
    removeModal()
  })
  
  gelman_data = read_excel("./data/well_data.xlsx")
  wells = unique(gelman_data[, c("Bore", "lat", "lon")])
  wells = plyr::rename(
    wells,
    c(
      "lat" = "LatitudeMeasure",
      "lon" = "LongitudeMeasure",
      "Bore" = "MonitoringLocationIdentifier"
    )
  )
  wells$MonitoringLocationName = wells$MonitoringLocationIdentifier
  wells$MonitoringLocationTypeName = "Sampling Wells"
  
  
  wells_long = unique(gelman_data[, c("Bore", "SampleDate")])
  wells_long = plyr::rename(wells_long, c("Bore" = "MonitoringLocationIdentifier"))
  wells_long = plyr::rename(wells_long, c("SampleDate" = "ActivityStartDate"))
  wells_long$ParameterName = "Sampling Well"
  wells_long$Bore = wells_long$MonitoringLocationIdentifier
  wells_long$ActivityStartDate = as.Date(wells_long$ActivityStartDate, format =
                                           '%Y-%m-%d')
  wells_long$ActivityIdentifier =  paste(wells_long$MonitoringLocationIdentifier,
                                         wells_long$ActivityStartDate,
                                         sep = "_")
  wells_long = data.frame(wells_long)
  
  wells = wells[wells$MonitoringLocationIdentifier %in% wells_long$MonitoringLocationIdentifier,]
  wells = data.frame(wells)
  
  wells_ind_prof_asmnts = unique(gelman_data[, c("Bore", "SampleDate", "Value", "lat", "lon")])
  wells_ind_prof_asmnts = plyr::rename(
    wells_ind_prof_asmnts,
    c(
      "lat" = "LatitudeMeasure",
      "lon" = "LongitudeMeasure",
      "Bore" = "MonitoringLocationName",
      "SampleDate" = "ActivityStartDate"
    )
  )
  wells_ind_prof_asmnts$MonitoringLocationIdentifier = wells_ind_prof_asmnts$MonitoringLocationName
  wells_ind_prof_asmnts$ActivityStartDate = as.Date(wells_ind_prof_asmnts$ActivityStartDate, format =
                                                      '%Y-%m-%d')
  
  wells_ind_prof_asmnts$ActivityIdentifier =
    paste(
      wells_ind_prof_asmnts$MonitoringLocationIdentifier,
      wells_ind_prof_asmnts$ActivityStartDate,
      sep = "_"
    )
  
  wells_ind_prof_asmnts = wells_ind_prof_asmnts[wells_ind_prof_asmnts$ActivityIdentifier
                                                %in% wells_long$ActivityIdentifier,]
  wells_ind_prof_asmnts$Well_Name = wells_ind_prof_asmnts$MonitoringLocationIdentifier
  wells_ind_prof_asmnts = plyr::rename(wells_ind_prof_asmnts, c("Value" =
                                                                  "do_pct_exc"))
  wells_ind_prof_asmnts = data.frame(wells_ind_prof_asmnts)
  
  wells_mlid_param_asmnts = unique(gelman_data[, c("Bore", "Depth2", "Elevation", "lat", "lon")])
  wells_mlid_param_asmnts = plyr::rename(
    wells_mlid_param_asmnts,
    c(
      "Bore" = "Well_Name",
      "Depth2" = "Depth_feet",
      "Elevation" = "Elevation_feet",
      "lat" = "Latitude",
      "lon" = "Longtitude"
    )
  )
  #wells_mlid_param_asmnts$ParameterName = "Sampling Well"
  wells_mlid_param_asmnts = data.frame(wells_mlid_param_asmnts)
  #wells_mlid_param_asmnts = cbind(wells_mlid_param_asmnts$Well_Name, wells_mlid_param_asmnts)
  
  rec_txt_raw <- read.csv(file = "./data/all_data_rec.csv")
  rec_txt = unique(rec_txt_raw[, c("WellName", "Date", "Concentration")])
  rec_txt$Date = as.Date(rec_txt$Date, format = '%Y-%m-%d')
  rec_txt$Type = 'rec_ori'
  rec_txt = data.frame(rec_txt)
  
  predict_simple_raw <-
    read.csv(file = "./data/all_predict_data.csv")
  predict_simple = unique(predict_simple_raw[, c("WellName", "Date", "Concentration")])
  predict_simple$Date = as.Date(predict_simple$Date, format = '%Y-%m-%d')
  predict_simple$Type = 'pred'
  predict_simple = data.frame(predict_simple)
  
  predict_simple_rec_raw <-
    read.csv(file = "./data/all_predict_data_rec.csv")
  predict_simple_rec = unique(predict_simple_rec_raw[, c("WellName", "Date", "Concentration")])
  predict_simple_rec$Date = as.Date(predict_simple_rec$Date, format = '%Y-%m-%d')
  predict_simple_rec$Type = 'pred_rec'
  predict_simple_rec = data.frame(predict_simple_rec)
  
  predict_rm5_rec_raw <-
    read.csv(file = "./data/rm5_predict_data_rec.csv")
  predict_rm5_rec = unique(predict_rm5_rec_raw[, c("WellName", "Date", "Concentration")])
  predict_rm5_rec$Date = as.Date(predict_rm5_rec$Date, format = '%Y-%m-%d')
  predict_rm5_rec$Type = 'pred_rec_rm5'
  predict_rm5_rec = data.frame(predict_rm5_rec)
  
  predict_rm5_raw <- read.csv(file = "./data/rm5_predict_data.csv")
  predict_rm5 = unique(predict_rm5_raw[, c("WellName", "Date", "Concentration")])
  predict_rm5$Date = as.Date(predict_rm5$Date, format = '%Y-%m-%d')
  predict_rm5$Type = 'pred_rm5'
  predict_rm5 = data.frame(predict_rm5)
  
  # Empty reactive values object
  reactive_objects = reactiveValues()
  
  # Select map set up
  map = leaflet::createLeafletMap(session, 'map')
  
  session$onFlushed(once = T, function() {
    output$map <- leaflet::renderLeaflet({
      buildMap(sites = wells, plot_polys = TRUE)
    })
  })
  
  # Table interface
  output$table_input = DT::renderDataTable({
    DT::datatable(
      wells_mlid_param_asmnts,
      selection = 'single',
      rownames = FALSE,
      filter = "top",
      options = list(
        scrollY = '600px',
        paging = FALSE,
        scrollX = TRUE,
        dom = "ltipr"#,
        #searchCols = list(NULL,list(search=paste(reactive_objects$sel_mlid)))
      )
    )
  })
  
  # Map marker click (to identify selected site)
  observe({
    req(wells_long)
    site_click <- input$map_marker_click
    if (is.null(site_click)) {
      return()
    }
    siteid = site_click$id
    reactive_objects$sel_mlid = siteid
  })
  
  
  
  # Table row click (to identify selected site & parameter)
  observe({
    req(input$table_input_rows_selected)
    row_click = input$table_input_rows_selected
    siteid = wells_mlid_param_asmnts[row_click, "Well_Name"]
    reactive_objects$sel_param = wells_mlid_param_asmnts[row_click, "ParameterName"]
    reactive_objects$sel_mlid = siteid
  })
  
  # Change map zoom on table click & update selected heatmap_param to selected row param
  map_proxy = leaflet::leafletProxy("map")
  observeEvent(input$table_input_rows_selected, {
    lat = wells[wells$MonitoringLocationIdentifier == reactive_objects$sel_mlid, "LatitudeMeasure"]
    long = wells[wells$MonitoringLocationIdentifier == reactive_objects$sel_mlid, "LongitudeMeasure"]
    map_proxy %>% leaflet::setView(lng = long,
                                   lat = lat,
                                   zoom = 20)
  })
  # Select profiles & date options based on selected site ID
  observe({
    req(reactive_objects$sel_mlid)
    reactive_objects$sel_profiles = wells_long[wells_long$MonitoringLocationIdentifier ==
                                                 reactive_objects$sel_mlid,]
    profile_dates = unique(reactive_objects$sel_profiles$ActivityStartDate)
    profile_dates = profile_dates[order(profile_dates)]
    reactive_objects$profile_dates = profile_dates
  })
  
  
  # Filter table to match clicked site from map
  input_table_proxy = DT::dataTableProxy('table_input')
  observeEvent(input$map_marker_click, {
    input_table_proxy %>% DT::clearSearch() %>% DT::updateSearch(keywords = list(global = "", columns =
                                                                                   c(
                                                                                     "", paste(reactive_objects$sel_mlid)
                                                                                   )))
  })
  
  # Profile date selection
  output$date_select <- renderUI({
    req(reactive_objects$profile_dates)
    selectInput("date_select",
                "Profile date:",
                reactive_objects$profile_dates)
  })
  output$date_slider <- renderUI({
    req(reactive_objects$profile_dates)
    date_min = min(reactive_objects$profile_dates)
    date_max = max(reactive_objects$profile_dates)
    date_max = as.Date("2026-01-01")
    sliderInput(
      "date_slider",
      "Date range:",
      min = date_min,
      max = date_max,
      value = c(date_min, date_max)
    )
  })
  
  # Generate selected aid
  observe({
    req(input$date_select)
    reactive_objects$selectedActID = reactive_objects$sel_profiles[reactive_objects$sel_profiles$ActivityStartDate ==
                                                                     input$date_select, "ActivityIdentifier"][1]
  })
  # Extract profile assessments & profiles_wide for selected site
  observe({
    req(reactive_objects$sel_mlid, input$date_slider)
    selected_prof_asmnts = wells_ind_prof_asmnts[wells_ind_prof_asmnts$Well_Name == reactive_objects$sel_mlid &
                                                   wells_ind_prof_asmnts$ActivityStartDate >= input$date_slider[1] &
                                                   wells_ind_prof_asmnts$ActivityStartDate <= input$date_slider[2]
                                                 ,]
    selected_prof_asmnts = selected_prof_asmnts[order(selected_prof_asmnts$ActivityStartDate),]
    reactive_objects$selected_prof_asmnts = selected_prof_asmnts
    
    
    
    
    selected_rec_txt = rec_txt[rec_txt$WellName == gsub(" ", "-", reactive_objects$sel_mlid)  &
                                 rec_txt$Date >= input$date_slider[1] &
                                 rec_txt$Date <= input$date_slider[2]
                               ,]
    selected_rec_txt = selected_rec_txt[order(selected_rec_txt$Date),]
    reactive_objects$selected_rec_txt = selected_rec_txt
    
    
    
    selected_predict_simple = predict_simple[predict_simple$WellName == reactive_objects$sel_mlid  &
                                               predict_simple$Date >= input$date_slider[1] &
                                               predict_simple$Date <= input$date_slider[2]
                                             ,]
    selected_predict_simple = selected_predict_simple[order(selected_predict_simple$Date),]
    reactive_objects$selected_predict_simple = selected_predict_simple
    
    selected_predict_simple_rec = predict_simple_rec[predict_simple_rec$WellName == reactive_objects$sel_mlid  &
                                                       predict_simple_rec$Date >= input$date_slider[1] &
                                                       predict_simple_rec$Date <= input$date_slider[2]
                                                     ,]
    selected_predict_simple_rec = selected_predict_simple_rec[order(selected_predict_simple$Date),]
    reactive_objects$selected_predict_simple_rec = selected_predict_simple_rec
    
    
    ori_data = selected_prof_asmnts[c("Well_Name", "do_pct_exc", "ActivityStartDate")]
    ori_data = plyr::rename(
      ori_data,
      c(
        "Well_Name" = "WellName",
        "do_pct_exc" = "Concentration",
        "ActivityStartDate" = "Date"
      )
    )
    
    ori_data$Type = "ori"
    total_data <- rbind(rec_txt, predict_simple_rec)
    total_data <- rbind(total_data, ori_data)
    total_data <- rbind(total_data, predict_rm5_rec)
    total_data <- rbind(total_data, predict_rm5)
    total_data <- rbind(total_data, predict_simple)
    total_data = total_data[total_data$WellName == reactive_objects$sel_mlid, ]
    reactive_objects$selected_rbinded = total_data
  })
  
  output$ggPlot = renderPlotly({
    req(reactive_objects$selected_prof_asmnts)
    
    ggplot(reactive_objects$selected_rbinded,
           aes(Date, Concentration)) + geom_point(aes(colour = factor(Type)), show.legend = FALSE)
  })
  
  output$mymap <- renderLeaflet({
    date_time = format(input$slider, "%Y%m")
    imgPath = paste(projectPath, "/data/tif/Conc.", date_time, ".tif", sep = "")
    r <- raster(imgPath)
    pal <-
      colorNumeric(c("#FFFFFF", "#FF7F27", "#FC0505"), values(r),
                   na.color = "transparent")
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addRasterImage(r,
                     colors = pal,
                     opacity = 0.5,
                     maxBytes = 123123123)
  })
}

## run app
shinyApp(ui = ui, server = server)
# rsconnect::setAccountInfo(name='yifanluo',
#                           token='801F6411CB4C8EFD33298155AE6A8725',
#                           secret='0B0CMhurDaBk0BtwqUEx+VxVguLFT+9yt4VYzwx2')
#
# library(rsconnect)
# rsconnect::deployApp()
