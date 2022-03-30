library(wqTools)
library(magrittr)
library(ggplot2)
library(plotly)
library(leaflet)
library(raster)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
projectPath = getwd()

gelman_data = read.csv(file = "./data/well_data.csv")
well_types = data.frame(read.csv(file = "./data/well_types.csv"))
rec_txt_raw <- read.csv(file = "./data/all_data_rec.csv")
predict_simple_raw <-
  read.csv(file = "./data/all_predict_data.csv")
predict_simple_rec_raw <-
  read.csv(file = "./data/all_predict_data_rec.csv")
predict_rm5_rec_raw <-
  read.csv(file = "./data/rm5_predict_data_rec.csv")
predict_rm5_raw <- read.csv(file = "./data/rm5_predict_data.csv")

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
  
  wells = wells[wells$MonitoringLocationIdentifier %in% wells_long$MonitoringLocationIdentifier, ]
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
                                                %in% wells_long$ActivityIdentifier, ]
  wells_ind_prof_asmnts$Well_Name = wells_ind_prof_asmnts$MonitoringLocationIdentifier
  wells_ind_prof_asmnts = plyr::rename(wells_ind_prof_asmnts, c("Value" =
                                                                  "do_pct_exc"))
  wells_ind_prof_asmnts = data.frame(wells_ind_prof_asmnts)
  
  wells_mlid_param_asmnts = unique(gelman_data[, c("Bore", "Depth2", "Elevation", "lat", "lon")])
  wells_mlid_param_asmnts = plyr::rename(
    wells_mlid_param_asmnts,
    c(
      "Bore" = "Well_Name",
      "Depth2" = "Depth",
      "Elevation" = "Elevation",
      "lat" = "Latitude",
      "lon" = "Longtitude"
    )
  )
  
  #wells_mlid_param_asmnts$ParameterName = "Sampling Well"
  wells_mlid_param_asmnts = data.frame(wells_mlid_param_asmnts)
  wells_mlid_param_asmnts =
    merge(
      x = wells_mlid_param_asmnts,
      y = well_types[, c("Well_Name", "Well_Types")],
      by = "Well_Name",
      all.x = TRUE
    )
  
  #wells_mlid_param_asmnts = cbind(wells_mlid_param_asmnts$Well_Name, wells_mlid_param_asmnts)
  rec_txt = unique(rec_txt_raw[, c("WellName", "Date", "Concentration")])
  rec_txt$Date = as.Date(rec_txt$Date, format = '%Y-%m-%d')
  rec_txt$Type = 'RF interpolation'
  rec_txt = data.frame(rec_txt)
  
  predict_simple = unique(predict_simple_raw[, c("WellName", "Date", "Concentration")])
  predict_simple$Date = as.Date(predict_simple$Date, format = '%Y-%m-%d')
  predict_simple$Type = 'Five-year prediction'
  predict_simple = data.frame(predict_simple)
  
  predict_simple_rec = unique(predict_simple_rec_raw[, c("WellName", "Date", "Concentration")])
  predict_simple_rec$Date = as.Date(predict_simple_rec$Date, format = '%Y-%m-%d')
  predict_simple_rec$Type = 'RF prediction'
  predict_simple_rec = data.frame(predict_simple_rec)
  
  predict_rm5_rec = unique(predict_rm5_rec_raw[, c("WellName", "Date", "Concentration")])
  predict_rm5_rec$Date = as.Date(predict_rm5_rec$Date, format = '%Y-%m-%d')
  predict_rm5_rec$Type = 'RF predict validation'
  predict_rm5_rec = data.frame(predict_rm5_rec)
  
  predict_rm5 = unique(predict_rm5_raw[, c("WellName", "Date", "Concentration")])
  predict_rm5$Date = as.Date(predict_rm5$Date, format = '%Y-%m-%d')
  predict_rm5$Type = 'Raw predict validation'
  predict_rm5 = data.frame(predict_rm5)
  
  # Empty reactive values object
  reactive_objects = reactiveValues()
  
  # Select map set up
  map = leaflet::createLeafletMap(session, 'map')
  
  session$onFlushed(once = T, function() {
    output$map <- leaflet::renderLeaflet({
      wqTools::buildMap(sites = wells)
    })
  })
  
  # Table interface
  output$table_input = DT::renderDataTable({
    DT::datatable(
      wells_mlid_param_asmnts,
      selection = 'single',
      rownames = FALSE,
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
    showNotification(paste(siteid,"clicked"))
  })
  
  # Table row click (to identify selected site & parameter)
  # observe({
  #   showNotification("observe activate")
  #   req(input$table_input_rows_selected)
  #   row_click = input$table_input_rows_selected
  #   siteid = wells_mlid_param_asmnts[row_click, "Well_Name"]
  #   reactive_objects$sel_param = wells_mlid_param_asmnts[row_click, "ParameterName"]
  #   reactive_objects$sel_mlid = siteid
  #   showNotification(paste(siteid,"clicked"))
  # })
  
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
  # input_table_proxy = DT::dataTableProxy('table_input')
  # observeEvent(input$map_marker_click, {
  #   input_table_proxy %>% DT::clearSearch() %>%
  #     DT::updateSearch(keywords =list(global = "", 
  #                             columns =
  #                               c(
  #                                 paste(reactive_objects$sel_mlid), "", "", "",""
  #                               )))
  # })
  # 
  # Profile date selection
  output$date_select <- renderUI({
    req(reactive_objects$profile_dates)
    selectInput("date_select",
                "Profile date:",
                reactive_objects$profile_dates)
  })
  
  # Generate selected aid
  observe({
    req(input$date_select)
    reactive_objects$selectedActID = reactive_objects$sel_profiles[reactive_objects$sel_profiles$ActivityStartDate ==
                                                                     input$date_select, "ActivityIdentifier"][1]
  })
  
  # Extract profile assessments & profiles_wide for selected site
  observe({
    req(reactive_objects$sel_mlid)
    selected_prof_asmnts = wells_ind_prof_asmnts[wells_ind_prof_asmnts$Well_Name == reactive_objects$sel_mlid
                                                 ,]
    selected_prof_asmnts = selected_prof_asmnts[order(selected_prof_asmnts$ActivityStartDate),]
    reactive_objects$selected_prof_asmnts = selected_prof_asmnts
    selected_rec_txt = rec_txt[rec_txt$WellName ==  reactive_objects$sel_mlid
                               ,]
    selected_rec_txt = selected_rec_txt[order(selected_rec_txt$Date),]
    reactive_objects$selected_rec_txt = selected_rec_txt
    
    selected_predict_simple = predict_simple[predict_simple$WellName == reactive_objects$sel_mlid
                                             ,]
    selected_predict_simple = selected_predict_simple[order(selected_predict_simple$Date),]
    reactive_objects$selected_predict_simple = selected_predict_simple
    
    selected_predict_simple_rec = predict_simple_rec[predict_simple_rec$WellName == reactive_objects$sel_mlid
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
    
    ori_data$Type = "Historical record"
    total_data <- rbind(ori_data, predict_simple)
    # total_data <- rbind(rec_txt, predict_simple_rec)
    # total_data <- rbind(total_data, ori_data)
    # total_data <- rbind(total_data, predict_rm5_rec)
    # total_data <- rbind(total_data, predict_rm5)
    # total_data <- rbind(total_data, predict_simple)
    
    total_data = total_data[total_data$WellName == reactive_objects$sel_mlid, ]
    reactive_objects$selected_rbinded = total_data
  })
  
  # output$ggPlot = renderPlotly({
  #   req(reactive_objects$selected_prof_asmnts)
  #   
  #   ggplot(reactive_objects$selected_rbinded,
  #          aes(Date, Concentration)) +
  #     geom_point(aes(colour = factor(Type)), show.legend = FALSE) +
  #     geom_hline(
  #       yintercept = 1,
  #       size = 0.5,
  #       linetype = "dashed",
  #       color = "grey80"
  #     ) +
  #     geom_hline(
  #       yintercept = 4,
  #       size = 0.5,
  #       linetype = "dashed",
  #       color = "grey60"
  #     ) +
  #     geom_hline(
  #       yintercept = 7.2,
  #       size = 0.5,
  #       linetype = "dashed",
  #       color = "grey40"
  #     ) +
  #     geom_hline(
  #       yintercept = 85,
  #       size = 0.5,
  #       linetype = "dashed",
  #       color = "grey20"
  #     ) +
  #     scale_y_continuous(limits = c(0, max(
  #       1.0,
  #       max(reactive_objects$selected_rbinded$Concentration)
  #     )))
  # })
  # 
  output$mymap <- renderLeaflet({
    date_time = gsub('-', '', input$month_slider)
    imgPath = paste(projectPath, "/data/tif/Conc.", date_time, ".tif", sep = "")
    r <- raster(imgPath)
    color_t = rev(
      c(
        '#03071E',
        '#370617',
        '#6A040F',
        '#9D0208',
        '#D00000',
        '#DC2F02',
        '#E85D04',
        '#F48C06',
        '#FAA307',
        '#ffb700',
        '#ffd000',
        '#ffea00'
      )
    )
    
    pal <-
      colorQuantile(
        color_t,
        c(1, 4 , 7.2 , 85 , 150 , 280 , 500 , 1000 , 1900 , 3000, 5000, 3000000),
        n = 13,
        na.color = "transparent"
      )
    
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addRasterImage(r,
                     colors = pal,
                     opacity = 0.8,
                     maxBytes = 123123123) %>%
      # addPolygons(r,
      #             lng = long,
      #             lat = lat,
      #             color = "#03F",
      #             weight = 5,
      #             opacity = 0.5) %>%
      setView(lng = -83.792,
              lat = 42.284,
              zoom = 13) %>%
      addLegend(
        "bottomright",
        colors = color_t,
        labels = c(
          '<1' ,
          '1-4' ,
          '4-7.2' ,
          '7.2-85' ,
          '85-150' ,
          '150-280' ,
          '280-500' ,
          '500-1000' ,
          '1000-1900' ,
          '1900-3000',
          '3000-5000',
          '>5000'
        ),
        opacity = 0.8
      )
  })
}
