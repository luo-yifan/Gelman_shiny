library(shiny)
library(leaflet)
library(raster)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
projectPath ='/Users/zhangziliang/WorkSpace/lyf/gelman_shiny/ggif'

ui <- fluidPage(
    leafletOutput("mymap"),
    p(),
    sidebarLayout(
        sidebarPanel(
            sliderInput("slider",
                        "Dates:",
                        min = as.Date("1986-02-01","%Y-%m-%d"),
                        max = as.Date("2022-11-01","%Y-%m-%d"),
                        value=as.Date("2016-12-01"),
                        timeFormat="%Y-%m")
        ),
        mainPanel(
            plotOutput("distPlotLactul"))
        
    )

)

server <- function(input, output, session) {
    output$mymap <- renderLeaflet({
        date_time = format(input$slider, "%Y%m")
        imgPath = paste(projectPath,"/tif/Conc.",date_time,".tif", sep = "")
        r <- raster(imgPath)
        pal <- colorNumeric(c("#FFFFFF", "#FF7F27", "#FC0505"), values(r),
                            na.color = "transparent")
        leaflet()%>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            )%>%
        addRasterImage(r, colors=pal, opacity = 0.5, maxBytes = 123123123)
    })
}

shinyApp(ui, server)
