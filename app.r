



library(shiny)
source("ui.R")
source("server.R")



## run app
shinyApp(ui = ui, server = server)

# library(rsconnect)
# rsconnect::setAccountInfo(name='yifanluo',
#                           token='801F6411CB4C8EFD33298155AE6A8725',
#                           secret='0B0CMhurDaBk0BtwqUEx+VxVguLFT+9yt4VYzwx2')
# rsconnect::deployApp()
