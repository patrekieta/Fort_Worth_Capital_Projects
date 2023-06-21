library(shinythemes)
library(shinyjqui)
library(rvest)
library(tidyverse)
library(rstudioapi)
library(leaflet)
library(shiny)

# setwd("C:/Users/patar/OneDrive/Documents/Fort Worth Capital Projects/Fort_Worth_Capital_Projects")
map_data <- read.csv("www/map_data.csv")
nomap_data <- read.csv("www/nomap_data.csv")
last_updated <- map_data$Date_updated[1]


ui <- fluidPage( theme = shinytheme("superhero"),
                tags$head(
                  tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                            padding-top:1.5vh !important; 
                            padding-bottom:0vh !important;
                            height: 5vh;
                            font-size:1.5vw;
                            
                            }
                           .navbar {min-height:25px !important; width:100%;margin-bottom:0vh;}
                           .container-fluid {padding:0px;}
                           .row {margin:0px;}'))
                  ),
  navbarPage(title = "",position = "static-top", id = "pages", fluid = TRUE,
             header = div(style="color:#FFFFFF;font-family:courier;font-size:5vmin;text-align:center;padding:0px;width:99vw;", HTML("<u>Fort Worth Capital Projects</u>")),
             tabPanel(title = "Home",
                      div(style="margin:0 auto;padding-top:1vh;height:82vh;width:97vw;",
                                           leafletOutput(outputId = "leafletMap", height = "100%", width = "97vw")
                                                   ),
                      div(style = "text-align:right;font-size:1.5vmin;position:fixed;bottom:1vh;right:1vw;",paste0("Last updated: ",last_updated))
                      ),
             tabPanel(title = "Additional Projects", value = "additional_projects",
                      lapply(seq_len(nrow(nomap_data)), function(i){uiOutput(paste0('button',i))})
                      ),
             tabPanel(title = "About", 
                      mainPanel(
                        div(style = "width:97vw;margin:8vh auto;text-align:center;font-size:4vmin;", HTML(paste0('<p>All data for this project comes from: </p><a href="https://www.fortworthtexas.gov/projects">https://www.fortworthtexas.gov/projects</a>')))
                      )
                      )
             )

)


server <- function(input, output) {
  
  page_visit_initial <- reactiveValues(level = "0")
  
  
  output$leafletMap <- renderLeaflet({
    leaflet(data = map_data) %>%
      setView(lat = 32.75, lng = -97.333333, zoom = 11) %>%
      addTiles() %>%
      addMarkers(lat = ~latitude, lng = ~longitude,layerId = map_data$Id, popup = ~title)
  })

observeEvent(input$leafletMap_marker_mouseover,{
  mouseover_id <- as.integer(input$leafletMap_marker_mouseover$id)
  mouseover_lat <- as.double(input$leafletMap_marker_mouseover$lat)
  mouseover_lng <- as.double(input$leafletMap_marker_mouseover$lng)
  offset <- 4287177/input$leafletMap_zoom^8
  leafletProxy(mapId = "leafletMap") %>% addPopups(lat = mouseover_lat + offset, lng = mouseover_lng, popup = map_data$title[map_data$Id==mouseover_id], layerId = "hoverPopup")
})

observeEvent(input$leafletMap_marker_mouseout,{
  leafletProxy(mapId = "leafletMap") %>% clearPopups()
})
  
observeEvent(input$leafletMap_marker_click, { 
  p <- input$leafletMap_marker_click
  location <- as.numeric(p$id)
  output$sidetext <- showModal(modalDialog(title =  HTML('<u style="font-size:2vmin;">Project Details:</u>'),
                                           div(HTML(paste0('<p style="font-size:1.6vmin;">Project Title:</p>
                                                           <p style="font-size:1.4vmin;">',map_data$title[map_data$Id==p$id],'</p>
                                                           <br>
                                                           <p style="font-size:1.6vmin;">Project Type:</p>
                                                           <p style="font-size:1.4vmin;">',map_data$type[map_data$Id==p$id],'</p>
                                                           <br>
                                                           <p style="font-size:1.6vmin;">Project Details:</p>
                                                           <p style="font-size:1.3vmin;">',map_data$details[map_data$Id==p$id],'</p>
                                                           <br>
                                                           <p style="font-size:1.6vmin;">Project Location:</p>
                                                           <p style="font-size:1.4vmin;">',map_data$location[map_data$Id==p$id],'</p>
                                                           <br>
                                                           <p style="font-size:1.6vmin;">Project Value:</p>
                                                           <p style="font-size:1.4vmin;">',map_data$value[map_data$Id==p$id],'</p>
                                                           <br>
                                                           <p style="font-size:1.6vmin;">Project District(s):</p>
                                                           <p style="font-size:1.4vmin;">',map_data$district[map_data$Id==p$id],'</p>
                                                           <br>
                                                           <p style="font-size:1.6vmin;">Project Completion Date:</p>
                                                           <p style="font-size:1.4vmin;">',map_data$completion.date[map_data$Id==p$id],'</p>
                                                           <br>
                                                           <p style="font-size:1.6vmin;">Project Status:</p>
                                                           <p style="font-size:1.4vmin;">',map_data$status[map_data$Id==p$id],'</p>
                                                           <br>
                                                           <p style="font-size:1.6vmin;">Project URL:</p>
                                                           <a style="font-size:1.4vmin;" href="',map_data$URL[map_data$Id==p$id],'">',map_data$URL[map_data$Id==p$id],'</a>')))
                                           ,easyClose = TRUE))
})


observeEvent(input$pages,{
  if(input$pages=="additional_projects" & page_visit_initial$level =="0"){
    showModal(modalDialog(title = "Description:","This page contains additional projects that do not exist on the map.",easyClose = TRUE))
    page_visit_initial$level <- "1"
    }
})


lapply(seq_len(nrow(nomap_data)), function(i){
  output[[paste0('button',i)]] <- renderUI({
    div(style="height:16vh;width:98vw;padding:1vh 0vw;text-align:center;display:flex;justify-content:center;align-items:center;",actionButton(inputId = paste0("Add_Button",i)
                                                          ,label=HTML(paste0('<p style="font-size:2vmin;padding:0px;"><u>',nomap_data$title[nomap_data$Id==i],
                                                                             '</u></p><p style="font-size:1.8vmin;white-space:normal;padding:0px;">',nomap_data$details[nomap_data$Id==i],'</p>'))
                                                          ,style = "height:14vh;width:80vw;margin:0 auto;padding:0px;" , onclick = paste0("window.open('",nomap_data$URL[nomap_data$Id==i],"','_blank')")
                                                          ))
  })
})  

# lapply(seq_len(nrow(nomap_data)), function(j){
#   observeEvent(input[[paste0("Add_Button",j)]],{
#     button <- j
#     browseURL(nomap_data$URL[nomap_data$Id==j])
#   })
# })

}

shinyApp(ui = ui, server = server)
