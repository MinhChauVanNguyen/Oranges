library(recharts)
library(echarts4r)
library(echarts4r.maps)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(purrr)
library(dplyr)


data_by_region <- data.frame(
  Name = rep("A", 8),
  Region = rep("Northland", 8),
  long = rep(174.3223, 8),
  lat = rep(-35.7047, 8),
  Year = 2013:2020,
  Amount = c(227, 252, 373, 363, 287, 307, 308, 293)
)

# Remove Chatham Island for echarts4r maps
nz_file <- system.file("New_Zealand.json", package = "echarts4r.maps") 
nz_json <- jsonlite::read_json(nz_file)


# get names of polygons
names <- nz_json$features %>% 
  map("properties") %>% 
  map("name") %>% 
  unlist()

island_index <- grep("Chatham", names)

nz_json$features[[island_index]] <- NULL


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    selectInput(inputId = "year", 
                label = "pick a year",
                choices = unique(factor(data_by_region$Year))),
    fluidRow(
      column(
        width = 5, tags$div(
      fluidRow(
        column(
          width = 6,
          materialSwitch(
            inputId = "switchMap",
            value = TRUE)
    ))))),
    fluidRow(column(6, uiOutput(outputId = "map")))
  )
)

server = function(input, output, session) {
  
  output$map <- renderUI({
    if(input$switchMap == TRUE){
      echarts4rOutput(outputId = "myMap1")
    }else{
      echarts4rOutput(outputId = "myMap2")
    }
  })
   
    output$myMap1 <- renderEcharts4r({
      
      data_by_year <- data_by_region[data_by_region$Year == req(input$year),]
      data_by_year <- data.frame(data_by_year)
      data_by_year$Region <- factor(data_by_year$Region)
      
      newdata <- data_by_year %>% top_n(5)
      
      for(i in 1:nrow(newdata)){
        minh <- list(name = newdata$Name[i], coord = c(newdata$lng[i], newdata$lat[i]))
        map <- map %>% e_mark_point(data = minh,
                                    symbolSize = c(30, 40), animation = TRUE,
                                    itemStyle = list(color ="pink", opacity = 0.5, borderColor = "blue"),
                                    effect=list(show=TRUE)) 
      }
      
      data_by_year %>%
        e_charts(Region) %>%
        e_map_register("NZ", nz_json) %>%
        e_map(Amount, map = "NZ") %>%
        e_visual_map(
          Amount,
          top = "20%",
          left = "0%",
          inRange = list(color = c("#3366FF","#6699FF", "#66CCFF", "#33CCFF")),
          type = "piecewise",
          splitList = list(
            list(min = 300),
            list(min = 250, max = 300),
            list(min = 100, max = 250),
            list(value = 0, label = "None")
          ),
          formatter = htmlwidgets::JS("function(value, index, values){
                if(index == 'Infinity'){
                return ' > ' + value.toLocaleString()
                }else if (value != 0){
                return value.toLocaleString() + ' - ' + index.toLocaleString()
                }else{
                return value.toLocaleString()
                }
            }"))
    })
    
    
}

shinyApp(ui, server)