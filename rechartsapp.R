library(echarts4r)
library(echarts4r.maps)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(purrr)
library(dplyr)

orange <- read.csv("oranges.csv", header = TRUE, stringsAsFactors = FALSE)

data_by_region <- data.frame(
  Name = LETTERS[1:8],
  Region = c("Auckland", "Bay of Plenty", "Canterbury", "Gisborne", "Hawke's Bay", "Manawatu-Wanganui", "Marlborough","Nelson"),
  long = c(167.7994, 170.1401, 170.4842, 171.607, 172.8973, 173.283966, 173.666664, 174.0602),
  lat = c(-45.9028, -45.6166, -43.5883, -41.8833298, -41.7573, -41.288, -41.270634, -40.7549),
  Oranges = c(227, 252, 373, 363, 287, 307, 308, 293)
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
    fluidRow(column(6,
                    echarts4rOutput(outputId = "myMap1")
    ))
  )
)

server = function(input, output, session) {
   
    output$myMap1 <- renderEcharts4r({
      data_by_region$Region <- factor(data_by_region$Region)
      
      e_common(font_family = "monospace")
      
      map <- data_by_region %>%
        e_charts(Region) %>%
        e_map_register("NZ", nz_json) %>%
        e_map(Oranges, map = "NZ") %>%
        e_visual_map(
          Oranges,
          top = "20%",
          left = "0%",
          inRange = list(color = c("hotpink", "#3366FF","#6699FF", "#66CCFF", "#33CCFF")),
          type = "piecewise",
          splitList = list(
            list(min = 300),
            list(min = 250, max = 300),
            list(min = 100, max = 250),
            list(value = 1, label = "None"),
            list(value = 0, label = "Top 5")
          ),
          formatter = htmlwidgets::JS("function(value, index, values){
                if(index == 'Infinity'){
                return ' > ' + value.toLocaleString()
                }else if (value != 0){
                return value.toLocaleString() + ' - ' + index.toLocaleString()
                }else{
                return value.toLocaleString()
                }
            }")) %>%
        e_tooltip(formatter = htmlwidgets::JS("
                         function(params){
                        return('<strong>' + params.name +
                     '</strong><br />Total: ' +  echarts.format.addCommas(params.value)) + ' oranges'}"))
      newdata <- data_by_region %>% top_n(5)
      
      for(i in 1:nrow(newdata)){
        data <- list(value = newdata$Name[i], coord = c(newdata$long[i], newdata$lat[i]))
        map <- map %>% e_mark_point(data = data,
                                    symbolSize = c(40, 40),
                                    label = list(fontSize = 10, color = "black", fontWeight = "bolder"),
                                    itemStyle = list(
                                      color = "hotpink",
                                      borderColor = "hotpink"
                                    ))
        
      }
      
      map
    })
    
}

shinyApp(ui, server)
