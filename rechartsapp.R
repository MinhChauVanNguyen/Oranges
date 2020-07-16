#library(recharts)
library(echarts4r)
library(echarts4r.maps)
library(shiny)
library(shinyWidgets)
library(shinydashboard)

orange <- read.csv("oranges.csv", header = TRUE, stringsAsFactors = FALSE)

data_by_region <- group_by(orange, Name, Region, long, lat, Year) %>%
  summarise(Oranges = sum(Total))

nz_file <- system.file("New_Zealand.json", package = "echarts4r.maps") 
nz_json <- jsonlite::read_json(nz_file)

# identify polygon causing issues
library(purrr)

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
    uiOutput("YEARS"),
    # radioGroupButtons(
    #   inputId = "switchMap", label = NULL, justified = TRUE,
    #   choiceNames = c(
    #     paste(icon("hand-point-right"), "Guide"),
    #     paste(icon("laptop"), "Info")
    #   ),
    #   choiceValues = c("alerttwo", "alertone"),
    #   status = "primary"
    # ),
    uiOutput(outputId = "check"),
    #div(style="margin-bottom:-30px;", echarts4rOutput(outputId = "MAP"))
    fluidRow(column(6, uiOutput(outputId = "orangemap")))
    # eChartOutput(outputId = "myChart"),
    # echarts4rOutput(outputId = "MAP")
  )
)

server = function(input, output, session) {
    output$YEARS <- renderUI({
      selectInput(inputId = "year", 
                  label = "pick a year",
                  choices = unique(factor(data_by_region$Year)))
    })
    
   output$check <- renderUI(
     checkboxInput(inputId = "switchMap", label = "switch", value = TRUE),
   )
   
    output$orangemap <- renderUI({
     if(req(input$switchMap)){
       eChartOutput(outputId = "myChart")
     }else{
       echarts4rOutput(outputId = "MAP")
     }
    })
   
    output$MAP <- renderEcharts4r({
      data_by_year <- data_by_region[data_by_region$Year == req(input$year),]
      data_by_year <- data.frame(data_by_year)
      data_by_year$Region <- factor(data_by_year$Region)
      
      data_by_year %>%
        e_charts(Region) %>%
        e_map_register("NZ", nz_json) %>%
        e_map(Oranges, map = "NZ") %>%
        e_visual_map(
          Oranges,
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
            }")) %>%
        e_tooltip(formatter = htmlwidgets::JS("
                         function(params){
                        return('<strong>' + params.name +
                     '</strong><br />Total: ' +  echarts.format.addCommas(params.value)) + ' oranges'}")) %>%
        e_title(
          text = "Oranges grouped by year and region",
          subtext = "Choose a year and hover over the map for more information") %>%
        e_text_style(
          fontFamily = "monospace"
        )
    })
    
    output$myChart <- renderEChart({
      yeardata <- data_by_region[data_by_region$Year == input$year, ]

      top5dat <- as.data.frame(yeardata) %>% top_n(5)

      top5dat <- data.frame(top5dat)

      names(top5dat) <- c('Family', 'name', 'lng', 'lat', 'Year', 'value')

      echartr(NULL, type='map_world', subtype='New Zealand') %>%
        addMP(series='Top 5',
              data=top5dat,
              symbol='pin',
              symbolSize=JS('function (v) {return 10 + v/50;}'),
              effect=list(show=TRUE),
              itemStyle = list(normal = itemStyle(color = "#EE82EE"))
        ) %>%
        addGeoCoord(top5dat[,c('name', 'lng', 'lat')]) %>%
        setToolbox(show = FALSE) %>%
        setSeries(hoverable=FALSE, itemStyle=list(
          normal=itemStyle(
            labelStyle = labelStyle(color="#EE82EE"),
            borderColor='rgba(100,149,237,1)', borderWidth=0.5,
            areaStyle=areaStyle(color='#1b1b1b')))) %>%
        setLegend(show = FALSE) %>%
        setTitle('Advanced map', 'Fictious Data', pos=11) %>%
        setTooltip(textStyle = textStyle(fontFamily="monospace"),
                   formatter=JS('function (params) {
  return params.seriesName + "<br/>" + params.name + ": " + params.value + " oranges" }'))
    })
}

shinyApp(ui, server)