isolate({
  updateTabItems(session, "sidebarmenu", "tabOne")
})

output$DATA <- renderUI({
  selectInput(inputId = "Names",
              label = "Choose a Family Name", selectize = FALSE,
              choices = sort(unique(factor(orange$Name))))
})

datasetInput <- reactive({
  inputdata <- orange[orange$Name == req(input$Names), ]
})

tsdata <- reactive({
  inputdata <- datasetInput()
  tsdata <- ts(inputdata$Total, frequency = 12,
               start = c(min(inputdata$Year), min(inputdata[inputdata$Year == min(inputdata$Year), "Month"])))
})

################################# Tab Item 1 ######################################
output$YEARS <- renderUI({
  selectInput(inputId = "year", 
              label = "year",
              selected = "2015",
              choices = unique(factor(data_by_region$Year)))
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


################################# Tab Item 2 ######################################

output$yearly1 <- renderValueBox({
  datafrme <- datasetInput()
  highValue(datafrme = datafrme, year = 2017)
})

output$yearly2 <- renderValueBox({
  datafrme <- datasetInput()
  highValue(datafrme = datafrme, year = 2016)
})

output$yearly3 <- renderValueBox({
  datafrme <- datasetInput()
  highValue(datafrme = datafrme, year = 2015)
})

output$OUT <- renderUI({
  HTML(paste0("<mark>", "Summary table for the Family selected", 
              "</mark>"))
})

output$TABLE <-  DT::renderDataTable({
  orange <- datasetInput()
  oranges <- orange[!(names(orange) %in% c("X", "Region", "long", "lat"))]
  DT::datatable(oranges, rownames = FALSE, escape = FALSE,
                caption = htmltools::tags$caption("Table: Family", input$Names,
                                                  style = "caption-side:bottom; text-align:center;"),
                options = list(lengthMenu = c(5,10,15,20),
                               scrollX = TRUE,
                               searching = TRUE,
                               pagingType = "simple",
                               language = list(info = '_TOTAL_ records', 
                                               paginate = list(previous = 'Back', `next` = 'Forward')))
  )
})

output$hchart <- renderHighchart({
  ts <- tsdata()
  highchart(type = "stock") %>%
    hc_add_series(ts, type = "line", color = "#5F9EA0",
                  tooltip = list(pointFormat = "{point.y} oranges")) %>%
    hc_tooltip(crosshairs = FALSE, borderWidth = 2) %>%
               #formatter = JS("function(){return (this.y + ' oranges' )}")) %>%
    hc_scrollbar(enabled = FALSE) %>%
    hc_navigator(enabled = FALSE) %>%
    hc_rangeSelector(enabled = FALSE) %>%
    hc_xAxis(title = list(text = "Year", style = list(color = "darkorange", fontWeight = "bold")),
             lineColor = "black") %>%
    hc_yAxis(title = list(text = "Monthly bought oranges",
                          style = list(color = "darkorange", fontWeight = "bold")), 
             opposite = FALSE, lineWidth = 1, lineColor = "black", gridLineColor = "white") %>%
    hc_chart(borderColor = '#ADD8E6', borderRadius = 10, borderWidth = 2, 
             backgroundColor = list(
               linearGradient = c(0, 0, 500, 500),
               stops = list(
                 list(0, 'rgb(255, 255, 255)'),
                 list(1, 'rgb(140, 224, 255)')
               )),
             style = list(fontFamily = "monospace")) %>%
    hc_title(text = "<b>Total oranges bought over the years line chart</b>",
             margin = 20, align = "center",
             style = list(color = "darkorange", useHTML = TRUE)) %>%
    hc_legend(enabled = FALSE)
})


################################## Tab Item 3 ######################################
fit <- reactive({
  tsdata <- tsdata()
  #training <- window(tsdata, 
  #            start = c(floor(time(tsdata)[floor(length(tsdata)*0.8)]),
  #match(month.abb[cycle(tsdata)][floor(length(tsdata)*0.8)], month.abb)+1))
  #test <- window(tsdata, 
  #              start = c(floor(time(tsdata)[floor(length(tsdata)*0.8)]), match(month.abb[cycle(tsdata)][floor(length(tsdata)*0.8)], month.abb) + 1))
  arimadata <- auto.arima(tsdata)
})

forecasting <- reactive({
  model <- suppressWarnings(fit())
  tsdata <- tsdata()
  ARIMA.mean <- model %>% forecast(h = 36, level = c(30,50,70))
})

datat <- reactive({
  fc <- forecasting()
  fc <- data.frame(fc)
  data <- setNames(cbind(rownames(fc), fc, row.names = NULL), 
                   c("MonthYear", "Point Forecast", "Lo 30", "Hi 30", "Lo 50", "Hi 50", "Lo 70", "Hi 70"))
  for(i in names(data)[2:8]){
    data[[i]] <- round(as.numeric(data[[i]]))
  }
  data
})

output$TABLE2 <- DT::renderDataTable(
  DT::datatable(suppressWarnings(datat()), rownames = FALSE, 
                caption = htmltools::tags$caption(
                  style = 'caption-side:bottom; text-align:center; color:orange; font-weight:bold; font-size:17px;', "Table: ARIMA model"),
                options = list(pageLength = 5, lengthMenu = c(5,10,15,20),
                               pagingType = "full", searching = FALSE,
                               language = list(info = '_TOTAL_ records'))
  )
)

fitdf <- reactive({
  tsdata <- req(tsdata())
  fit <- tslm(tsdata ~ trend + season)
})

forecastingtwo <- reactive({
  tsdata <- tsdata()
  fit <- fitdf()
  fc <- forecast(fit, h = 36, level = c(30, 50, 70))
  fc <- data.frame(fc)
  data <- setNames(cbind(rownames(fc), fc, row.names = NULL), 
                   c("MonthYear", "Point Forecast", "Lo 30", "Hi 30", "Lo 50", "Hi 50", "Lo 70", "Hi 70"))
  for(i in names(data)[2:8]){
    data[[i]] <- round(as.numeric(data[[i]]))
  }
  data
})

output$TABLE3 <- DT::renderDataTable(
  DT::datatable(suppressWarnings(forecastingtwo()), rownames = FALSE, 
                caption = htmltools::tags$caption(
                  style = 'caption-side:bottom; text-align:center; color:orange; font-weight:bold; font-size:17px;', "Table: Regression Model"),
                options = list(pageLength = 5, lengthMenu = c(5,10,15,20),
                               pagingType = "full", searching = FALSE, 
                               language = list(info = '_TOTAL_ records'))
               )
)



