tmap_mode("plot")

output$DATA3 <- renderUI({
  selectInput(inputId = "Names3",
              label = "Choose a Family Name", selectize = FALSE,
              choices = sort(unique(factor(orange$Name))))
})

data3 <- reactive({
  inputdata <- orange[orange$Name == req(input$Names3), ]
})

tsdata3 <- reactive({
  inputdata <- data3()
  tsdata <- ts(inputdata$Total, frequency = 12,
               start = c(min(inputdata$Year), min(inputdata[inputdata$Year == min(inputdata$Year), "Month"])))
})

mapdata <- reactive({
  #if(isTRUE(input$Names %in% LETTERS[1:16])){
    nz[nz$Name == nz$Name[nz$People == req(input$Names3)],]
  #}else{
   # nz[nz$Name == nz$Name[nz$People2 == req(input$Names)],]
  #}
})

output$my_tmap <- renderPlot({
  tm_shape(nz) + tm_borders(lty = "solid", lwd = 1, col = "white") +      
    tm_shape(mapdata()) + tm_fill(col = "color", title = "Region", labels = nz$Name[nz$People == req(input$Names3)]) + 
    tm_layout(frame = FALSE, bg.color = "steelblue", legend.width = 1, legend.text.size = 1) +
    tm_borders(lwd = 2, col = "white") 
  #tm_basemap(NULL)
})

forecastingy <- reactive({
  tsdata <- req(tsdata3())
  fit <- tslm(tsdata ~ trend + season)
  forecastdata <- forecast(fit, h = 36, level = c(30,50,70))
  forecastdata
})

output$PLOT3 <- renderDygraph({
  tsdata <- req(tsdata3())
  reg.mean <- forecastingy()
  graph <- cbind(actuals = tsdata, pointfc_mean = reg.mean$mean,
                  lower_70 = reg.mean$lower[,3], upper_70 = reg.mean$upper[,3],
                  lower_50 = reg.mean$lower[,2], upper_50 = reg.mean$upper[,2],
                  lower_30 = reg.mean$lower[,1], upper_30 = reg.mean$upper[,1])
  dygraph(graph, main = "Linear Regression with seasonal dummy predictors") %>%
    dySeries(name = "actuals", fillGraph = TRUE, axis = "y2") %>%
    dySeries(name = "pointfc_mean", label = "forecast", fillGraph = TRUE, axis = "y2") %>%
    dySeries(name = c("lower_30", "pointfc_mean", "upper_30"), label = "30% PI", axis = "y2") %>%
    dySeries(name = c("lower_50", "pointfc_mean", "upper_50"), label = "50% PI", axis = "y2") %>%
    dySeries(name = c("lower_70", "pointfc_mean", "upper_70"), label = "70% PI", axis = "y2") %>%
    dyAxis(name = "y2", valueFormatter = interval_value_formatter, label = "Monthly Oranges bought",
           drawGrid = FALSE, independentTicks = FALSE) %>%
    dyAxis(name = "x", axisLabelFormatter = 'function(d){ var month = d.getMonth().toString().fontsize(2) ;var year = d.getFullYear().toString().fontsize(2); return  year}',
           label = "Year") %>%
    dyCSS(textConnection("
      .dygraph-title {color:#0099CC; font-weight:bold;}
      .dygraph-xlabel {font-size:13px;}
      .dygraph-axis-label-y1 {display:none;}
      .dygraph-axis-label-y2 {font-size:13px;}
      .dygraph-legend {background-color:transparent !important;}")) %>%
    dyCallbacks(underlayCallback=JS("function(ctx, area, dygraph) {
                        ctx.strokeStyle = 'black';
                         ctx.strokeRect(area.x, area.y, area.w, area.h);}")) %>%
    dyOptions(axisLineColor = "navy", gridLineColor = "grey", 
              digitsAfterDecimal = 0, strokeWidth = 2, 
              includeZero = TRUE, drawAxesAtZero = TRUE,
              colors = c("black", "#FF3399", "#00CCFF", "#33FF00")) %>%
    dyRangeSelector(dateWindow = NULL, height = 20,
                    fillColor = "#99CCFF", strokeColor = "#99CCFF", keepMouseZoom = TRUE,
                    retainDateWindow = FALSE) %>%
    dyLegend(labelsDiv = "legend2", labelsSeparateLines = TRUE, show = "always") %>%
    dyHighlight(highlightCircleSize = 5, 
                highlightSeriesBackgroundAlpha = 1,
                hideOnMouseOut = FALSE)  
})

options(warn = -1) 
 
output$legendplot <- renderPlotly({
  grpnames <- c("One", "Two", "Three")
  xval <- as.factor(c(100, 101, 102, 103, 104))
  frame <- merge(grpnames, xval, all = T)

  yval <- rep(5, 15)
  df <- tbl_df(cbind(frame, yval))
  colnames(df) <- c("GroupName", "X", "Y")

  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  

  p <- df %>% group_by(X) %>% arrange(GroupName) %>% mutate(Y = cumsum(Y)) %>%
    plot_ly(type = 'scatter', x = ~X, y = ~Y, color = ~GroupName, colors = c("#00CCFF", "#33FF00", "black"),
            mode = 'lines', alpha = 0.2, fill = 'tonexty', hoverinfo = 'none') %>%
    layout(xaxis = ax, yaxis = ax, showlegend = FALSE,
           #xaxis_title = "x Axis Title",
           font = list(family = "monospace"),
           margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
           #autosize = FALSE,  paper_bgcolor="LightSteelBlue",
           width = 180, height = 150) %>%
    config(displayModeBar = FALSE) %>%
    add_annotations(
      x = c(102, 102, 102),
      y = c(3, 8, 13),
      xref = "x",
      yref = "y",
      text = c("<b>30% P.I</b>", "<b>50% P.I</b>", "<b>70% P.I</b>"),
      showarrow = FALSE
    )
  suppressWarnings(p)
})

tab3data <- reactive({
  dat <- orange[orange$Name == req(input$Names3), ]
  dat <- dat[!(names(dat) %in% c("Region", "long", "lat", "Member"))]
  fitted.mode <- fitted(fitdf())
  data <- data.frame(Y = as.matrix(fitted.mode), date = floor(time(fitted.mode)))
  newdata <- cbind(dat, floor(data$Y))
  newdata <- newdata[, -1]
  colnames(newdata) <- c("Name", "Month", "Year", "True", "Predicted")
  newdata
})

forecastingdat <- reactive({
  fc <- forecastingy()
  fc <- data.frame(fc)
  data <- setNames(cbind(rownames(fc), fc, row.names = NULL),
                   c("MonthYear", "Point Forecast", "Lo 30", "Hi 30", "Lo 50", "Hi 50", "Lo 70", "Hi 70"))
  for(i in names(data)[2:8]){
    data[[i]] <- round(as.numeric(data[[i]]))
  }
  data
})  

output$YEAR3 <- renderUI({
  data <- forecastingdat()
  selectInput(inputId = "Year3", label = "Predicted Month-Year",
              choices = unique(data$MonthYear))
})

output$TYPE <- renderUI({
  mydata <- forecastingdat()
  selectInput(inputId = "Type", label = "Type of forecast",
              choices = names(mydata)[2:8])
})

predicted <- reactive({
  mydata <- forecastingdat()
  x <- mydata[, names(mydata) == input$Type][mydata$MonthYear == input$Year3]
})

mgintake <- reactive({
  y <- predicted()
  m <- orange$Member[orange$Name == input$Names3][1]
  z <- (y*53.2)/m
  z
})

observe({
  mgintake <- mgintake()
  member <- orange$Member[orange$Name == input$Names3][1]
  y <- predicted()
  updateKnobInput(session, inputId = "knob1", 
                  label = paste("Family", input$Names3, "has", member, 
                               "members and their forecast number of oranges is", y), 
                  value = mgintake) 
})

output$ICON <- renderText({
  paste("<b><span style='color:#FFFFFF;'>", "Click on the Calculator icon in", "<br>", "the box header on", "<br>", "the right for more information", "</span></b>")
})

observeEvent(input$calculation,{
  createAlert(session, anchorId = "percentage", alertId = "two", title = "<center>IMPORTANT</center>",
              content = HTML("<div class=alert alert-info, role=alert style='color:black;font-family:monospace;'>
                        <p>Choose a future month-year and a type<br>
                           of forecast to output the predicted monthly<br>
                           vitamin C intake (mg) per family member, <br>
                           corresponding month-year. The predicted<br> 
                           Vitamin C intake is calculated as follows:</p>
                        $$\\textrm{mg intake} = \\frac{53.2 \\times \\textrm{No of Predicted Oranges}}{\\textrm{No of Family Member}}$$<script>MathJax.Hub.Queue(['Typeset', MathJax.Hub]);</script>"), 
              append = FALSE, style = "info")
  jqui_draggable(selector = '.alert-info')
})

options(warn = -1) 

output$gvis <- renderPlotly({
  dt <- tab3data()
  dt$Date <- as.Date(as.yearmon(paste(dt$Year, " ", dt$Month), format = "%Y %m"))
  fitted.model <- fitted(fitdf())
  plot_ly(dt, type = 'scatter', mode = 'lines') %>%
    add_trace(x = ~Date, y = ~True, name = "True", text = 'oranges', hovertemplate = '%{y:,} %{text}',
              line = list(color = 'rgb(231,107,243)')) %>%
    add_trace(x = ~Date, y = ~Predicted, name = "Predicted", text = 'oranges', hovertemplate = '%{y:,} %{text}',
              line = list(color = 'rgb(0,176,246)')) %>%
    config(displayModeBar = FALSE) %>%
    layout(title = list(text = "<b>TRUE VS PREDICTED DATA</b>"), titlefont = list(size = 25),
           margin = list(l = 50, r = 50, b = 50, t = 40, pad = 4),
           legend = list(x = 0.5, y = 1.0, orientation = 'h'),
           font = list(family = "monospace"),
           hovermode = "x unified",
           xaxis = list(
             type = "date",
             tickformat = "%Y %b",
             title = "YEAR MONTH",
             titlefont = list(color = "blue"),
             linecolor = toRGB("black"),
             showline = TRUE,
             mirror = "ticks",
             linewidth = 2),
           yaxis = list(
             title = "ORANGES",
             zeroline = FALSE,
             titlefont = list(color = "blue"))
    ) -> plott
  suppressWarnings(plott)
})
