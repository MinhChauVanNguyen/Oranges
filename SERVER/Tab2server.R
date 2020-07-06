output$DATA2 <- renderUI({
  selectizeInput(inputId = "Name2",
              label = "Choose a Family Name",
              choices = sort(unique(factor(orange$Name))),
              options = list(
                placeholder = 'Name',
                onInitialize = I('function() { this.setValue(""); }'))
  )
})

dataset <- reactive({
  inputdata <- orange[orange$Name == req(input$Name2), ]
})

observeEvent(input$RESET, {
  reset("horizon")
})

shinyjs::onclick("toggleAdvanced", shinyjs::toggle(id = "advanced", anim = TRUE)) 

observeEvent(input$toggleAdvanced,{
  if(input$toggleAdvanced %% 2 == 1) {
    txt <- "Hide options"
    icon <- icon("arrow-up")
  } else {
    txt <- "Advanced options"
    icon <- icon("arrow-down")
  }
  updateActionButton(session, "toggleAdvanced", label = txt, icon = icon)
})

output$YEAR <- renderUI({
  subdata <- dataset()
  subdata$Year <- factor(subdata$Year)
  checkboxGroupInput(inputId = "year2",
      label = tags$span("Choose a few years", style = "font-weight:bold; color:white"),
      choices = unique(subdata$Year))
})

observe({
  if(is.null(input$Name2)){return()}
  data <- orange[orange$Name == req(input$Name2), ]
  data$Year <- factor(data$Year)
  updateCheckboxGroupInput(
    session, inputId = 'year2', choices = unique(data$Year),
    selected = if (input$bar) unique(data$Year)
  )
})

observeEvent(input$switchAlert,{
  if(input$switchAlert == "alerttwo"){
    createAlert(session, anchorId = "instruction", alertId = "Alert1", title = HTML("<center><font size='40px'>INSTRUCTIONS</font></center>"),
                content = HTML("<div class=alert alert-success role=alert style='color:black; text-align:left;'>
              <p><span style='color:yellow;'>Choose a Family Name and use the <b>Press</b> button to obtain a dygraph of the<br> 
              fitted ARIMA model.</span> Users can also select a preferred forecast time period<br>
              (horizon). If the forecast horizon is not selected then the standard settings<br>
              will apply. By default, the length of the data history needs to be at least<br>
              two times the length of the forecast horizon in order for a forecast to be<br>
              generated. For example, we need to have at least <span style='color:yellow;'><b>6 x 2 = 12 months</b></span> of historical<br>
              data to forecast 6 months ahead. This is to ensure that there is sufficient data<br>
              for a reasonable forecast outcome.</p>
           <hr>
           For more information on the default settings, please refer to the <span style='color:yellow;'><b>Info</b></span> button. <br>
           For advanced options, choose a family and subset the data using the preferred <br>
           years then click the <span style='color:yellow;'><b>Subset</b></span> button.</div>"), append = FALSE, style = 'success')
    jqui_draggable(selector = '.alert-success')
  }else{
    createAlert(session, anchorId = "information", alertId = "Alert2", title = HTML("<center>Please Read</center>"),
                content = HTML("<div class=alert alert-info role=alert style='color:black;'>
      <p>The requirement for forecasting 3 years ahead is that the selected Family has to <br>
         have at least three years worth of data, i.e. nrow(Family) = 36 observations/months.<br>
         Ideally, the family should have more than 36 observations in order to improve the <br>
         model's accuracy. Most tracks will have data for 6 years or longer. It is recommended <br>
         that only historical data from the most recents years are used.</p>
      <hr>
    <ol>
  <li>Set the threshold number of rows in a selected Family to nrow = 72 observations. </li>
  <li>If the Family chosen has data less than 72 months then use the full data to fit <br>
  the model. Otherwise, only use the last 72 observations for the selected Family. </li>
  <li>Use the fitted model to forecast the number of oranges bought for h = 24 months <br>
  if all data for the chosen Family has been used. Otherwise, use h = 36 months. </li>
  </ol></div>"), append = FALSE, style = 'info')
    jqui_draggable(selector = '.alert-info')
  }
})

observeEvent(input$subset,{
  if(is.null(input$year2)){
    createAlert(session, anchorId = "noyearinput", alertId = "extra2", title = HTML("<center>OOPS!</center>"),
                content = HTML("<div class=alert alert-danger role=alert style='color:black;text-align:left;'>
              Please choose at least three years <br>
              in <b>Advanced options</b> before pressing<br>
              the <b>Subset</b> button.</div>"), 
                append = FALSE, style = 'danger')
    jqui_draggable(selector = '.alert-danger')
  }else{
    closeAlert(session, alertId = "extra2")
  }
})

button1 <- reactiveValues(but1 = FALSE)
button2 <- reactiveValues(but2 = FALSE)

observeEvent(input$press,{
  button1$but1 <- TRUE
  button2$but2 <- FALSE
})

observeEvent(input$subset,{
  button1$but1 <- FALSE
  button2$but2 <- TRUE
})

observeEvent(input$press, {
  output$TEXT <- renderText({
    if(button1$but1)
      paste("ARIMA model for Family", isolate(input$Name2))
    else 
      return()
  })
})

observeEvent(input$subset, {
  output$TEXT2 <- renderText({
    if(button2$but2)
      paste("ARIMA (advanced) model for Family", isolate(input$Name2))
    else
      return()
  })
})

observeEvent(input$subset, {
  showTab(inputId = "tabsets", target = "box2")
})

observeEvent(input$press, {
  hideTab(inputId = "tabsets", target = "box2")
})

observeEvent(c(input$press,input$subset),{
  if(is.null(req(input$Name2)) && is.null(input$year) || input$Name2 == ""){
    shinyjs::toggle("hiding")
  }else{
    shinyjs::show("hiding")
  }
})

truedata <- reactive({
  inputdata <- dataset()
  if(input$horizon == ""){
    if(nrow(inputdata) > 72 || nrow(inputdata) == 72){
      taildt <- tail(inputdata, n = 72)
      true <- ts(taildt$Total, frequency = 12,
                 start = c(min(taildt$Year), min(taildt[taildt$Year == min(taildt$Year), "Month"])))
    } else {
      true <- ts(inputdata$Total, frequency = 12,
                 start = c(min(inputdata$Year), min(inputdata[inputdata$Year == min(inputdata$Year), "Month"])))
    }
  } else {
    horizon <- input$horizon
    h <- as.integer(horizon)
    if(h < nrow(inputdata)){
      taildata <- tail(inputdata, n = h*2)
      true <- ts(taildata$Total, frequency = 12,
                 start = c(min(taildata$Year), min(taildata[taildata$Year == min(taildata$Year), "Month"])))
    } else {
      true <- ts(inputdata$Total, frequency = 12,
                 start = c(min(inputdata$Year), min(inputdata[inputdata$Year == min(inputdata$Year), "Month"])))
    }
  }
  true
})

fit1 <- eventReactive(input$press,{
  inputdata <- dataset()
  if(input$horizon == ""){
    if(isTRUE(nrow(inputdata) > 72) || isTRUE(nrow(inputdata) == 72)){
      taildata <- tail(inputdata, n = 72)
      subsetdata <- ts(taildata$Total, frequency = 12,
                       start = c(min(taildata$Year), min(taildata[taildata$Year == min(taildata$Year), "Month"])))
      traindata <- window(subsetdata, 
                      start = c(start(time(subsetdata))[1], match(month.abb[cycle(subsetdata)][1], month.abb)), 
                      end = c(floor(time(subsetdata)[floor(length(subsetdata)*0.8)]),
                              match(month.abb[cycle(subsetdata)][floor(length(subsetdata)*0.8)], month.abb)))
      arimadata <- auto.arima(traindata, stepwise = FALSE, approximation = FALSE)
    }else{
      fulldata <- ts(inputdata$Total, frequency = 12,
                     start = c(min(inputdata$Year), min(inputdata[inputdata$Year == min(inputdata$Year), "Month"])))
      arimadata <- auto.arima(fulldata, stepwise = FALSE, approximation = FALSE)
    } 
  }else{
    horizon <- input$horizon
    h <- as.integer(horizon)
    if(h < nrow(inputdata)){
      taildata <- tail(inputdata, n = h*2)
      subsethorizon <- ts(taildata$Total, frequency = 12,
                          start = c(min(taildata$Year), min(taildata[taildata$Year == min(taildata$Year), "Month"])))
      trainhorizon <- window(subsethorizon, 
                          start = c(start(time(subsethorizon))[1], match(month.abb[cycle(subsethorizon)][1], month.abb)), 
                          end = c(floor(time(subsethorizon)[floor(length(subsethorizon)*0.8)]),
                                  match(month.abb[cycle(subsethorizon)][floor(length(subsethorizon)*0.8)], month.abb)))
      arimadata <- auto.arima(trainhorizon, stepwise = FALSE, approximation = FALSE)
    } else {
      fullhorizon <- ts(inputdata$Total, frequency = 12,
                        start = c(min(inputdata$Year), min(inputdata[inputdata$Year == min(inputdata$Year), "Month"])))
      arimadata <- auto.arima(fullhorizon, stepwise = FALSE, approximation = FALSE)
    }
  }
  arimadata
})

output$ARIMA <- renderPrint({
  if(button1$but1){
    fit1 <- suppressWarnings(fit1())
    fit1
  }else{
    return(req(NULL))
  }
})

tsdata2 <- reactive({
  x <- dataset()
  subsetdt <- subset(x, Year %in% input$year2)
  tsdata <- ts(subsetdt$Total, frequency = 12, 
               start = c(min(subsetdt$Year), min(subsetdt[subsetdt$Year == min(subsetdt$Year), "Month"])), 
               end = c(max(subsetdt$Year), max(subsetdt[subsetdt$Year == max(subsetdt$Year), "Month"])))
})

fit2 <- eventReactive(input$subset,{
  tsdata <- req(tsdata2())
  train <- window(tsdata, 
                  start = c(start(time(tsdata))[1], match(month.abb[cycle(tsdata)][1], month.abb)), 
                  end = c(floor(time(tsdata)[floor(length(tsdata)*0.8)]),
                          match(month.abb[cycle(tsdata)][floor(length(tsdata)*0.8)], month.abb)))
  horizon <- input$horizon
  h <- as.integer(horizon)
  if(input$lambda){
    if(input$horizon == ""){
      fit2 <- auto.arima(train, stepwise = FALSE, approximation = FALSE, lambda = BoxCox.lambda(train))
    }else{
      if(h < length(tsdata)){
        taildata <- tail(tsdata, n = h*2)
        trainhorizon <- window(taildata, 
                               start = c(start(time(taildata))[1], match(month.abb[cycle(taildata)][1], month.abb)), 
                               end = c(floor(time(taildata)[floor(length(taildata)*0.8)]),
                                       match(month.abb[cycle(taildata)][floor(length(taildata)*0.8)], month.abb)))
        fit2 <- auto.arima(trainhorizon, stepwise = FALSE, approximation = FALSE, lambda = BoxCox.lambda(trainhorizon))
      }else{
        fit2 <- auto.arima(tsdata, stepwise = FALSE, approximation = FALSE, lambda = BoxCox.lambda(tsdata))
      }
    }
  }else{
    if(input$horizon == ""){
      fit2 <- auto.arima(train, stepwise = FALSE, approximation = FALSE, lambda = NULL)
    }else{
      if(h < length(tsdata)){
        taildata <- tail(tsdata, n = h*2)
        trainhorizon <- window(taildata, 
                               start = c(start(time(taildata))[1], match(month.abb[cycle(taildata)][1], month.abb)), 
                               end = c(floor(time(taildata)[floor(length(taildata)*0.8)]),
                                       match(month.abb[cycle(taildata)][floor(length(taildata)*0.8)], month.abb)))
        fit2 <- auto.arima(trainhorizon, stepwise = FALSE, approximation = FALSE, lambda = NULL)
      }else{
        fit2 <- auto.arima(tsdata, stepwise = FALSE, approximation = FALSE, lambda = NULL)
      }
    }
  }
  fit2
})

output$ARIMA2 <- renderPrint({
  if(button2$but2){
    fit2 <- suppressWarnings(fit2())
    fit2
  }else{
    return(req(NULL))
  }
})

accurate <- eventReactive(input$subset,{
  model <- fit2()
  tsdata <- tsdata2()
  testing <- window(tsdata, 
      start = c(floor(time(tsdata)[floor(length(tsdata)*0.8)]),
      match(month.abb[cycle(tsdata)][floor(length(tsdata)*0.8)], month.abb) + 1))
  fc <- forecast(model, h = 36)
  xy <- t(accuracy(fc, testing))
  xy
})

accurate1 <- eventReactive(input$press,{
  model <- fit1()
  tsdata <- truedata()
  testing <- window(tsdata, 
                    start = c(floor(time(tsdata)[floor(length(tsdata)*0.8)]),
                          match(month.abb[cycle(tsdata)][floor(length(tsdata)*0.8)], month.abb)+1))
  if(input$horizon == 6){
    fc <- forecast(model, h = 6, level = c(30,50,70))
  }else if(input$horizon == 12){
    fc <- forecast(model, h = 12, level = c(30,50,70))
  }else if(input$horizon == 24){
    fc <- forecast(model, h = 24, level = c(30,50,70))
  }else{
    fc <- forecast(model, h = 36, level = c(30,50,70))
  }
  xy <- t(accuracy(fc, testing))
  xy
})

output$MSE <- renderPrint({
  data <- dataset()
  if(button1$but1){
    if(isTRUE(nrow(data) < 72) && isTRUE(input$horizon == "")){
      paste("There is not enough data.")
    }else{
      accurate <- accurate1()
      accurate
    }
  }else{
    return(req(NULL))
  }
})

output$MSE2 <- renderPrint({
  if(button2$but2){
    accurate <- accurate()
    accurate
  }else{
    return(req(NULL))
  }
})

graph1 <- eventReactive(input$press,{
  truedata <- truedata()
  model <- req(fit1())
  if(model$series == "traindata"){
    ARIMA.mean <- model %>% forecast(h = 36, level = c(30,50,70))
  }else if(model$series == "trainhorizon"){
    ARIMA.mean <- model %>% forecast(h = input$horizon, level = c(30,50,70))
  }else{
    ARIMA.mean <- model %>% forecast(h = 24, level = c(30,50,70))
  }
  graph1 <- cbind(actuals = truedata, pointfc_mean = ARIMA.mean$mean,
                  lower_70 = ARIMA.mean$lower[,"70%"], upper_70 = ARIMA.mean$upper[,"70%"],
                  lower_50 = ARIMA.mean$lower[,"50%"], upper_50 = ARIMA.mean$upper[,"50%"],
                  lower_30 = ARIMA.mean$lower[,"30%"], upper_30 = ARIMA.mean$upper[,"30%"])
})

graph2 <- eventReactive(input$subset,{
  y <- suppressWarnings(fit2())
  if(y$series == "tsdata"){
    ARIMA.mean <- y %>% forecast(h = length(tsdata2()), level = c(30,50,70))
  }else{
    ARIMA.mean <- y %>% forecast(h = 36, level = c(30,50,70))
  }
  graph2 <- cbind(actuals = tsdata2(), pointfc_mean = ARIMA.mean$mean,
                  lower_70 = ARIMA.mean$lower[,"70%"], upper_70 = ARIMA.mean$upper[,"70%"],
                  lower_50 = ARIMA.mean$lower[,"50%"], upper_50 = ARIMA.mean$upper[,"50%"],
                  lower_30 = ARIMA.mean$lower[,"30%"], upper_30 = ARIMA.mean$upper[,"30%"])
})

name1 <- eventReactive(input$press,{
  fit <- req(fit1())
  if(fit$series == "subsetdata"){
    ARIMA.mean <- fit %>% forecast(h = 36, level = c(30,50,70))
  }else{
    ARIMA.mean <- fit %>% forecast(h = 24, level = c(30,50,70))
  }
  ARIMA.mean
})

name2 <- eventReactive(input$subset,{
  fit <- req(fit2())
  if(fit$series == "tsdata"){
    ARIMA.mean <- fit %>% forecast(h = length(tsdata2()), level = c(30,50,70))
  }else{
    ARIMA.mean <- fit %>% forecast(h = 36, level = c(30,50,70))
  }
  ARIMA.mean
})

output$PLOT <- renderDygraph({
  if(button1$but1){
    dygrapph(graph = graph1(), title = name1()$method, id = "legend1") 
  }else if (button2$but2){
    dygrapph(graph = graph2(), title = name2()$method, id = "legend1")
  } else {
    return(req(NULL))
  }
})

output$RES1 <- renderPlot({
  if(button2$but2){
    cbind(Fitted = fitted(fit2()), Residuals = residuals(fit2())) %>%
      as.data.frame() %>%
      ggplot(aes(x = Fitted, y = Residuals)) + geom_point() + my_theme() +
      ggtitle("Scatter plot of residuals") -> plot1
    
    autoplot(resid(fit2())) +
      geom_line(color = "cyan") + my_theme() +
      ggtitle("Line plot of residuals") + 
      xlab("Year") + ylab("") -> plot2
    
    minh <- grid.arrange(plot1, plot2, ncol = 2)
    print(minh)
  }else{
    return()
  }
})

output$RES2 <- renderPlot({
  if(button2$but2){
    res <- resid(fit2())
    ci4 <- qnorm((1 + 0.95)/2)/sqrt(length(res))
    ggAcf(res) +
      geom_segment(lineend = "butt", color = "cyan") +
      geom_hline(yintercept = 0, color = "cyan") +
      geom_hline(yintercept = c(ci4, -ci4), color = "#FF0099", linetype = "dashed") +
      my_theme() + ggtitle("ACF plot of residuals") -> p11 
    
    update_geom_defaults("line", list(color = "darkblue"))
    update_geom_defaults("bar", list(fill = "cyan"))
    gghistogram(res, add.rug = TRUE, add.normal = TRUE) + 
      ggtitle("Histogram of residuals") + my_theme() + geom_line() -> p12
    
    plots.res <- grid.arrange(p11, p12, ncol = 2)
    print(plots.res)
  }else{
    return()
  }
})

