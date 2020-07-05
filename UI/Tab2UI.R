options(spinner.color = "#FFA500", spinner.type = 7)

tabPanel("Arima Model", value = 2,
         br(), 
         h3(textOutput(outputId = "TEXT", container = span)),
         h3(textOutput(outputId = "TEXT2", container = span)),
         tags$style(type='text/css', '#ARIMA {background-color:gray87; color:#FF8C00; border:radius:50px;}'),
         fluidRow(
           column(6, offset = 3, verbatimTextOutput(outputId = "ARIMA") %>% withSpinner())
           #column(5, verbatimTextOutput(outputId = "MSE") %>% withSpinner())
         ),
         tags$style(type='text/css', '#ARIMA2 {background-color:#DCDCDC; color:#FF8C00;}'),
         tags$style(type='text/css', '#MSE {background-color:white; color:#FF8C00; max-height:375px;}'),
         fluidRow(
           column(6, verbatimTextOutput(outputId = "ARIMA2") %>% withSpinner()),
         ),
         br(),
         shinyjs::hidden(div(id = "hiding", 
          tabsetPanel(id = "tabsets", 
            tabPanel("Time Series Plot", value = "box1",
              br(),
              fluidRow(
                column(9, style = "margin-bottom:50px;", 
                       dygraphOutput(outputId = "PLOT", width = "95%") %>% withSpinner()),
                column(2, style = "margin-left:-35px; margin-top:27px; font-size:12px;", 
                    fluidRow(div(id = "dylegend_1", box(width = NULL, 
                       textOutput(outputId = "legend1")))),
                    fluidRow(helpText(HTML("<b>Note</b>: The shaded regions on the graph represent the 30% (narrowest region),
                                     50% and 70% (widest region) prediction intervals for the 3-year forecast.
                                     You can hover over the graph to view the monthly prediction values."), style = "font-size:13px")))
              )
            ),
            tabPanel("Residual Plots", value = "box2", 
               plotOutput(outputId = "RES1", width = "70%", height = "270px") %>% withSpinner(), 
               plotOutput(outputId = "RES2", width = "70%", height = "270px") %>% withSpinner()
            )
         ))),
         hr()
)