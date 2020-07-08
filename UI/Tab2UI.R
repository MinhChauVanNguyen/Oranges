options(spinner.color = "#FFA500", spinner.type = 7)

tabPanel("Arima Model", value = 2,
         div(style = "margin-left:30px; margin-top:40px; margin-bottom:50px;", grVizOutput(outputId = "dg", width = "95%", height = "380px")),
         fluidRow(
           column(10, style = "margin-top:-20px;", htmlOutput(outputId = "TEXT", container = span)),
           column(2,shinyjs::hidden(div(id = "flowchartbutton", style="float:right;margin-top:-20px;margin-right:20px;", 
                                        actionButton(inputId = "HideFlow", label = "Show Flow Chart"))))
         ),
         tags$style(type = 'text/css', '#ARIMA {background-color:gray87; color:#FF8C00; border:radius:50px;}'),
         tags$style(type = 'text/css', '#MSE {background-color:gray87; color:#FF8C00; max-height:375px;}'),
         fluidRow(
           column(5, 
             fluidRow(div(style = "margin-left:50px;", h4(textOutput(outputId = "TEXT2")))),
             fluidRow(div(style = "margin-left:50px;", verbatimTextOutput(outputId = "ARIMA") %>% withSpinner()))),
           column(4, offset = 1, 
             fluidRow(div(style = "margin-left:50px;", h4(textOutput(outputId = "TEXT3")))),
             fluidRow(div(style = "margin-left:50px;", verbatimTextOutput(outputId = "MSE") %>% withSpinner())))
         ),
         tags$style(type='text/css', '#ARIMA2 {background-color:gray87; color:#FF8C00;}'),
         tags$style(type='text/css', '#MSE2 {background-color:gray87; color:#FF8C00; max-height:375px;}'),
         fluidRow(
           column(5, style = "margin-left:50px;", fluidRow(verbatimTextOutput(outputId = "ARIMA2") %>% withSpinner())),
           column(4, offset = 1, style = "margin-left:60px;", fluidRow(verbatimTextOutput(outputId = "MSE2") %>% withSpinner()))
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
            ),
            tabPanel("Data Process", value = "box3",
                     br(),
                fluidRow(
                  column(6, offset = 1, 
                      fluidRow(
                        div(style = "display:inline-block;vertical-align:top;", starBlock(grade = 3, color = "teal")),
                        div(style = "display:inline-block;vertical-align:top;", starBlock(grade = 3, color = "teal")),
                        div(style = "display:inline-block;vertical-align:top;", starBlock(grade = 3, color = "teal")),
                        div(style = "display:inline-block;vertical-align:top;", starBlock(grade = 3, color = "teal"))),
                      fluidRow(div(style = "font-size:16px;",
                      p("Data is split into training and test set pre-fitting the model.
                        Training data is then used to fit the ARIMA model and test data
                        is used to test the accuracy or how well the model fits the data.")))
                  )),
                br()
            )
         ))),
         hr()
)