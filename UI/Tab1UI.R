tabPanel("Results", value = 1,
         br(),
         tabItems(
           tabItem(tabName = "tabOne",
                   div(style = "padding-bottom:240px; margin-left:55px;", 
                       boxPlus(width = 10, enable_label = T, label_text = "New", label_status = "info",
                               footer = tags$a(icon("book-reader"), href = "https://otexts.com/fpp2/graphics.html", "Time Series Background"),
                               solidHeader = T, title = tagList(icon("anchor"), "Information about the dashboard"),
                               collapsible = T, closable = F, status = "info",
                               "This dashboard consists of a", tags$b("Result"), 
                               "tab which displays the statistical forecasting results, and two other tabs which display 
                     details of the statistical forecasting implementation using the",
                               tags$b("ARIMA"), "and", tags$b("Regression"),
                               "methods. Forecasting is performed monthly to capture seasonal patterns. This page shows the 
                     historical number of oranges recorded at monthly intervals. You can view the forecasting results 
                     in tabular forms by using the sidebar menu.")),
                   br(),
             div(style = "margin-left:50px;",
                 fluidRow(column(width = 12, offset = 0, class = PARS$classcol, style="padding-right:0px;",
                                 valueBoxOutput(outputId = "yearly1", width = 4),
                                 valueBoxOutput(outputId = "yearly2", width = 4),
                                 valueBoxOutput(outputId = "yearly3", width = 4)
                 ))
             )
             ),
             tabItem(tabName = "tabTwo", 
                     br(),
                     fluidRow(
                       column(4, offset = 1,
                              fluidRow(div(style = "text-align:left; font-weight: bold;", 
                                           htmlOutput(outputId = "OUT"))),
                              br(),
                              fluidRow(div(id = "table1", 
                                           DT::dataTableOutput(outputId = "TABLE", width = "85%")))
                       ),
                       column(6, offset = 1,  
                              fluidRow(
                                column(4, div(style = "display:inline-block; margin-left:-10px;", uiOutput("YEARS"))),
                                column(2, 
                                       tags$span(
                                         dropdownButton(inputId = "dropdown",
                                              tags$h4(icon("home", lib = "glyphicon"), "Options"),
                                              hr(),
                                              prettyRadioButtons(inputId = "multiple", label = "Choose to show or hide tabs", 
                                                  choices = c("Hide Tabs", "Show Tabs"),  icon = icon("check"),
                                                  bigger = TRUE, status = "info", animation = "jelly"),
                                              circle = FALSE, right = TRUE, inline = TRUE, status = "custom", 
                                              icon = icon("gear"), size = "sm", width = "300px", 
                                              tooltip = tooltipOptions(title = "Options", placement = "top")), 
                                         style = "display:inline-block; margin-top:25px;"))
                              ),
                              div(style = "margin-bottom:-20px;", fluidRow(echarts4rOutput(outputId = "MAP")))
                       )
                     )
                     ),
             tabItem(tabName = "tabThree", 
                     fluidRow(column(10, offset = 2, div(icon("info-circle"), 
                          "The following tables display the forecast number of oranges for", style = "color:blue"), 
                      div("the selected Family in the next three years together with 30%, ", style = "color:blue"),
                      div("50% and 70% lower (Lo) and upper (Hi) prediction interval bounds.", style = "color:blue"))),
                     br(),
                     fluidRow(
                       column(6, div(id = "table2", style = "font-size:12px;", DT::dataTableOutput(outputId = "TABLE2", width = "90%"))),
                       column(6, div(id = "table3", style = "font-size:12px;", DT::dataTableOutput(outputId = "TABLE3", width = "90%")))
                     ),
                     br()
              )
         ),
         hr()
)
