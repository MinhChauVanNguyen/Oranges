tabPanel("Results", value = 1,
         br(),
         tabItems(
           tabItem(tabName = "tabOne",
                br(),
                fluidRow(
                  column(5, div(style = "margin-left:30px;",
                         fluidRow(boxPlus(width = NULL, label_status = "info",
                               footer = tags$a(icon("book-reader"), href = "https://otexts.com/fpp2/graphics.html", 
                                               "Time Series Background", style = "color:orange;"),
                               solidHeader = TRUE, title = tagList(icon("anchor"), "Information about the dashboard"),
                               collapsible = TRUE, closable = FALSE, status = "info",
                               "This dashboard consists of a", tags$b("Result"), 
                               "tab which displays the statistical forecasting results, and two other tabs which display 
                                details of the statistical forecasting implementation using the",
                               tags$b("ARIMA"), "and", tags$b("Regression"),
                               "methods. Forecasting is performed monthly to capture seasonal patterns.
                                In the", tags$b("Regression"), "tab also included a feature which helps predicting the average
                                vitamin C from oranges for each family member. On the right is a review of the
                                historical number of oranges grouped by year and family/region as shown on the New Zealand map. You can view the forecasting results 
                                in tabular forms by using the sidebar menu."))),
                         div(style = "display:inline-block;margin-left:35px;", 
                             fluidRow(img(src = "oraange.png", height = "40px"),
                                      img(src = "oraaange.png", height = "40px"),
                                      img(src = "oraange.png", height = "40px"),
                                      img(src = "oraaange.png", height = "40px"),
                                      img(src = "oraange.png", height = "40px"),
                                      img(src = "oraaange.png", height = "40px"),
                                      img(src = "oraange.png", height = "40px"),
                                      img(src = "oraaange.png", height = "40px")
                                      ))
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
                                                                      choices = c("Show Tabs", "Hide Tabs"),  icon = icon("check"),
                                                                      bigger = TRUE, status = "info", animation = "jelly"),
                                                   circle = FALSE, right = TRUE, inline = TRUE, status = "custom", 
                                                   icon = icon("gear"), size = "sm", width = "300px", 
                                                   tooltip = tooltipOptions(title = "Options", placement = "top")), 
                                    style = "display:inline-block; margin-top:25px;"))
                         #   column(5, div(style = "display:inline-block; margin-top:25px;", radioGroupButtons(
                         #       inputId = "switchMap", label = NULL, justified = TRUE,
                         #       choiceNames = c(
                         #         paste(icon("map-marker-alt"), "Map"),
                         #         paste(icon("fighter-jet"), "Advanced")
                         #       ),
                         #       choiceValues = c("mapone", "maptwo"),
                         #       status = "primary"
                         #     )))
                         ),
                         div(style="margin-bottom:-30px;", fluidRow(echarts4rOutput(outputId = "MAP")))
              )
             ),
             br(),
             br(),
             hr(),
             ),
             tabItem(tabName = "tabTwo", 
                     br(),
                     div(style = "margin-left:50px;",
                         fluidRow(column(width = 12, offset = 0, class = PARS$classcol, style="padding-right:0px;",
                                         valueBoxOutput(outputId = "yearly1", width = 4),
                                         valueBoxOutput(outputId = "yearly2", width = 4),
                                         valueBoxOutput(outputId = "yearly3", width = 4)
                         ))
                     ),
                     br(),
                     br(),
                     fluidRow(
                       column(4, offset = 1,
                              fluidRow(div(style = "text-align:left; font-weight: bold;", 
                                           htmlOutput(outputId = "OUT"))),
                              br(),
                              fluidRow(div(id = "table1", 
                                           DT::dataTableOutput(outputId = "TABLE", width = "85%")))
                       ),
                       column(5, style = "margin-top:-18px;",
                          tags$hr(id = "newline2"),
                          div(style = "margin-top:-15px;margin-bottom:-15px;", 
                              fluidRow(
                                column(width = 4, id = "one", uiOutput(outputId = "stats1")),
                                bsPopover(id = "one", placement = "top", title = "", content = "% change from<br>2017 data"),
                                column(width = 4, id = "two", uiOutput(outputId = "stats2")),
                                bsPopover(id = "two", placement = "top", title = "", content = "% change from<br>2018 data"),
                                column(width = 4, id = "three", uiOutput(outputId = "stats3")),
                                bsPopover(id = "three", placement = "top", title = "", content =  "% change from<br>2019 data"),
                              )),
                          tags$hr(id = "newline2"),
                              fluidRow(div(style = "margin-left:35px;", 
                                           echarts4rOutput(outputId = "chart", height = "270px", width = "105%")))
                                    #highchartOutput(outputId = "hchart", height = "250px", width = "100%")))
                      )
                     ),
                     br(),
                     br()
             ),
             tabItem(tabName = "tabThree", 
                     br(),
              fluidRow(
                     column(5, style = "margin-left:30px;",
                      div(icon("info-circle"), style = "color:blue;",
                        "The following tables display the forecast number of oranges for 
                        the selected Family in the next three years together with 30%, 
                        50% and 70% lower (Lo) and upper (Hi) prediction interval bounds.",
                        br(),
                        br(),
                        icon("info-circle"),
                        "Arima model can include", code("approximation = FALSE"), "and",
                        code("stepwise = FALSE"), "arguments to look for every models
                        possible. These arguments however slow down the", code("auto.arima()"), 
                        "function's running time. For this reason, they are removed by default 
                        but can be added using the", tags$b("ARIMA model options"), "on the right."
                        )
                      ),
                      column(6, style = "margin-right:-60px;margin-left:60px;", box(status = "danger",
                                    uiOutput(outputId = "MOREDATA"),
                                    prettyRadioButtons(inputId = "switch", label = "ARIMA model options",
                                                       choices = c("Deactivate arguments", "Activate arguments"),  icon = icon("check"),
                                                       bigger = TRUE, status = "success", animation = "jelly"))),
                     ),
                     br(),
                     fluidRow(
                       column(6, div(id = "table2", style = "font-size:12px;", DT::dataTableOutput(outputId = "TABLE2", width = "90%") %>% withSpinner())),
                       column(6, div(id = "table3", style = "font-size:12px;", DT::dataTableOutput(outputId = "TABLE3", width = "90%")))
                     ),
                     br(),
                     br()
              ),
           tabItem(tabName = "tabFour", 
                   br(),
                   fluidRow(column(6, offset = 3, 
                    div(class = "minh_box", 
                        div(style = "margin-bottom:-5px;text-align:center;font-weight:bold;font-size:20px!important;","To-do-list"),
                        hr(),
                        htmlOutput(outputId = "todolist")),
                    br(),
                    br()
            )))
         ),
         hr()
)
