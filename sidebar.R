output$sidebar <- renderUI({
  if(USER$login == TRUE){
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    dashboardSidebar(
      div(style="text-align:center !important;", id = "image", img(src = "orange2.png", height = "50px")),
      bsPopover(id = "image", title = "", content = "<b>Please refresh the page if <br> an output is not showing <br>properly</b>", placement = "bottom"),
      br(),
      conditionalPanel(condition = "input.tabs=='3' || input.tabs=='1' && input.sidebarmenu=='tabTwo'",
                       div(style="color:white",
                           uiOutput(outputId = "DATA"))
      ),
      conditionalPanel(condition = "input.tabs=='1' && input.sidebarmenu=='tabOne'",
                     h4("Time Series Analysis")
      ),
      conditionalPanel(condition = "input.tabs=='1' && input.sidebarmenu=='tabThree'",
                       h4("Time Series Analysis")
      ),
      conditionalPanel(condition = "input.tabs=='1'",
                       br(),
                       sidebarMenu(sidebarMenuOutput(outputId = "menu")),
                       br()
      ),
      conditionalPanel(condition="input.tabs=='2'",
                       div(style="color:white", uiOutput(outputId = "DATA2")),
                       actionButton(inputId = "press", label = "Press"),
                       actionButton(inputId = "subset", label = "Subset"),
                       div(style="color:white;",
                           selectInput(inputId = "horizon", 
                                          label = HTML("Horizon", as.character(div(style="display:inline-block; margin-left:-5px;", actionLink(inputId = 'RESET', label = '(reset)', icon("sync-alt"))))),
                                          choices = c("Forecast in months" = "", 6, 12, 24)
                       )),
                       bsPopover(id = "horizon", title = "", content = "Click on the refresh icon to reset horizon values", placement = "right"),
                       radioGroupButtons(
                         inputId = "switchAlert", label = NULL, justified = TRUE,
                         choiceNames = c(
                           paste(icon("hand-point-right"), "Guide"),
                           paste(icon("laptop"), "Info")
                         ),
                         selected = character(0), 
                         choiceValues = c("alerttwo", "alertone"),
                         status = "primary"
                       ),
                       bsAlert(anchorId = "instruction"),
                       bsAlert(anchorId = "information"),
                       bsAlert(anchorId = "noyearinput"),
                       br(),
                       tags$style(type='text/css', '#toggleAdvanced {font-weight:bold;color:white;'),
                       tags$style(type='text/css', '#RESET {font-weight:bold;color:white;'),
                       shinyjs::hidden(
                         div(id = "advanced",
                             div(style = "color:white;text-align:center;", uiOutput(outputId = "YEAR")),
                             div(style = "margin-bottom:-25px;margin-left:10px;margin-top:-30px;color:white;text-align:center;",
                                 checkboxInput(inputId = "bar", label = "All/None")), 
                             div(style = "margin-left:15px", checkboxInput(inputId = "lambda", label = tags$span("Transform data", style = "font-weight:bold;color:white;text-align:center;"))),
                          )
                       ),
                       div(style="margin-top:8x;text-align:center", actionLink(inputId = "toggleAdvanced", label = "Advanced options"))
      ),
      conditionalPanel(condition="input.tabs=='3'",
                       br(),
                       withMathJax(),
                       div(style = "margin-top:-15px; margin-bottom:-15px;",
                           plotOutput(outputId = "my_tmap", width = "100%", height = "300px")),
                       br(),
                       hr(),
                       bsAlert(anchorId = "percentage"),
                       div(style="margin-left:15px;", htmlOutput(outputId = "ICON")),
                       uiOutput(outputId = "YEAR3"),
                       uiOutput(outputId = "TYPE"),
      ),
      br(),
      div(menuItemOutput(outputId = "times"), style = "color:white;")
    )
  }else{
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  }
})
