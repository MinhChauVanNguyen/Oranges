tabPanel("Regression Model", value = 3,
         br(),
          fluidRow(
           column(2, style = "margin-left:70px;margin-top:30px; margin-right:-30px;",
            fluidRow(div(id = "dylegend_2", box(width = NULL, textOutput(outputId = "legend2")))),
            div(style = "margin-left:12px;font-size:16px", fluidRow(p("P.I* Color Classes"))),
            div(style = "margin-bottom:-250px;", fluidRow(plotlyOutput(outputId = "legendplot"))),
            div(style = "margin-left:12px;font-size:13px", fluidRow(p("*prediction interval")))
          ),
           column(9, style = "margin-left:20px;", dygraphOutput(outputId = "PLOT3", width = "90%"))
         ),
         br(),
         br(),
         fluidRow(
           column(4, style="margin-right:-65px; margin-left:20px; margin-top:-20px;",
                  br(),
                  tags$script(HTML("$('.box').eq(0).css('border', '2px solid #8AD2FF80');")),
                  tags$script(HTML(
                    "$(\"#knob1\").knob({ 'draw' : function () { $(this.i).val(this.cv + 'mg') } });"
                  )),
                  div(id = "mydiv", box(width = NULL, 
                            title = HTML("Average Vitamin C intake <br> per family member", 
                                         as.character(actionLink(inputId = "calculation", label = "", icon = icon("calculator")))), solidHeader = TRUE,
                            collapsible = TRUE,  status = "primary", align = "center",
                            knobInput(inputId = "knob1",
                                      label = "",
                                      value = 50, min = 0, max = 1000, 
                                      readOnly = TRUE, lineCap = "round",
                                      fgColor = "#FF8C00", inputColor = "#FF8C00")))),
           column(6, offset = 1, style = "margin-bottom:50px; padding-left:40px;",
                  fluidRow(plotlyOutput(outputId = "gvis", height = "300px")),
                  fluidRow(div(style = "margin-left:30px;margin-top:25px;", 
                   p("The above line chart shows the accuracy of 
                      the fitted Regression line used for modelling the data.")))
           )
         ),
         
         hr()
)