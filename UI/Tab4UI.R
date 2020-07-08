tabPanel(value = 4,
         title = HTML("Signed in as", 
                      gsub('([[:upper:]])', ' \\1', as.character(input$userName))),
         icon = icon("cloud")
)