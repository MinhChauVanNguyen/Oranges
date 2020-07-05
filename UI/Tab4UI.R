tabPanel(value = 4, id = "disabled",
         title = HTML("Signed in as", 
                      gsub('([[:upper:]])', ' \\1', as.character(input$userName)))
)