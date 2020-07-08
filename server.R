MAIN_PATH <- paste0(getwd(), "/")

source("global.R", local = TRUE)

source( "ui.R", local = TRUE)

source("loginpage.R", local = TRUE)

server <- function(input, output, session){
  login = FALSE
  USER <- reactiveValues(login = login)

  observe({
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) {
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(5000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(5000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        }
      }
    }
  })
  
  output$disclaimer <- renderText({
    paste("<font color=\"#FF0000\"><b>", "Dashboard Overview:", "</b></font>",
          "The dashboard is divided into three tabs, each tab's
          functionality reflects the final results, analysis stage 
          and the chosen analysis methods.")
  })
  
  source(file = paste0(MAIN_PATH, "sidebar.R"), local = TRUE)
  
  output$footer <- renderText({
    if(USER$login == TRUE){
      paste("<b><span style='color:#0099CC;'>Under Development</span></b>.","Last Updated on", paste0(Sys.time(), "."))
    }else{
      return()
    }
  })
  
  output$logoutbtn <- renderUI({
    if(USER$login == TRUE){
     req(USER$login)
     tags$a(actionButton(inputId = "logout", label = "Sign Out", icon = icon("power-off")),
           href = "javascript:window.location.reload(true)"
    )
    }else{
      return()
    }
  })

  output$body <- renderUI({
    if(USER$login == TRUE){
      tabsetPanel(
        id = "tabs", selected = 1, 
        source(paste0(MAIN_PATH,"/UI/Tab1UI.R"),local=TRUE)$value,
        source(paste0(MAIN_PATH,"/UI/Tab2UI.R"),local=TRUE)$value,
        source(paste0(MAIN_PATH,"/UI/Tab3UI.R"),local=TRUE)$value,
        source(paste0(MAIN_PATH,"/UI/Tab4UI.R"),local=TRUE)$value
      )
    }else{
      loginpage
    }
  })
  
  output$times <- renderUI({
    invalidateLater(1000)
    menuItem(Sys.time())
  })
  
  
  observe({
    if(req(input$multiple) == "Hide Tabs"){
      hideTab(inputId = "tabs", target = "2")
      hideTab(inputId = "tabs", target = "3")
    }else{
      showTab(inputId = "tabs", target = "2")
      showTab(inputId = "tabs", target = "3")
    }
  })
  
  source(paste0(MAIN_PATH, "/SERVER/Tab1server.R"), local = TRUE)$value
  
  source(paste0(MAIN_PATH, "/SERVER/Tab2server.R"), local = TRUE)$value
  
  source(paste0(MAIN_PATH, "/SERVER/Tab3server.R"), local = TRUE)$value
  
}