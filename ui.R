ui <- dashboardPage(
  dashboardHeader(
    title = div(style = "float:left; margin-top:-8px; font-size:15px;color:black;", 
                  "Oranges Dashboard"), 
    #tags$li(uiOutput(outputId = "refreshbtn"), class = "dropdown"),
    tags$li(uiOutput(outputId = "logoutbtn"), class = "dropdown"),
    titleWidth = 200
  ),
  dashboardSidebar(
    width = 200,
    useShinyjs(),    
    #extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }", functions = "refresh"),
    # remove sidebar toggle
    tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
    uiOutput(outputId = "sidebar")
  ),
  body = dashboardBody(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    #tags$head(tags$meta(name = "viewport", content = "width=1600"), 
    uiOutput("body"),
    br(),
    div(style="margin-top:-30px", htmlOutput("footer"))
  )
)