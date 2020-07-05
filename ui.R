MAIN_PATH <- paste0(getwd(), "/")

ui <- dashboardPage(
  dashboardHeader(
    title = div(style = "float:left; margin-top:-8px; font-size:15px;color:black;", 
                  "Oranges Dashboard"), 
    tags$li(uiOutput(outputId = "logoutbtn"),
            class = "dropdown"),
    titleWidth = 200
  ),
  dashboardSidebar(
    width = 200,
    useShinyjs(),
    tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
    uiOutput(outputId = "sidebar")
  ),
  body = dashboardBody(
    tags$style(HTML(
      "div.MathJax_Display{
   text-align:center !important;
   font-family:monospace !important;
      }
      .MathJax body{font-family:'monospace'}"
      )),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    uiOutput("body"),
    br(),
    div(style="margin-top:-30px", htmlOutput("footer"))
  )
)