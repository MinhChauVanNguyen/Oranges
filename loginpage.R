## login username and password
credentials = data.frame(
  username_id = "Minh",
  passod = password_store("1"),
  stringsAsFactors = FALSE
)

loginpage <- div(id = "loginpage",
    br(),
    fluidRow(
      column(2, div(style = "float:right; display:inline-block;margin-top:12px;margin-right:-140px;", 
              img(height = 50, src = "Orange.png"))),
      column(10, div(style = "margin-left:120px;", h2("TIME SERIES ANALYSIS OF ORANGE DATA", align = "left")))
    ),
    fluidRow(
      column(4, style = "margin-left:200px;margin-top:-20px;",
        fluidRow(div(
            br(),
            br(),
            h3("Abstract"),
            p("The Orange Dashboard can be used to predict the number of oranges bought for selected Family
               using Time Series Analysis. Please note the data used was generated randomly 
               for the purpose of demonstration of the Analysis. For login details:", style ="color:black;"),
            tags$ul(
              tags$li(tags$b("Username"), ": Minh"), 
              tags$li(tags$b("Password"), ": 1")
            )
         )),
        br(),
        tags$script(HTML("$('.box').eq(0).css('border', '1px solid #3DA0D1');")),
        fluidRow(box(width = NULL, div(style="color:black;margin-top:-10px;", htmlOutput(outputId = "disclaimer")))),
        fluidRow(a(icon("github-alt", "fa-2x"), "Link to Github Repo", href = "https://github.com/MinhChauVanNguyen/Orange", style = "color:orange;"))
      ),
    column(4, offset = 1, style = "padding:0px; margin-top:-30px;",
      div(style = "width:300px; max-width:100%; margin-top:100px; padding:0px; border-radius:25px;",
        wellPanel(style = "background:rgb(255,165,0);",
            tags$h2("LOG IN", class = "text-center", style = "padding-top:0; color:white; font-weight:600;"),
            div(style = "color:white;", 
                textInput(inputId = "userName", placeholder = "Username", label = tagList(icon("user"), "Username")),
                passwordInput(inputId = "passwd", placeholder = "Password", label = tagList(icon("unlock-alt"), "Password"))
            ),
            br(),
            div(style = "text-align:center; margin-top:-20px;",
            actionButton(inputId = "login", "SIGN IN"),
            shinyjs::hidden(div(id = "nomatch", tags$p("Invalid login or password!")))
            )
        ))
    )
  )
)
