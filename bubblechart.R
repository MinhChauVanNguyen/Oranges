orange <- read.csv("oranges.csv", header = TRUE, stringsAsFactors = FALSE)

bubbledata <- orange %>% filter(Year == 2018) %>% group_by(Region, Name) %>%
  summarise(Oranges = sum(Total))
bubbledata <- head(bubbledata, 8)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    highchartOutput(outputId = "bubbly")
  )
)

server <- function(input, output, session) {
  
  output$bubbly <- renderHighchart({
    hc <- hchart(bubbledata, "packedbubble", hcaes(name = Name, value = Oranges, group = Region))
    hc %>% 
      hc_tooltip(
        useHTML = TRUE,
        pointFormat = "<b>{point.name}:</b> {point.value} oranges"
      ) %>% 
      hc_plotOptions(
        packedbubble = list(
          maxSize = "150%",
          minSize = "50%",
          zMin = 0,
          dataLabels = list(
            enabled = TRUE,
            format = "{point.name}",
            style = list(
              color = "black",
              fontFamily = "monospace",
              textOutline = "none",
              fontWeight = "normal")
          )
        )
      ) %>%
      hc_legend(itemStyle = list(fontFamily = "monospace"))
    
  })
}

shinyApp(ui, server)

