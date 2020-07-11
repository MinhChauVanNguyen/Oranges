# Install packages: FAILED WHEN DEPLOYING TO IO

# Install_And_Load <- function(packages) {
#   #  Finds all the already installed R packages
#   k <- packages[!(packages %in% installed.packages()[,"Package"])]
#   # Check if the packages which we want to install are already installed or not
#   # if package is missing (not installed), install the package
#   # if package is already installed, it does not install it again.
#   if(length(k)){
#     install.packages(k, repos = "https://cran.stat.auckland.ac.nz/")
#   }
#   for(package_name in packages){
#     library(package_name,character.only=TRUE, quietly = TRUE)
#   }
# }
# # 
# Install_And_Load(c("DT",
#                    "tidyverse",
#                    "dplyr",
#                    "formattable",
#                    "data.table",
#                    # shiny packages
#                    "shiny",
#                    "shinydashboard",
#                    "shinydashboardPlus",
#                    "shinyWidgets",
#                    "shinyjs",
#                    "shinyBS",
#                    "shinyjqui",
#                    "shinycustomloader",
#                    "shinycssloaders",
#                    # map packages
#                    "tmap",
#                    "tmaptools",
#                    "spData",
#                    # ts plot packages
#                    "dygraphs",
#                    "echarts4r",
#                    "plotly",
#                    "highcharter",
#                    # residual plot packages
#                    "ggplot2",
#                    "grid",
#                    "gridExtra",
#                    # time series packages
#                    "zoo",
#                    "caret",
#                    "xts",
#                    "forecast",
#                    "seasonal",
#                    "imputeTS",
#                    "urca",
#                    # login
#                    "sodium",
#                    "mathjaxr",
#                    "DiagrammeR"
#                    ))

require(grid)
require(DiagrammeR)
require(mathjaxr)
require(sodium)
require(urca)
require(imputeTS)
require(seasonal)
require(forecast)
require(xts)
require(caret)
require(zoo)
require(gridExtra)
require(ggplot2)
require(highcharter)
require(plotly)
require(dygraphs)
require(echarts4r)
require(tidyverse)
require(dplyr)
require(formattable)
require(data.table)
require(shiny)
require(shinydashboard)
require(shinydashboardPlus)
require(shinyWidgets)
require(shinyjs)
require(shinyBS)
require(shinyjqui)
require(shinycustomloader)
require(shinycssloaders)
# map packages
require(tmap)
require(tmaptools)
require(echarts4r.maps)
require(spData)
require(DT)


# Read in the data
orange <- read.csv("oranges.csv", header = TRUE, stringsAsFactors = FALSE)


# DYGRAPH
interval_value_formatter <- "function(num, opts, seriesName, g, row, col) {
            value = g.getValue(row, col);
            if(value[0] != value[2]) {
            lower = Dygraph.numberValueFormatter(value[0], opts);
            upper = Dygraph.numberValueFormatter(value[2], opts);
            return '[' + lower + ', ' + upper + ']';
            } else {
            return Dygraph.numberValueFormatter(num, opts);
            }}"

dygrapph <- function(graph, title, id){
  dygraph(graph, main = title) %>%
    dySeries(name = "actuals", fillGraph = TRUE) %>%
    dySeries(name = "pointfc_mean", label = "forecast", fillGraph = TRUE) %>%
    dySeries(name = c("lower_30", "pointfc_mean", "upper_30"), label = "30% PI") %>%
    dySeries(name = c("lower_50", "pointfc_mean", "upper_50"), label = "50% PI") %>%
    dySeries(name = c("lower_70", "pointfc_mean", "upper_70"), label = "70% PI") %>%
    dyAxis("y", valueFormatter = interval_value_formatter, label = "Monthly bought oranges") %>%
    dyAxis("x", axisLabelFormatter = 'function(d){ var month = d.getMonth().toString().fontsize(2) ;var year = d.getFullYear().toString().fontsize(2); return  year}',
           label = "Year") %>%
    dyCSS(textConnection("
      .dygraph-title {color:#0099CC; font-weight:bold;}
      .dygraph-axis-label {font-size:12px;}
      .dygraph-ylabel {font-size:14px;}
      .dygraph-xlabel {font-size:14px;}")) %>%
    dyCallbacks(underlayCallback=JS("function(ctx, area, dygraph) {
                        ctx.strokeStyle = 'black';
                        ctx.strokeRect(area.x, area.y, area.w, area.h);}")) %>%
    dyOptions(axisLineColor = "navy", gridLineColor = "grey",
              digitsAfterDecimal = 1, strokeWidth = 2, colors = c("black", "#FF3399", "#00CCFF", "#33FF00")) %>%
    dyRangeSelector(dateWindow = NULL, height = 20,
                    fillColor = "#99CCFF", strokeColor = "#99CCFF", keepMouseZoom = TRUE,
                    retainDateWindow = FALSE) %>%
    dyLegend(labelsDiv = id, labelsSeparateLines = TRUE, show = "always") %>%
    dyHighlight(highlightCircleSize = 5,
                highlightSeriesBackgroundAlpha = 1,
                hideOnMouseOut = FALSE)
    #dyShading(from = "2014-1-1", to = "2020-12-1") 
}


################################## HIGH CHART VALUE BOX ##############################

##############################
##  Highchart function      ##
## Sparklines & value boxes ##
##     Joshua Kunst         ##
##  http://jkunst.com/      ##
##############################

hc_theme_sparkline2 <- function(...) {
  theme <- list(
    chart = list(
      backgroundColor = NULL,
      margins = c(0, 0, 0, 0),
      spacingTop = 0,
      spacingRight = 0,
      spacingBottom = 0,
      spacingLeft = 0,
      plotBorderWidth = 0,
      borderWidth = 0,
      style = list(
        overflow = "visible",
        fontFamily = "monospace"
      ),
      skipClone = TRUE
    ),
    xAxis = list(
      visible = FALSE, 
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    yAxis = list(
      visible = FALSE,
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    plotOptions = list(
      series = list(
        marker = list(enabled = FALSE),
        lineWidth = 1,
        shadow = FALSE,
        fillOpacity = 0.25
      )
    )
  )
  
  theme <- structure(theme, class = "hc_theme")
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }
  theme
}

PARS <- list(
  debug = FALSE,
  classcol = "col-lg-offset-0 col-lg-10 col-md-offset-0 col-md-12 col-sm-offset-0 col-sm-12",
  sparkline_color = "#333333",
  font = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"'
)

valueBoxSpark <- function(value, subtitle, icon = NULL, color = "aqua", 
                          width = 4, href = NULL, spark = NULL, width_spark = "240px", height_spark = "100px",
                          minititle = NULL) {
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon)) 
    shinydashboard:::tagAssert(icon, type = "i")
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      if(!is.null(minititle)) tags$small(minititle),
      h3(value),
      tags$span(hc_size(spark, width = width_spark, height = height_spark)),
      if (!is.null(subtitle)) p(subtitle)
    ),
    if (!is.null(icon)) div(class = "icon-large", icon)
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(class = if (!is.null(width)) 
    paste0("col-sm-", width), boxContent)
}


highValue <- function(datafrme, year){
  d2 <- datafrme %>% 
    filter(Year == year) %>%
    mutate(yearly_ave = sum(Total)/nrow(datafrme[datafrme$Year==year,]))
  d2$Month <- month.abb[d2$Month]
  
  hc <- hchart(d2, "areaspline", hcaes(x=Month, y=Total), name = "Total") %>%
    hc_add_theme(hc_theme_sparkline2()) %>%
    hc_tooltip(valueSuffix = " oranges", borderColor = '#651FFF') %>%
    hc_plotOptions(series = list(color = PARS$sparkline_color,
                                 fillColor = list(linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
                                                  stops = list(list(0.0, "transparent"), list(1.0, PARS$sparkline_color)))))
  lbl <- paste(last(ceiling(d2$yearly_ave)), "oranges")
  
  valueBoxSpark(
    #icon = icon("refresh"),
    value = lbl,
    subtitle = paste("Oranges sale in", d2$Year[1]),
    color = "teal",
    spark = hc,
    minititle = "Yearly Average"
  )
}

# NZ map

# grouped by region and year
data_by_region <- group_by(orange, Name, Region, Year) %>%
  summarise(Oranges = sum(Total))

nz_file <- system.file("New_Zealand.json", package = "echarts4r.maps")
nz_json <- jsonlite::read_json(nz_file)

# get names of polygons
names <- nz_json$features %>%
   map("properties") %>%
   map("name") %>%
   unlist()

island_index <- grep("Chatham", names)

nz_json$features[[island_index]] <- NULL


############################### Theme for residual plots #####################################

my_theme <- function(...) {
  theme(
    text = element_text(family = "mono"),
    axis.line = element_line(color = "black"), 
    axis.text.x = element_text(color = "black", size = 12, lineheight = 0.9),  
    axis.text.y = element_text(color = "black", size = 12, lineheight = 0.9),  
    axis.ticks = element_line(color = "black", size = 0.5),  
    axis.title.x = element_text(color = "black", margin = margin(0, 10, 0, 0), size = 12),  
    axis.title.y = element_text(color = "black", angle = 90, margin = margin(0, 10, 0, 0), size = 12),  
    axis.ticks.length = unit(0.5, "lines"), 
    legend.background = element_rect(color = NULL, fill = "white"),  
    legend.key = element_rect(color = "black",  fill = "white"),  
    legend.key.size = unit(1.2, "lines"),  
    legend.key.height = NULL,  
    legend.key.width = NULL,      
    legend.text = element_text(color = "black"),  
    legend.title = element_text(face = "bold", hjust = 0, color = "black"),  
    legend.text.align = NULL,  
    legend.title.align = NULL,  
    legend.direction = "horizontal",  
    legend.box = "horizontal", 
    legend.justification = c("right", "top"),
    legend.position = c(.95, .95),
    panel.background = element_rect(fill = NA, color  =  NA),  
    panel.border =  element_rect(linetype = "solid", fill = NA, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    strip.background = element_rect(
      color = "black", fill = "#FF0066", size= 1.5, linetype="solid"),
    strip.text.x = element_text(color = "white"),  
    strip.text.y = element_text(color = "white", angle = -90),  
    plot.background = element_rect(color = "white", fill = "white"),  
    plot.title = element_text(color = "black", hjust = 0.5, lineheight = 1.25,
                              margin = margin(2, 2, 2, 2), face = "bold.italic"),  
    plot.subtitle = element_text(color = "black", hjust = 0, margin = margin(2, 2, 2, 2)),  
    plot.caption = element_text(color = "black", hjust = 0),  
    plot.margin = unit(rep(1, 4), "lines"))
}


############################### tmap of NZ ############################################
data(nz)

nz$color <- rep("rgba(255,0,0,0.4)", 16)

drops <- c("Land_area","Median_income", "Sex_ratio", "Population", "Island")
nz <- nz[ , !(names(nz) %in% drops)]

nz$People <- LETTERS[1:16]


##############################################
#                                            #
# https://github.com/swsoyee/2019-ncov-japan #
#              Author: Su Wei                #   
#                                            #
##############################################

getChangeIcon <- function(number) {
  if (number > 0) {
    return("fa fa-caret-up")
  } else {
    return("fa fa-caret-down")
  }
}

getChangeColor <- function(number) {
  if (number > 0) {
    return("blue")
  } else {
    return("red")
  }
}
getChangeIconWrapper <- function(number, type = "icon") {
  if (type == "icon") {
    return(getChangeIcon(number))
  } else {
    return(getChangeIcon(number))
  }
}

