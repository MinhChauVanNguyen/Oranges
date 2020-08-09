library(recharts)
library(dplyr)

orange <- read.csv("oranges.csv", header = TRUE, stringsAsFactors = FALSE)

data <- group_by(orange, Region, long, lat) %>% summarize(Amount = sum(Total))
data <- data.frame(data)
names(data) <- c('name', 'lng', 'lat','value')
data

orange <- read.csv("oranges.csv", header = TRUE, stringsAsFactors = FALSE)

data_by_region <- group_by(orange, Name, Region, long, lat, Year) %>%
  summarise(Amount = sum(Total))

echartr(NULL, type='map_world', subtype='New zealand') %>% 
  addMP(series='Top 5',
        data=data.frame(
          name=c('Northland', 'Auckland', 'Nelson', 'Gisborne', 'Otago'),
          value=c(193, 194, 229, 273, 279)), 
        symbol='pin',
        symbolSize=JS('function (v) {return 10 + v/50;}'), 
        effect=list(show=TRUE),
        itemStyle = list(normal = itemStyle(color = "#EE82EE"))
  ) %>%
  addGeoCoord(data[,c('name', 'lng', 'lat')]) %>%
  setToolbox(show = FALSE) %>%
  setSeries(itemStyle=list(
    normal=itemStyle(
      labelStyle = labelStyle(color="#EE82EE"),
      borderColor='rgba(100,149,237,1)', borderWidth=0.5, 
      areaStyle=areaStyle(color='#1b1b1b')))) %>%
  setLegend(textStyle=textStyle(fontFamily='monospace', fontWeight='bold', fontSize=16, pos = 10)) %>%
  setTooltip(textStyle = textStyle(fontFamily="monospace"),
             formatter=JS('function (params) {
  return params.seriesName + "<br/>" + params.name + ": " + params.value + " oranges" }'))

