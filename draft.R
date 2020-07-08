inputdata <- orange[orange$Name == 'A',]
orange1

tsdata <- ts(inputdata$Total, frequency = 12,
             start = c(min(inputdata$Year), min(inputdata[inputdata$Year == min(inputdata$Year), "Month"])))

tsdata

training <- window(tsdata, 
                start = c(start(time(tsdata))[1], match(month.abb[cycle(tsdata)][1], month.abb)), 
                end = c(floor(time(tsdata)[floor(length(tsdata)*0.8)]),
                        match(month.abb[cycle(tsdata)][floor(length(tsdata)*0.8)], month.abb)))
training

test <- window(tsdata, 
               start = c(floor(time(tsdata)[floor(length(tsdata)*0.8)]), match(month.abb[cycle(tsdata)][floor(length(tsdata)*0.8)], month.abb) + 1))
test

datafrme <- orange[orange$Name == 'B',]

d2 <- datafrme %>% 
  filter(Year == max(datafrme$Year)) %>%
  mutate(yearly_ave = sum(Total)/nrow(datafrme[datafrme$Year==max(datafrme$Year),]))

round(d2$yearly_ave, 2)

paste(last(ceiling(d2$yearly_ave)), "oranges")


data_by_year <- data_by_region[data_by_region$Year == 2014,]
data_by_year <- data.frame(data_by_year)
data_by_year$region <- factor(data_by_year$Region)

data_by_year %>%
  e_charts(Region) %>%
  e_map_register("NZ", nz_json) %>%
  e_map(Oranges, map = "NZ") %>%
  e_visual_map(
    Oranges,
    top = "20%",
    left = "0%",
    inRange = list(color = c("#3366FF","#6699FF", "#66CCFF", "#33CCFF")),
    type = "piecewise",
    splitList = list(
      list(min = 1300),
      list(min = 1100, max = 1200),
      list(min = 1000, max = 1100),
      list(value = 0, label = "None")
    ),
    formatter = htmlwidgets::JS("function(value, index, values){
                if(index == 'Infinity'){
                return ' > ' + value.toLocaleString()
                }else if (value != 0){
                return value.toLocaleString() + ' - ' + index.toLocaleString()
                }else{
                return value.toLocaleString()
                }
            }")) %>%
  e_tooltip(formatter = htmlwidgets::JS("
                         function(params){
                        return('<strong>' + params.name +
                     '</strong><br />Total: ' +  echarts.format.addCommas(params.value)) }")) %>%
  e_title(
    text = "Visitors grouped by year and region",
    subtext = "Choose a year and hover over the map for more information")


familyA <- orange[orange$Name == "A",]
nrow(familyA)

familyAyr1 <- familyA[familyA$Year == 2018,]
sum(familyAyr1$Total)

taildt <- tail(familyA, n = 72)
taildt

true <- ts(taildt$Total, frequency = 12,
           start = c(min(taildt$Year), min(taildt[taildt$Year == min(taildt$Year), "Month"])))


train <- window(true, 
                start = c(start(time(true))[1], match(month.abb[cycle(true)][1], month.abb)), 
                end = c(floor(time(true)[floor(length(true)*0.8)]),
                        match(month.abb[cycle(true)][floor(length(true)*0.8)], month.abb)))

arimadata <- auto.arima(train, stepwise = FALSE, approximation = FALSE)

testing <- window(true, 
                  start = c(floor(time(true)[floor(length(true)*0.8)]),
                            match(month.abb[cycle(true)][floor(length(true)*0.8)], month.abb)+1))

fc <- forecast(arimadata, h = 36, level = c(30,50,70))
xy <- t(accuracy(fc, testing))
xy



tail(true, n = 24)
