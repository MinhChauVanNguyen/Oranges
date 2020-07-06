region <- read.csv("NZregions.csv", header = TRUE, stringsAsFactors = FALSE)


set.seed(0)

df <- data.frame(
  Name = c(rep(LETTERS[1], 96),
           rep(LETTERS[2:5], each = 84),
           rep(LETTERS[6:11], each = 72),
           rep(LETTERS[12:16], each = 60)),
  Month = rep(1:12, 97),
  Year = c(rep(2013:2020, each = 12),
           rep(rep(2014:2020, each = 12), 4),
           rep(rep(2015:2020, each = 12), 6),
           rep(rep(2016:2020, each = 12), 5)),
  Member = c(rep(floor(runif(1, min = 2, max = 6)), 96),
             rep(floor(runif(4, min = 2, max = 6)), each = 84),
             rep(floor(runif(6, min = 2, max = 6)), each = 72),
             rep(floor(runif(5, min = 2, max = 6)), each = 60)),
  Total = c(floor(runif(96, min = 0, max = 50)),
            floor(runif(4*84, min = 0, max = 50)),
            floor(runif(6*72, min = 0, max = 50)),
            floor(runif(5*60, min = 0, max = 50))),
  Region = c(
    # 8 yrs (96 months) x 1
    rep("Northland", 96), 
    # 7 yrs (84 months) x 4
    rep("Auckland", 84), 
    rep("Waikato", 84),
    rep("Bay of Plenty", 84), 
    rep("Gisborne", 84), 
    # 6 yrs (72 months) x 6
    rep("Hawke's Bay", 72),
    rep("Taranaki", 72), 
    rep("Manawatu-Wanganui", 72), 
    rep("Wellington", 72), 
    rep("West Coast", 72),
    rep("Canterbury", 72), 
    # 5 yrs (60 months) x 5
    rep("Otago", 60), 
    rep("Southland", 60),
    rep("Tasman", 60),
    rep("Nelson", 60),
    rep("Marlborough", 60)),
  long = c(
    rep(region$long[region$region == "Northland"], 96), 
    # 7 yrs (84 months) x 4
    rep(region$long[region$region == "Auckland"], 84),
    rep(region$long[region$region == "Waikato"], 84), 
    rep(region$long[region$region == "Bay of Plenty"], 84),
    rep(region$long[region$region == "Gisborne"], 84), 
    # 6 yrs (72 months) x 6
    rep(region$long[region$region == "Hawke's Bay"], 72),
    rep(region$long[region$region == "Taranaki"], 72), 
    rep(region$long[region$region == "Manawatu-Wanganui"], 72), 
    rep(region$long[region$region == "Wellington"], 72), 
    rep(region$long[region$region == "West Coast"], 72),
    rep(region$long[region$region == "Canterbury"], 72), 
    # 5 yrs (60 months) x 5
    rep(region$long[region$region == "Otago"], 60), 
    rep(region$long[region$region == "Southland"], 60), 
    rep(region$long[region$region == "Tasman"], 60),
    rep(region$long[region$region == "Nelson"], 60),
    rep(region$long[region$region == "Marlborough"], 60)),
  lat = c(rep(
    region$lat[region$region == "Northland"], 96), 
    # 7 yrs (84 months) x 4
    rep(region$lat[region$region == "Auckland"], 84),
    rep(region$lat[region$region == "Waikato"], 84), 
    rep(region$lat[region$region == "Bay of Plenty"], 84),
    rep(region$lat[region$region == "Gisborne"], 84), 
    # 6 yrs (72 months) x 6
    rep(region$lat[region$region == "Hawke's Bay"], 72),
    rep(region$lat[region$region == "Taranaki"], 72), 
    rep(region$lat[region$region == "Manawatu-Wanganui"], 72), 
    rep(region$lat[region$region == "Wellington"], 72), 
    rep(region$lat[region$region == "West Coast"], 72),
    rep(region$lat[region$region == "Canterbury"], 72), 
    # 5 yrs (60 months) x 5
    rep(region$lat[region$region == "Otago"], 60), 
    rep(region$lat[region$region == "Southland"], 60), 
    rep(region$lat[region$region == "Tasman"], 60),
    rep(region$lat[region$region == "Nelson"], 60),
    rep(region$lat[region$region == "Marlborough"], 60)
  )
)

View(df)

write.csv(df, "oranges.csv")
