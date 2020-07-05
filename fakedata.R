region <- read.csv("regionone.csv", header = TRUE)
region2 <- read.csv("regiontwo.csv", header = TRUE)

set.seed(0)

df <- data.frame(Name = sort(rep(LETTERS[1:26], 48)),
                 Month = rep(1:12, 104),
                 Year = rep(c(rep(2014, 12), rep(2015, 12), rep(2016, 12), rep(2017, 12)), 26),
                 Total = floor(runif(1248, min = 0, max = 101)),
                 Region = rep(c(rep("Auckland", 48), rep("Bay of Plenty", 48), rep("Canterbury", 48),
                                rep("Hawke's Bay", 48), rep("Manawatu-Wanganui", 48), rep("Northland", 48),
                                rep("Otago", 48), rep("Southland", 48), rep("Taranaki", 48), rep("Tasman", 48),
                                rep("Waikato", 48), rep("Wellington", 48), rep("West Coast", 48)), 2),
                 Member = rep(floor(runif(26, min = 2, max = 6)), each = 48),
                 long = rep(c(rep(region2$long[region2$region == "Auckland"], 48), 
                              rep(region2$long[region2$region == "Bay of Plenty"], 48),
                              rep(region2$long[region2$region == "Canterbury"][1], 48), 
                              rep(region2$long[region2$region == "Hawke's Bay"], 48),
                              rep(region2$long[region2$region == "Manawatu-Wanganui"], 48), 
                              rep(region2$long[region2$region == "Northland"][1], 48),
                              rep(region2$long[region2$region == "Otago"][1], 48), 
                              rep(region2$long[region2$region == "Southland"][1], 48), 
                              rep(region2$long[region2$region == "Taranaki"], 48), 
                              rep(region2$long[region2$region == "Tasman"][1], 48), 
                              rep(region2$long[region2$region == "Waikato"][1], 48), 
                              rep(region2$long[region2$region == "Wellington"], 48), 
                              rep(region2$long[region2$region == "West Coast"][1], 48)), 2),
                 lat = rep(c(rep(region2$lat[region2$region == "Auckland"], 48), 
                             rep(region2$lat[region2$region == "Bay of Plenty"], 48),
                             rep(region2$lat[region2$region == "Canterbury"][1], 48), 
                             rep(region2$lat[region2$region == "Hawke's Bay"], 48),
                             rep(region2$lat[region2$region == "Manawatu-Wanganui"], 48), 
                             rep(region2$lat[region2$region == "Northland"][1], 48),
                             rep(region2$lat[region2$region == "Otago"][1], 48), 
                             rep(region2$lat[region2$region == "Southland"][1], 48), 
                             rep(region2$lat[region2$region == "Taranaki"], 48), 
                             rep(region2$lat[region2$region == "Tasman"][1], 48), 
                             rep(region2$lat[region2$region == "Waikato"][1], 48), 
                             rep(region2$lat[region2$region == "Wellington"], 48), 
                             rep(region2$lat[region2$region == "West Coast"][1], 48)), 2),
                 stringsAsFactors = FALSE)

write.csv(df, "oranges.csv")


