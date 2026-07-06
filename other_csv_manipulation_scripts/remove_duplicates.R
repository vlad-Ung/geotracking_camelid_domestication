library(dplyr)

data <- read.csv("map_project/site_data.csv")

data <- data[!duplicated(data), ]

data <- data |> filter(!is.na(lat))

write.csv(data, "map_project/site_data.csv")