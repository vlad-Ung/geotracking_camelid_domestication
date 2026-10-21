library(dplyr)

data <- read.csv("database/sites_cleaned.csv")

data <- data[!duplicated(data), ] |> 
  na.omit()

write.csv(data, "database/sites_cleaned.csv")