library(dplyr)

dir <- "map_project/inputs/camel_data"

species <- "all_camels"

file <- paste0(dir, "/", species, ".csv")

df <- read.csv(file)

summary <- df |> 
  count(site)