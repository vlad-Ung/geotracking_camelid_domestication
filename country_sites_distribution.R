library(dplyr)
library(ggplot2)

dir <- "map_project/outputs/statistics"

species_list <- c("bactrian", "dromedary", "hybrid", "unidentified", "all_camels")

for (species in species_list) {
  file <- paste0(dir, "/", species, "_counts.csv")

  df <- read.csv(file)

  g <- ggplot(df, aes(NUMPOINTS, forcats::fct_reorder(shapeName, NUMPOINTS))) +
    geom_bar(stat = "identity") +
    labs(
      title = "Site distribution across modern countries",
      x = "counts",
      y = ""
    )
  output_file <- paste0(dir, "/", species, "_counts.png")

  ggsave(output_file, g)
}
