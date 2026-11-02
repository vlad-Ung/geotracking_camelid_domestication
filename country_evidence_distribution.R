library(dplyr)
library(ggplot2)

dir <- "map_project/outputs/statistics"

species <- "all_camels"

file <- paste0(dir, "/", species, "_countries_attributes.csv")

df <- read.csv(file)

summary <- df |>
  count(evidence_t, shapeName)

summary$evidence_t[summary$evidence_t == ""] <- "NA"

g <- ggplot(summary, aes(
  x = n,
  y = forcats::fct_reorder(shapeName, n),
  fill = forcats::fct_reorder2(evidence_t, n, shapeName)
)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  scale_fill_viridis_d(option = "cividis") +
  labs(
    title = "Evidence distribution across modern countries",
    fill = "Evicence type",
    x = "count",
    y = ""
  )

output_file <- paste0(dir, "/", species, "_evidence_distribution.png")

ggsave(output_file, g)
