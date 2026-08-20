library(dplyr)
library(ggplot2)
library(openxlsx)

data <- read.xlsx("map_project/camel_data/literature_remains_sites_join.xlsx")

summary <- data |>
  group_by(evidence_type) |>
  summarise(n = n())

summary$evidence_type <- as.factor(summary$evidence_type)

p <- ggplot(summary, aes(forcats::fct_reorder(evidence_type, n, .desc = TRUE), n)) +
  geom_col() +
  geom_label(
    label = summary$n,
    nudge_y = 5
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15))
ggsave("map_project/evidence_distribution.png", plot = p, units = "px", width = 3507, height = 2480)
