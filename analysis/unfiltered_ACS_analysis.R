## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory -----------------------------------------------------
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Immigrant Health Coverage 2010-2024")

# ACS, unfiltered --------------------------------------------------------
acs_unfiltered = fread("data/output/acs_unfiltered.csv")

unfiltered_immig_counts = acs_unfiltered %>%
  group_by(year, immig_status) %>%
  summarise(
    n = n(),
    population = sum(perwt, na.rm = TRUE)) %>%
  ungroup()

write_csv(unfiltered_immig_counts, "results/unfiltered_immig_counts_year.csv")

unfiltered_ACS_population = ggplot(unfiltered_immig_counts, aes(x = as.numeric(year), y = population / 1e6, color = immig_status)) +
  geom_line(linewidth = 1.8) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    breaks = seq(0, 300, by = 50),
    labels = function(x) paste0(x, "M"),
    limits = c(0, 300),
    expand = c(0.02, 0)) +
  labs(
    title = "Population by Immigration Status (2010-2024)",
    subtitle = "ACS; Unfiltered Sample",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 40, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 30, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 25, color = "gray40"),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/unfiltered_ACS_population.png", unfiltered_ACS_population, width = 15, height = 10, dpi = 300)
