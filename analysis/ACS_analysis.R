## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory -----------------------------------------------------
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Immigrant Health Coverage 2010-2024")

# ACS -----------------------------------------------------------------------
acsdata = fread("data/output/acsdata.csv")

# immigrant status counts
immig_counts = acsdata %>%
  group_by(year, immig_status) %>%
  summarise(
    n = n(),
    population = sum(perwt, na.rm = TRUE)) %>%
  ungroup()

write_csv(immig_counts, "results/immig_counts_year.csv")

# what share of working-age immigrants are undocumented?
immig_counts %>%
  filter(year == 2024, immig_status != "Native-born") %>%
  mutate(undoc_share = population / sum(population))
  
colors = c(
  "Native-born"         = "#3043B4",
  "Naturalized citizen" = "#0D0E51",
  "Legal immigrant"     = "#7C756D",
  "Undocumented"        = "#C97703")

ACS_population = ggplot(immig_counts, aes(x = as.numeric(year), y = population / 1e6, color = immig_status)) +
  geom_line(linewidth = 1.1) +
  geom_point() +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(0, 200, by = 25)) +
  labs(
    title = "Population by Immigration Status (2010-2024)",
    subtitle = "ACS; Working-age adults 18–64",
    x = NULL,
    y = "Population (millions)",
    color = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_stata() +
  theme(
    plot.title = element_text(size = 40, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 30, color = "black", margin = margin(b = 12), hjust = 0),
    legend.position = "bottom",
    legend.text = element_text(size = 20),
    axis.title.y = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25, angle = 0, vjust = 0.5),
    plot.caption = element_text(size = 12),
    plot.background = element_rect(fill = "white"))

ggsave("results/ACS_population.png", width = 15, height = 10)
