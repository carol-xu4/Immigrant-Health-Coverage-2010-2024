## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory -----------------------------------------------------
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Immigrant Health Coverage 2010-2024")

# ACS data ------------------------------------------------------------------
acsdata = fread("data/output/acsdata.csv")

# immigrant status counts
immig_counts = acsdata %>%
  group_by(year, immig_status) %>%
  summarise(
    n = n(),
    population = sum(perwt, na.rm = TRUE)) %>%
  ungroup()

write_csv(immig_counts, "results/immig_counts_year.csv")

# share of working-age immigrants who are undocumented?
immig_counts %>%
  filter(year == 2024, immig_status != "Native-born") %>%
  mutate(undoc_share = population / sum(population))

colors = c(
  "Native-born"         = "#3043B4",
  "Naturalized citizen" = "#0D0E51",
  "Legal immigrant"     = "#7C756D",
  "Undocumented"        = "#C97703")

ACS_population = ggplot(immig_counts, aes(x = as.numeric(year), y = population / 1e6, color = immig_status)) +
  geom_line(linewidth = 1.8) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    breaks = seq(0, 300, by = 50),
    labels = function(x) paste0(x, "M"),
    limits = c(0, 200),
    expand = c(0.02, 0)) +
  labs(
    title = "Population by Immigration Status (2010-2024)",
    subtitle = "ACS; Working-age adults (18-64)",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
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

ggsave("results/ACS_population.png", width = 15, height = 10)

# health coverage
coverage_counts = acsdata %>%
  mutate(
    coverage_type = case_when(
      hcovany == 1 ~ "Uninsured",
      hinsemp == 2 ~ "Employer-sponsored",
      hinspur == 2 ~ "Direct purchase",
      hinscaid == 2 ~ "Medicaid",
      hinscare == 2 ~ "Medicare",
      hinstri == 2 | hinsva == 2 ~ "Other public",
      TRUE ~ "Unknown")) %>%
  group_by(year, immig_status, coverage_type) %>%
  summarise(
    n = n(),
    population = sum(perwt, na.rm = TRUE)) %>%
  ungroup()

write_csv(coverage_counts, "results/coverage_counts_year.csv")

# 2024 coverage
coverage2024 = coverage_counts %>%
  filter(year == 2024) %>%
  group_by(immig_status) %>%
  mutate(rate = population / sum(population)) %>%
  ungroup()

ACS_coverage_2024 = ggplot(coverage2024, aes(x = immig_status, y = rate, fill = coverage_type)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.1)) +
  scale_fill_manual(values = c(
    "Employer-sponsored" = "#3043B4",
    "Direct purchase"    = "#7C756D",
    "Medicaid"           = "#C97703",
    "Medicare"           = "#0D0E51",
    "Other public"       = "#6B8E23",
    "Uninsured"          = "#C0392B")) +
  labs(
    title = "Health Insurance Coverage Type by Immigration Status (2024)",
    subtitle = "ACS; Working-age adults (18–64)",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/ACS_coverage_2024.png", width = 10, height = 6)

# 2010 coverage
coverage2010 = coverage_counts %>%
  filter(year == 2010) %>%
  group_by(immig_status) %>%
  mutate(rate = population / sum(population)) %>%
  ungroup()

ACS_coverage_2010 = ggplot(coverage2010, aes(x = immig_status, y = rate, fill = coverage_type)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.1)) +
  scale_fill_manual(values = c(
    "Employer-sponsored" = "#3043B4",
    "Direct purchase"    = "#7C756D",
    "Medicaid"           = "#C97703",
    "Medicare"           = "#0D0E51",
    "Other public"       = "#6B8E23",
    "Uninsured"          = "#C0392B")) +
  labs(
    title = "Health Insurance Coverage Type by Immigration Status (2010)",
    subtitle = "ACS; Working-age adults (18–64)",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/ACS_coverage_2010.png", width = 10, height = 6)

# native-born vs. legal vs. undocumented
coverage_2024_grouped = coverage_counts %>%
  filter(year == 2024) %>%
  mutate(
    immig_status = as.character(immig_status),
    group = case_when(
      immig_status == "Native-born"                                    ~ "Native-born",
      immig_status %in% c("Naturalized citizen", "Legal immigrant")    ~ "Legal immigrant",
      immig_status == "Undocumented"                                   ~ "Undocumented")) %>%
  group_by(group, coverage_type) %>%
  summarise(population = sum(population), .groups = "drop") %>%
  group_by(group) %>%
  mutate(rate = population / sum(population)) %>%
  ungroup() %>%
  mutate(group = factor(group, levels = c("Native-born", "Legal immigrant", "Undocumented")))

ACS_coverage_2024_grouped = ggplot(coverage_2024_grouped, aes(x = group, y = rate, fill = coverage_type)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.1)) +
  scale_fill_manual(values = c(
    "Employer-sponsored" = "#3043B4",
    "Direct purchase"    = "#7C756D",
    "Medicaid"           = "#C97703",
    "Medicare"           = "#0D0E51",
    "Other public"       = "#6B8E23",
    "Uninsured"          = "#C0392B")) +
  labs(
    title = "Health Insurance Coverage by Immigration Status (2024)",
    subtitle = "ACS; Working-age adults 18–64",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/ACS_coverage_2024_grouped.png", ACS_coverage_2024_grouped, width = 8, height = 6)

# combining all immigrants vs. native-born
coverage_2024_COMBINED = coverage_counts %>%
  filter(year == 2024) %>%
  mutate(
    immig_status = as.character(immig_status),
    group = case_when(
      immig_status == "Native-born" ~ "Native-born",
      TRUE                          ~ "All immigrants")) %>%
  group_by(group, coverage_type) %>%
  summarise(population = sum(population), .groups = "drop") %>%
  group_by(group) %>%
  mutate(rate = population / sum(population)) %>%
  ungroup() %>%
  mutate(group = factor(group, levels = c("Native-born", "All immigrants")))

write_csv(coverage_2024_COMBINED, "results/coverage_2024_COMBINED.csv")

ACS_coverage_2024_COMBINED = ggplot(coverage_2024_COMBINED, aes(x = group, y = rate, fill = coverage_type)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.1)) +
  scale_fill_manual(values = c(
    "Employer-sponsored" = "#3043B4",
    "Direct purchase"    = "#7C756D",
    "Medicaid"           = "#C97703",
    "Medicare"           = "#0D0E51",
    "Other public"       = "#6B8E23",
    "Uninsured"          = "#C0392B")) +
  labs(
    title = "Health Insurance Coverage by Immigration Status (2024)",
    subtitle = "ACS; Working-age adults 18–64",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/ACS_coverage_2024_COMBINED.png", ACS_coverage_2024_COMBINED, width = 8, height = 6)
