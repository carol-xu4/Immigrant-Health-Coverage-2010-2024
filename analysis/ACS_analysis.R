## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats)

# Set working directory
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

# share of immigrants who are undocumented?
immig_counts %>%
  filter(year == 2024, immig_status != "Native-born") %>%
  mutate(undoc_share = population / sum(population))

# how many legal immigrant children are there (non-citizen)
acsdata %>% filter(immig_status == "Legal immigrant", age <= 18) %>%
  group_by(year) %>%
  summarise(n = n(), population = sum(perwt, na.rm = TRUE))

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
    limits = c(0, 300),
    expand = c(0.02, 0)) +
  labs(
    title = "Population by Immigration Status (2010-2024)",
    subtitle = "ACS; Ages 0-97",
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

# HEALTH COVERAGE ------------------------------------------------------------------------
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
    subtitle = "ACS",
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
    subtitle = "ACS",
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
    subtitle = "ACS",
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
    subtitle = "ACS",
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

# UNINSURED & MEDICAID RATES ---------------------------------------------------------
uninsured_trend = acsdata %>%
  mutate(uninsured = ifelse(hcovany == 1, perwt, 0)) %>%
  group_by(year, immig_status) %>%
  summarise(
    total_pop   = sum(perwt, na.rm = TRUE),
    uninsured   = sum(uninsured, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(uninsured_rate = uninsured / total_pop)

ACS_uninsured_trend = ggplot(uninsured_trend, aes(x = as.numeric(year), y = uninsured_rate, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.05),
    limits = c(0, 0.65),
    expand = c(0.02, 0)) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = 2014.1, y = 0.55, label = "ACA (2014)",
           hjust = 0, size = 3, color = "gray50") +
  labs(
    title = "Uninsured Rate by Immigration Status (2010–2024)",
    subtitle = "ACS",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 10, color = "gray40"),
    axis.text.y = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/ACS_uninsured_trend.png", ACS_uninsured_trend, width = 10, height = 6)

# medicaid rate
medicaid_trend = acsdata %>%
  mutate(medicaid = ifelse(hinscaid == 2, perwt, 0)) %>%
  group_by(year, immig_status) %>%
  summarise(
    total_pop    = sum(perwt, na.rm = TRUE),
    medicaid     = sum(medicaid, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(medicaid_rate = medicaid / total_pop)

ACS_medicaid_trend = ggplot(medicaid_trend, aes(x = as.numeric(year), y = medicaid_rate, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703"
  )) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.05),
    limits = c(0, 0.5),
    expand = c(0.02, 0)) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = 2014, y = 0.33, label = "ACA (2014)",
           hjust = 0, size = 3, color = "gray50") +
  labs(
    title = "Medicaid Coverage Rate by Immigration Status (2010–2024)",
    subtitle = "ACS",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 10, color = "gray40"),
    axis.text.y = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/ACS_medicaid_trend.png", ACS_medicaid_trend, width = 10, height = 6)

# EMPLOYER-SPONSORED INSURANCE -----------------------------------------------------------------------------
esi_trend = acsdata %>%
  mutate(esi = ifelse(hinsemp == 2, perwt, 0)) %>%
  group_by(year, immig_status) %>%
  summarise(
    total_pop    = sum(perwt, na.rm = TRUE),
    esi          = sum(esi, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(esi_rate = esi / total_pop)

ACS_esi_trend = ggplot(esi_trend, aes(x = as.numeric(year), y = esi_rate, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.05),
    limit = c(0.25, 0.6),
    expand = c(0.02, 0)) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = 2014, y = 0.45, label = "ACA (2014)",
           hjust = 0, size = 3, color = "gray50") +
  labs(
    title = "Employer-Sponsored Coverage Rate by Immigration Status (2010–2024)",
    subtitle = "ACS",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 10, color = "gray40"),
    axis.text.y = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/ACS_esi_trend.png", ACS_esi_trend, width = 10, height = 6)

# All Immigrants vs. Native-born uninsured and medicaid
uninsured2 = acsdata %>%
  mutate(
    group = ifelse(as.character(immig_status) == "Native-born", "Native-born", "Immigrants"),
    uninsured = ifelse(hcovany == 1, perwt, 0)) %>%
  group_by(year, group) %>%
  summarise( total_pop = sum(perwt, na.rm = TRUE),
            uninsured = sum(uninsured, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(uninsured_rate = uninsured / total_pop)

ggplot(uninsured2, aes(x = as.numeric(year), y = uninsured_rate, color = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Native-born"    = "#3043B4",
    "Immigrants" = "#C97703")) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.05),
    expand = c(0.02, 0)) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = 2014.1, y = 0.20, label = "ACA (2014)",
           hjust = 0, size = 3, color = "gray50") +
  labs(
    title = "Uninsured Rate: Native-Born vs All Immigrants (2010–2024)",
    subtitle = "ACS",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 10, color = "gray40"),
    axis.text.y = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/ACS_uninsured2.png", width = 10, height = 6)

# medicaid, all immigrants vs. native- born
medicaid2 = acsdata %>%
  mutate(
    group    = ifelse(as.character(immig_status) == "Native-born", "Native-born", "Immigrants"),
    medicaid = ifelse(hinscaid == 2, perwt, 0)) %>%
  group_by(year, group) %>%
  summarise(
    total_pop = sum(perwt, na.rm = TRUE),
    medicaid  = sum(medicaid, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(medicaid_rate = medicaid / total_pop)

ACS_medicaid2 = ggplot(medicaid2, aes(x = as.numeric(year), y = medicaid_rate, color = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Native-born"    = "#3043B4",
    "Immigrants" = "#C97703")) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.05),
    expand = c(0.02, 0)) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = 2014.1, y = 0.19, label = "ACA (2014)",
           hjust = 0, size = 3, color = "gray50") +
  labs(
    title = "Medicaid Rate: Native-Born vs All Immigrants (2010–2024)",
    subtitle = "ACS",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 10, color = "gray40"),
    axis.text.y = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/ACS_medicaid2.png", ACS_medicaid2, width = 10, height = 6)

## AGE -------------------------------------------------------------------------------
age_medicaid2024 = acsdata %>%
    filter(year == 2024, hinscaid ==2) %>%
    mutate(group = ifelse(as.character(immig_status) == "Native-born", "Native-born", "Immigrants"))   

ACS_age_density = ggplot(age_medicaid2024, aes(x = age, fill = group, color = group,weight = perwt)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c(
    "Native-born"    = "#3043B4",
    "Immigrants" = "#C97703")) +
  scale_color_manual(values = c(
    "Native-born"    = "#3043B4",
    "Immigrants" = "#C97703")) +
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  labs(
    title = "Age Distribution of Medicaid Enrollees (2024)",
    subtitle = "Native-born vs. all immigrants",
    x = "Age",
    y = NULL,
    fill = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 10, color = "gray40"),
    axis.text.y = element_blank(),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/ACS_age_density2024.png", width = 10, height = 6)

age_medicaid2024 = acsdata %>%
  filter(year == 2024, hinscaid == 2)

ggplot(age_medicaid2024, aes(x = age, fill = immig_status, color = immig_status, weight = perwt)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  geom_vline(xintercept = 18, linetype = "dashed", color = "gray70", linewidth = 0.5) +
  geom_vline(xintercept = 65, linetype = "dashed", color = "gray70", linewidth = 0.5) +
  annotate("text", x = 18.5, y = Inf, label = "Age 18", vjust = 1.5,
           hjust = 0, size = 3, color = "gray50") +
  annotate("text", x = 65.5, y = Inf, label = "Age 65", vjust = 1.5,
           hjust = 0, size = 3, color = "gray50") +
  labs(
    title = "Age Distribution of Medicaid Enrollees (2024)",
    subtitle = "ACS",
    x = "Age",
    y = NULL,
    fill = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 10, color = "gray40"),
    axis.text.y = element_blank(),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/ACS_age_density2024_2.png", width = 10, height = 6, dpi = 300)

# median age of medicaid enrollees over time, by immigration status
medicaid_age_trend = acsdata %>%
  filter(hinscaid ==2) %>%
  group_by(year, immig_status) %>%
  summarise(median_age = weightedMedian(age, w = perwt, na.rm = TRUE),
  .groups = "drop")

ACS_medicaid_age_trend = ggplot(medicaid_age_trend, aes(x = as.numeric(year), y = median_age, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(breaks = seq(0, 80, by = 5), expand = c(0.02, 0), limits = c(10, 60)) +
  labs(
    title = "Median Age of Medicaid Enrollees by Immigration Status (2010–2024)",
    subtitle = "ACS",
    x = NULL,
    y = "Median age",
    color = NULL,
    caption = "Source: ACS PUMS via IPUMS, authors' calculations") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 10, color = "gray40"),
    axis.text.y = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/ACS_medicaid_age_trend.png", ACS_medicaid_age_trend, width = 10, height = 6)

# CALIFORNIA --------------------------------------------------------------------
acs_ca = acsdata %>%
  filter(statefip == 6)

ca_immig_counts = acs_ca %>%
  group_by(year, immig_status) %>%
  summarise( 
    n = n(), population = sum(perwt, na.rm = TRUE), .groups = "drop")

ggplot(ca_immig_counts, aes(x = as.numeric(year), y = population / 1e6, color = immig_status)) +
  geom_line(linewidth = 1.8) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    breaks = seq(0, 30, by = 5),
    labels = function(x) paste0(x, "M"),
    limits = c(0, 30),
    expand = c(0.02, 0)) +
  labs(
    title = "California Population by Immigration Status (2010-2024)",
    subtitle = "ACS",
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

ggsave("results/ACS_CA_population.png", width = 15, height = 10)

ca_uninsured = acs_ca %>%
  mutate(uninsured = ifelse(hcovany == 1, perwt, 0)) %>%
  group_by(year, immig_status) %>%
  summarise(
    total_pop      = sum(perwt, na.rm = TRUE),
    uninsured      = sum(uninsured, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(uninsured_rate = uninsured / total_pop)

ACS_CA_uninsured = ggplot(ca_uninsured, 
                            aes(x = as.numeric(year), y = uninsured_rate, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.05),
    expand = c(0.02, 0)) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_vline(xintercept = 2016, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = 2014.1, y = 0.55, label = "ACA (2014)",       hjust = 0, size = 3, color = "gray50") +
  annotate("text", x = 2016.1, y = 0.55, label = "Medi-Cal <19",     hjust = 0, size = 3, color = "gray50") +
  annotate("text", x = 2020.1, y = 0.50, label = "Medi-Cal <26",     hjust = 0, size = 3, color = "gray50") +
  annotate("text", x = 2022.1, y = 0.45, label = "Medi-Cal 50+",     hjust = 0, size = 3, color = "gray50") +
  labs(
    title = "Uninsured Rate by Immigration Status — California (2010–2024)",
    subtitle = "ACS",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 10, color = "gray40"),
    axis.text.y = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/ACS_CA_uninsured.png", width = 10, height = 6)

ca_medicaid = acs_ca %>%
  mutate(medicaid = ifelse(hinscaid == 2, perwt, 0)) %>%
  group_by(year, immig_status) %>%
  summarise(
    total_pop     = sum(perwt, na.rm = TRUE),
    medicaid      = sum(medicaid, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(medicaid_rate = medicaid / total_pop)

ACS_CA_medicaid = ggplot(ca_medicaid,
                           aes(x = as.numeric(year), y = medicaid_rate, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.05),
    expand = c(0.02, 0)) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_vline(xintercept = 2016, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = 2014.1, y = 0.45, label = "ACA (2014)",       hjust = 0, size = 3, color = "gray50") +
  annotate("text", x = 2016.1, y = 0.45, label = "Medi-Cal <19",     hjust = 0, size = 3, color = "gray50") +
  annotate("text", x = 2020.1, y = 0.45, label = "Medi-Cal <26",     hjust = 0, size = 3, color = "gray50") +
  annotate("text", x = 2022.1, y = 0.45, label = "Medi-Cal 50+",     hjust = 0, size = 3, color = "gray50") +
  labs(
    title = "Medicaid Rate by Immigration Status — California (2010–2024)",
    subtitle = "ACS",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 10, color = "gray40"),
    axis.text.y = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/ACS_CA_medicaid.png", width = 10, height = 6)

ca_medicaid_age_trend = acsdata %>%
  filter(hinscaid == 2, statefip == 6) %>%
  group_by(year, immig_status) %>%
  summarise(median_age = weightedMedian(age, w = perwt, na.rm = TRUE), 
    .groups = "drop")

CA_medicaid_age_trend = ggplot(ca_medicaid_age_trend, aes(x = as.numeric(year), y = median_age, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(breaks = seq(0, 80, by = 5), expand = c(0.02, 0), limits = c(10, 65)) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_vline(xintercept = 2016, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = 2014.1, y = 55, label = "ACA",          hjust = 0, size = 3, color = "gray50") +
  annotate("text", x = 2016.1, y = 55, label = "Medi-Cal <19", hjust = 0, size = 3, color = "gray50") +
  annotate("text", x = 2020.1, y = 55, label = "Medi-Cal <26", hjust = 0, size = 3, color = "gray50") +
  annotate("text", x = 2022.1, y = 55, label = "Medi-Cal 50+", hjust = 0, size = 3, color = "gray50") +
  labs(
    title = "Median Age of Medicaid Enrollees by Immigration Status — California (2010–2024)",
    subtitle = "ACS",
    x = NULL,
    y = "Median age",
    color = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 10, color = "gray40"),
    axis.text.y = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/CA_medicaid_age_trend.png", CA_medicaid_age_trend, width = 10, height = 6)

# removing california
acs_noca = acsdata %>%
  filter(statefip != 6) %>%
  group_by(year, immig_status) %>%
  summarise(median_age = weightedMedian(age, w = perwt, na.rm = TRUE), 
    .groups = "drop")

ggplot(acs_noca, aes(x = as.numeric(year), y = median_age, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(breaks = seq(0, 80, by = 5), expand = c(0.02, 0), limits = c(10, 60)) +
  labs(
    title = "Median Age of Medicaid Enrollees by Immigration Status (2010–2024)",
    subtitle = "ACS; excluding California",
    x = NULL,
    y = "Median age",
    color = NULL,
    caption = "Source: ACS PUMS via IPUMS, authors' calculations") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 10, color = "gray40"),
    axis.text.y = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/NOCA_medicaid_age_trend.png", width = 10, height = 6)

# STATE EXPANSIONS ----------------------------------------------------------------------------
acsdata = acsdata %>%
  mutate(expansion_state = case_when(
    # California — phased expansion
    statefip == 6  & year >= 2016 & age <= 18                        ~ 1,  # children under 19
    statefip == 6  & year >= 2020 & age <= 25                        ~ 1,  # young adults under 26
    statefip == 6  & year >= 2022 & age >= 50                        ~ 1,  # adults 50+
    statefip == 6  & year >= 2024 & age >= 26 & age <= 49            ~ 1,  # all adults 26-49
    # Oregon — full expansion all ages
    statefip == 41 & year >= 2022                                     ~ 1,
    # Illinois — adults 42+
    statefip == 17 & year >= 2022 & age >= 42                        ~ 1,
    # New York — adults 65+
    statefip == 36 & year >= 2024 & age >= 65                        ~ 1,
    # Colorado — adults 
    statefip == 8  & year >= 2023                                     ~ 1,
    # Washington — adults 
    statefip == 53 & year >= 2024                                     ~ 1,
    # Minnesota — adults
    statefip == 27 & year >= 2024                                     ~ 1,
    TRUE ~ 0 )) %>%
  mutate(expansion_label = ifelse(expansion_state == 1,
                                  "Expansion state",
                                  "Non-expansion state"))

undoc_expansion = acsdata %>%
  filter(immig_status == "Undocumented")

uninsured_expansion = undoc_expansion %>%
  mutate(uninsured = ifelse(hcovany ==1, perwt, 0)) %>%
  group_by(year, expansion_label) %>%
  summarise(total_pop = sum(perwt, na.rm = TRUE),
            uninsured = sum(uninsured, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(uninsured_rate = uninsured / total_pop)

ACS_undoc_uninsured_expansion = ggplot(uninsured_expansion,
  aes(x = as.numeric(year), y = uninsured_rate,
        color = expansion_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Expansion state"     = "#C97703",
    "Non-expansion state" = "#3043B4")) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.05),
    limits = c(0, 0.65),
    expand = c(0.02, 0)) +
    geom_vline(xintercept = 2016, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_vline(xintercept = 2020, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_vline(xintercept = 2022, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_vline(xintercept = 2023, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_vline(xintercept = 2024, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    annotate("text", x = 2016.1, y = 0.61, label = "CA <19",                hjust = 0, size = 2.8, color = "gray50") +
    annotate("text", x = 2020.1, y = 0.61, label = "CA <26",                hjust = 0, size = 2.8, color = "gray50") +
    annotate("text", x = 2022.1, y = 0.61, label = "CA 50+",                hjust = 0, size = 2.8, color = "gray50") +
    annotate("text", x = 2022.1, y = 0.56, label = "OR",                    hjust = 0, size = 2.8, color = "gray50") +
    annotate("text", x = 2022.1, y = 0.51, label = "IL 42+",                hjust = 0, size = 2.8, color = "gray50") +
    annotate("text", x = 2023.1, y = 0.61, label = "CO",                    hjust = 0, size = 2.8, color = "gray50") +
    annotate("text", x = 2024.1, y = 0.61, label = "CA",                    hjust = 0, size = 2.8, color = "gray50") +
    annotate("text", x = 2024.1, y = 0.56, label = "MN",                    hjust = 0, size = 2.8, color = "gray50") +
    annotate("text", x = 2024.1, y = 0.51, label = "WA",                    hjust = 0, size = 2.8, color = "gray50") +
    annotate("text", x = 2024.1, y = 0.46, label = "NY 65+",                hjust = 0, size = 2.8, color = "gray50") +
  labs(
    title = "Uninsured Rate for Undocumented Immigrants (2010–2024)",
    subtitle = "Expansion states: California, Oregon, Illinois, New York, Colorado, Washington, Minnesota, DC",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS PUMS via IPUMS, authors' calculations") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 10, color = "gray40"),
    axis.text.y = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/ACS_undoc_uninsured_expansion.png", width = 10, height = 6)

# Medicaid rates, expansion states
medicaid_expansion = undoc_expansion %>%
  mutate(medicaid = ifelse(hinscaid ==2, perwt, 0)) %>%
  group_by(year, expansion_label) %>%
  summarise(total_pop = sum(perwt, na.rm = TRUE),
            medicaid = sum(medicaid, na.rm = TRUE),
            .groups = "drop") %>% 
  mutate(medicaid_rate = medicaid / total_pop)

ACS_undoc_medicaid_expansion = ggplot(medicaid_expansion, 
  aes(x = as.numeric(year), y = medicaid_rate, color = expansion_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Expansion state"     = "#C97703",
    "Non-expansion state" = "#3043B4")) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.05),
    limits = c(0, 0.35),
    expand = c(0.02, 0)) +
    geom_vline(xintercept = 2016, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_vline(xintercept = 2020, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_vline(xintercept = 2022, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_vline(xintercept = 2023, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_vline(xintercept = 2024, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    annotate("text", x = 2016.1, y = 0.31, label = "CA <19",                hjust = 0, size = 2.8, color = "gray50") +
    annotate("text", x = 2020.1, y = 0.31, label = "CA <26",                hjust = 0, size = 2.8, color = "gray50") +
    annotate("text", x = 2022.1, y = 0.31, label = "CA 50+",                hjust = 0, size = 2.8, color = "gray50") +
    annotate("text", x = 2022.1, y = 0.24, label = "OR",                    hjust = 0, size = 2.8, color = "gray50") +
    annotate("text", x = 2022.1, y = 0.21, label = "IL 42+",                hjust = 0, size = 2.8, color = "gray50") +
    annotate("text", x = 2023.1, y = 0.31, label = "CO",                    hjust = 0, size = 2.8, color = "gray50") +
    annotate("text", x = 2024.1, y = 0.31, label = "CA",                    hjust = 0, size = 2.8, color = "gray50") +
    annotate("text", x = 2024.1, y = 0.26, label = "MN",                    hjust = 0, size = 2.8, color = "gray50") +
    annotate("text", x = 2024.1, y = 0.21, label = "WA",                    hjust = 0, size = 2.8, color = "gray50") +
    annotate("text", x = 2024.1, y = 0.16, label = "NY 65+",                hjust = 0, size = 2.8, color = "gray50") +
  labs(
    title = "Medicaid EnrollmentRate for Undocumented Immigrants (2010–2024)",
    subtitle = "Expansion states: California, Oregon, Illinois, New York, Colorado, Washington, Minnesota, DC",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS PUMS via IPUMS, authors' calculations") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 10, color = "gray40"),
    axis.text.y = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/ACS_undoc_medicaid_expansion.png", width = 10, height = 6)

# age 65+ medicaid, misclassification check -----------------------------------------------------------------
# CMS: total dual eligible enrollees
CMS_dual_enrollment = data.frame(
  year = 2006:2019,
  total_medicare = c(45685188, 46735669, 47868545, 48916671, 50052677,
                      51667131, 53540256, 55206227, 56767778, 58294184,
                      59818470, 61205108, 62894069, 64443367),
  medicare_only = c(37035298, 37873733, 38775366, 39554304, 40290749,
                     41441332, 42984784, 44399188, 45607720, 46803894,
                     48062848, 49246041, 50717280, 52104185),
  dually_eligible = c(8649890, 8861936, 9093179, 9362367, 9761928,
                       10225799, 10555472, 10807039, 11160058, 11490290,
                       11755622, 11959067, 12176789, 12339182),
  full_benefit = c(6819768, 6880844, 7011147, 7115138, 7279339,
                     7482875, 7617630, 7748066, 8016044, 8234056,
                     8391305, 8542340, 8661245, 8768749),
  partial_benefit = c(1830122, 1981092, 2082032, 2247229, 2482589,
                        2742924, 2937842, 3058973, 3144014, 3256234,
                        3364317, 3416727, 3515544, 3570433))

write_csv(CMS_dual_enrollment, "data/input/CMS_dual_enrollment.csv")

# toal dual enrollees, ACS
dual_enrollees = acsdata %>%
  filter(age >= 65, hinscaid == 2, hinscare == 2) %>%
  group_by(year) %>%
  summarise(dual = sum(perwt, na.rm = TRUE)) %>%
  print(n = Inf)

