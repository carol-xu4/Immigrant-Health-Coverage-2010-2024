# Preliminaries ------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory 
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Immigrant Health Coverage 2010-2024")

# analysis -------------------------------------------------------------------
acsdata = fread("data/output/acsdata.csv")

## adjusting income variables for inflation
# remove
acsdata = acsdata %>%
  mutate(
    inctot   = ifelse(inctot %in% c(9999999, 9999998), NA, inctot),
    ftotinc  = ifelse(ftotinc == 9999999, NA, ftotinc),
    poverty  = ifelse(poverty == 0, NA, poverty))

# 2024 factor from CPI99 = 0.531
table(acsdata$year, acsdata$cpi99)

cpi_2024 = acsdata$cpi99[acsdata$year == 2024][1]

cpi_2024

acsdata = acsdata %>%
  mutate(
    ftotinc_2024usd  = ftotinc  * cpi99 / cpi_2024,
    inctot_2024usd   = inctot   * cpi99 / cpi_2024)

# family income (pre-tax), by immig status (duplicates within families removed)
ftotinc_by_family = acsdata %>%
    filter(!is.na(ftotinc), pernum == 1) %>%
    group_by(year, immig_status) %>%
    summarise(
        mean_ftotinc = weighted.mean(ftotinc_2024usd, perwt, na.rm = TRUE),
        median_ftotinc = matrixStats::weightedMedian(ftotinc_2024usd, w = perwt, na.rm = TRUE),
        n = n(), .groups = "drop")

write_csv(ftotinc_by_family, "results/ftotinc_by_family.csv")

ggplot(ftotinc_by_family, aes(x = year, y = mean_ftotinc, color = immig_status)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    scale_y_continuous(labels = scales::label_dollar(scale = 1e-3, suffix = "K"), limits = c(60000, 130000), breaks = seq(60000, 130000, by = 10000)) +
    scale_x_continuous(breaks = unique(ftotinc_by_family$year)[c(TRUE, FALSE)]) +
    scale_color_manual(values = c(
        "Native-born"         = "#3043B4",
        "Naturalized citizen" = "#0D0E51",
        "Legal noncitizen"     = "#7C756D",
        "Undocumented"        = "#C97703")) +
    labs(
        title = "Mean Total family income, by immigration status",
        subtitle = "Mean family income (2024 dollars)",
        x = NULL, y = NULL,
        caption = "Source: ACS PUMS via IPUMS") +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
        plot.subtitle = element_text(size = 18, color = "gray40", hjust = 0, margin = margin(b = 12)),
        legend.position = "top",
        legend.justification = "left",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.spacing.x = unit(0.3, "cm"),
        legend.box.margin = margin(b = 5),
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
        plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE))

ggsave("results/mean_ftotinc_family.png", width = 15, height = 10)

ggplot(ftotinc_by_family, aes(x = year, y = median_ftotinc, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-3, suffix = "K")) +
  scale_x_continuous(breaks = unique(ftotinc_by_family$year)[c(TRUE, FALSE)]) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal noncitizen"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  labs(
    title = "Median Total family income, by immigration status",
    subtitle = "Median family income (2024 dollars);",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 18, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.spacing.x = unit(0.3, "cm"),
    legend.box.margin = margin(b = 5),
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
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

ggsave("results/median_ftotinc_by_immig_status.png", width = 15, height = 10)

# personal income (working age adults, 18-64)
acsinc_adults = acsdata %>%
    filter(age >= 18, age <= 64, !is.na(inctot))

inctot_immig = acsinc_adults %>%
    group_by(year, immig_status) %>%
    summarise(
        mean_inctot = weighted.mean(inctot_2024usd, perwt, na.rm = TRUE),
        median_inctot = matrixStats::weightedMedian(inctot_2024usd, w = perwt, na.rm = TRUE),
        n = n(), .groups = "drop")

write_csv(inctot_immig, "results/inctot_immig_year.csv")

ggplot(inctot_immig, aes(x = year, y = mean_inctot, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-3, suffix = "K")) +
  scale_x_continuous(breaks = unique(inctot_immig$year)[c(TRUE, FALSE)]) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal noncitizen"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  labs(
    title = "Mean Personal income, by immigration status",
    subtitle = "Mean personal income (2024 dollars); working-age adults 18-64",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 18, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.spacing.x = unit(0.3, "cm"),
    legend.box.margin = margin(b = 5),
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
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

ggsave("results/inctot_mean_by_immig_status.png", width = 15, height = 10)

ggplot(inctot_immig, aes(x = year, y = median_inctot, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-3, suffix = "K"), limits = c(10000, 50000)) +
  scale_x_continuous(breaks = unique(inctot_immig$year)[c(TRUE, FALSE)]) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal noncitizen"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  labs(
    title = "Median Personal income, by immigration status",
    subtitle = "Median personal income (2024 dollars); working-age adults 18-64",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 18, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.spacing.x = unit(0.3, "cm"),
    legend.box.margin = margin(b = 5),
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
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

ggsave("results/inctot_median_by_immig_status.png", width = 15, height = 10)

# poverty (one per family)
acspov = acsdata %>%
    filter(!is.na(poverty), pernum == 1)

poverty_immig = acspov %>%
    group_by(year, immig_status) %>%
    summarise(
        mean_poverty = weighted.mean(poverty, perwt, na.rm = TRUE),
        median_poverty = matrixStats::weightedMedian(poverty, w = perwt, na.rm = TRUE),
        n = n(), .groups = "drop")

poverty_rate_immig = acspov %>%
  group_by(year, immig_status) %>%
  summarise(
    pct_below_poverty = 100 * sum(perwt[poverty < 100]) / sum(perwt),
    pop = sum(perwt),
    n = n(),
    .groups = "drop")

print(poverty_rate_immig, n = Inf)

ggplot(poverty_rate_immig, aes(x = year, y = pct_below_poverty, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(10, 30)) +
  scale_x_continuous(breaks = unique(poverty_rate_immig$year)[c(TRUE, FALSE)]) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal noncitizen"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  labs(
    title = "Share of families below the poverty line, by immigration status",
    subtitle = "ACS; based on IPUMS-created family poverty threshold",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 28, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 18, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.spacing.x = unit(0.3, "cm"),
    legend.box.margin = margin(b = 5),
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
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

ggsave("results/poverty_rate_by_immig_status.png", width = 15, height = 10)

# arrival time
immigrants = acsdata %>%
    filter(!is.na(inctot), age >= 18, age <= 64, immig_status != "Native-born", yrimmig > 0) %>%
    mutate(years_in_us = year - yrimmig)

table(immigrants$years_in_us)

income_by_tenure = immigrants %>%
  mutate(tenure_group = case_when(
    years_in_us <= 5   ~ "0-5 years",
    years_in_us <= 10  ~ "6-10 years",
    years_in_us <= 20  ~ "11-20 years",
    years_in_us <= 30  ~ "21-30 years",
    TRUE               ~ "31+ years")) %>%
  mutate(tenure_group = factor(tenure_group, levels = c("0-5 years", "6-10 years", "11-20 years", "21-30 years", "31+ years"))) %>%
  group_by(immig_status, tenure_group) %>%
  summarise(
    mean_inctot = weighted.mean(inctot_2024usd, perwt, na.rm = TRUE),
    median_inctot = matrixStats::weightedMedian(inctot_2024usd, w = perwt, na.rm = TRUE),
    n = n(),
    .groups = "drop")

print(income_by_tenure, n = Inf)

income_by_tenure %>% select(immig_status, tenure_group, n) %>% print(n = Inf)

ggplot(income_by_tenure, aes(x = tenure_group, y = mean_inctot, color = immig_status, group = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-3, suffix = "K")) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal noncitizen"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  labs(
    title = "Personal income by years since immigration",
    subtitle = "Mean personal income (2024 dollars); working-age adults 18-64, foreign-born only",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 28, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 16, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size = 16, color = "gray40"),
    axis.text.y = element_text(size = 16, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

ggsave("results/income_by_years_in_us.png", width = 15, height = 10)

income_by_5yr_bins = immigrants %>%
  mutate(tenure_bin = case_when(
    years_in_us < 5   ~ "0-4",
    years_in_us < 10  ~ "5-9",
    years_in_us < 15  ~ "10-14",
    years_in_us < 20  ~ "15-19",
    years_in_us < 25  ~ "20-24",
    years_in_us < 30  ~ "25-29",
    years_in_us < 35  ~ "30-34",
    years_in_us < 40  ~ "35-39",
    years_in_us < 45  ~ "40-44",
    years_in_us < 50  ~ "45-49",
    TRUE              ~ "50+")) %>%
  mutate(tenure_bin = factor(tenure_bin, levels = c(
    "0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50+"))) %>%
  group_by(immig_status, tenure_bin) %>%
  summarise(
    mean_inctot = weighted.mean(inctot_2024usd, perwt, na.rm = TRUE),
    median_inctot = matrixStats::weightedMedian(inctot_2024usd, w = perwt, na.rm = TRUE),
    n = n(),
    .groups = "drop")

print(income_by_5yr_bins, n = Inf)

ggplot(income_by_5yr_bins, aes(x = tenure_bin, y = mean_inctot, color = immig_status, group = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-3, suffix = "K"), limits = c(20000, 80000)) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal noncitizen"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  labs(
    title = "Personal income by years since immigration",
    subtitle = "Mean personal income (2024 dollars); working-age adults 18-64, foreign-born only; 5-year bins",
    x = "Years since immigration", y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 28, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 16, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size = 13, color = "gray40", angle = 0),
    axis.text.y = element_text(size = 14, color = "gray40"),
    axis.title.x = element_text(size = 14, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

ggsave("results/income_by_years_in_us_5yr_bins.png", width = 15, height = 10)

table(acsdata$age[acsdata$immig_status == "Legal noncitizen"])

income_by_5yr_bins %>% filter(immig_status == "Legal noncitizen") %>% select(tenure_bin, n, mean_inctot)
