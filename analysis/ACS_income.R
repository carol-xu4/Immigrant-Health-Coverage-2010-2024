# Preliminaries ------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory 
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Immigrant Health Coverage 2010-2024")

# cleaning ------------------------------------------------------------------
ddi_acs_inc = read_ipums_ddi("data/input/usa_00008.xml")
acs = read_ipums_micro(ddi_acs_inc)

acs = acs %>%
    rename_with(tolower)

acsinc = acs %>%
  select(
    year, serial, pernum, cluster, strata, perwt, statefip,
    sex, age, race, hispan, marst,
    citizen, yrimmig, bpl, bpld, vetstat,
    incss, incwelfr, incsupp,
    classwkr, classwkrd, relate, related,
    hcovany, hcovpriv, hinsemp, hinspur,
    hcovpub, hinscaid, hinscare, hinsva, hinstri,
    momloc, momloc2, poploc, poploc2,
    inctot, ftotinc, poverty, cpi99)

# residual method 
acsinc = acsinc %>%
  mutate(immigrant = ifelse(citizen == 2 |
                            citizen == 3, 1, 0)) %>%
  mutate(immig_status = ifelse(bpl < 150 | immigrant == 0 | citizen == 1, 1, NA)) %>%
  mutate(foreign_born = ifelse(bpl >= 150, 1, 0)) %>%
  mutate(immig_status = case_when(
    immigrant == 1 & citizen == 2 ~ 2,
    immigrant == 1 & yrimmig < 1982 ~ 2,
    immigrant == 1 & incss > 0 & incss < 99999 ~ 2,
    immigrant == 1 & incsupp > 0 & incsupp < 99999 ~ 2,
    immigrant == 1 & incwelfr > 0 & incwelfr < 99999 ~ 2,
    immigrant == 1 & hinscare == 2 ~ 2,
    immigrant == 1 & hinscaid == 2 &
      # california — phased Medi-Cal expansion
      !(statefip == 6 & ((year >= 2016 & age <= 18) | (year >= 2020 & age <= 25) | (year >= 2022 & age >= 50) | (year >= 2024 & age >= 26 & age <= 49))) &
      # illinois — All Kids children 2006, HBIS seniors 2020, HBIA adults 42-64 2022
      !(statefip == 17 & ((year >= 2006 & age <= 18) | (year >= 2020 & age >= 65) | (year >= 2022 & age >= 42 & age <= 64))) &
      # washington — Apple Health for Kids children 2007 only (adult expansion is marketplace)
      !(statefip == 53 & year >= 2007 & age <= 18) &
      # new york — Child Health Plus children 2014, adults 65+ 2024
      !(statefip == 36 & ((year >= 2014 & age <= 18) | (year >= 2024 & age >= 65))) &
      # oregon — Cover All Kids children 2018, phase 1 ages 19-25/55+ 2022, full expansion 2023
      !(statefip == 41 & ((year >= 2018 & age <= 18) | (year == 2022 & (age <= 25 | age >= 55)) | (year >= 2023))) &
      # new jersey — children 2018
      !(statefip == 34 & year >= 2018 & age <= 18) &
      # connecticut — children under 15, 2010
      !(statefip == 9  & year >= 2010 & age <= 14) &
      # rhode island — children 2022
      !(statefip == 44 & year >= 2022 & age <= 18) &
      # maine — children 2022
      !(statefip == 23 & year >= 2022 & age <= 18) &
      # vermont — children 2022
      !(statefip == 50 & year >= 2022 & age <= 18) ~ 2,
    immigrant == 1 & vetstat == 2 ~ 2,
    immigrant == 1 & classwkrd == 26 ~ 2,
    immigrant == 1 & bpld == 25000 & yrimmig < 2017 ~ 2,
    TRUE ~ immig_status
  )) %>%
  mutate(legal = ifelse(
    immig_status == 1 | immig_status == 2, 1, 0
  )) %>%
  mutate(good = if_else(relate == 2 & immigrant == 1, 1, NA_real_)) %>%
  mutate(legal = ifelse(is.na(legal), 0, legal)) %>%
  group_by(year, serial) %>%
  mutate(slegal = mean(good * legal, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(immig_status = ifelse((slegal > 0 | is.na(slegal)) & immigrant == 1 & relate == 1 & marst == 1,
                               2,
                               immig_status)) %>%
  mutate(good1 = ifelse(relate == 1 & immigrant == 1, 1, NA_real_)) %>%
  group_by(year, serial) %>%
  mutate(hlegal = mean(good1 * legal, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(immig_status =
           ifelse((hlegal > 0 | is.na(hlegal)) &
                    immigrant == 1 & ((relate == 2 & marst == 1) | relate == 3 | relate == 9),
                  2,
                  immig_status
           )) %>%
  mutate(immig_status = ifelse(is.na(immig_status), 3, immig_status)) %>%
  mutate(undercount =
           ifelse(immig_status == 3,
                  1 + (0.13) * (0.925) ^ (year - yrimmig), 0)) %>%
  mutate(perwt = ifelse(undercount > 0, perwt * undercount, perwt)) %>%
  mutate(immig_status = case_when(
    immig_status == 1                          ~ "Native-born",
    immig_status == 2 & citizen == 2           ~ "Naturalized citizen",
    immig_status == 2 & citizen != 2           ~ "Legal immigrant",
    immig_status == 3                          ~ "Undocumented",
  )) %>%
  mutate(immig_status = factor(immig_status,
                               levels = c("Native-born",
                                          "Naturalized citizen",
                                          "Legal immigrant",
                                          "Undocumented")))

# rewrite final ACS dataset
fwrite(acsinc, "data/output/acsinc.csv")

# analysis -------------------------------------------------------------------
acsinc = fread("data/output/acsinc.csv")

## adjusting income variables for inflation
# remove
acsinc = acsinc %>%
  mutate(
    inctot   = ifelse(inctot %in% c(9999999, 9999998), NA, inctot),
    ftotinc  = ifelse(ftotinc == 9999999, NA, ftotinc),
    poverty  = ifelse(poverty == 0, NA, poverty))

# 2024 factor from CPI99 = 0.531
table(acsinc$year, acsinc$cpi99)

cpi_2024 = acsinc$cpi99[acsinc$year == 2024][1]

cpi_2024

acsinc = acsinc %>%
  mutate(
    ftotinc_2024usd  = ftotinc  * cpi99 / cpi_2024,
    inctot_2024usd   = inctot   * cpi99 / cpi_2024)

# family income (pre-tax), by immig status (duplicates within families removed)
ftotinc_by_family = acsinc %>%
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
    scale_y_continuous(labels = scales::label_dollar(scale = 1e-3, suffix = "K")) +
    scale_x_continuous(breaks = unique(ftotinc_by_family$year)[c(TRUE, FALSE)]) +
    scale_color_manual(values = c(
        "Native-born"         = "#3043B4",
        "Naturalized citizen" = "#0D0E51",
        "Legal immigrant"     = "#7C756D",
        "Undocumented"        = "#C97703")) +
    labs(
        title = "Total family income, by immigration status",
        subtitle = "Mean family income (2024 dollars); one person per family",
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
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  labs(
    title = "Total family income, by immigration status",
    subtitle = "Median family income (2024 dollars); one person per family",
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
acsinc_adults = acsinc %>%
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
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  labs(
    title = "Personal income, by immigration status",
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
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  labs(
    title = "Personal income, by immigration status",
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
acspov = acsinc %>%
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
    "Legal immigrant"     = "#7C756D",
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
