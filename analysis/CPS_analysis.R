## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats, gganimate, gifski)

# Set working directory
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Immigrant Health Coverage 2010-2024")

# CPS data ------------------------------------------------------------------
cpsdata = fread("data/output/cpsdata.csv")

# immigrant status counts
CPS_immig_counts = cpsdata %>%
  group_by(year, immig_status) %>%
  summarise(
    n = n(),
    population = sum(asecwt, na.rm = TRUE)) %>%
  ungroup()

write_csv(CPS_immig_counts, "results/CPS_immig_counts_year.csv")

colors = c(
  "Native-born"         = "#3043B4",
  "Naturalized citizen" = "#0D0E51",
  "Legal immigrant"     = "#7C756D",
  "Undocumented"        = "#C97703")

CPS_population = ggplot(CPS_immig_counts, aes(x = as.numeric(year), y = population / 1e6, color = immig_status)) +
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
    subtitle = "CPS; Ages 0-97",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: CPS ASEC via IPUMS") +
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

ggsave("results/CPS_population.png", width = 15, height = 10)

# Health Coverage
cps_coverage_counts = cpsdata %>%
  mutate(
    coverage_type = case_when(
        esi == 2 ~ "Employer-sponsored",
        dpcovly == 2 ~ "Direct purchase",
        himcaidly == 2 ~ "Medicaid",
        himcarely == 2 ~ "Medicare",
        hichamp == 2 | champvaly == 2 | inhcovly == 2 ~ "Other public",
        TRUE ~ "Uninsured")) %>%
  group_by(year, immig_status, coverage_type) %>%
  summarise(
    n = n(),
    population = sum(asecwt, na.rm = TRUE)) %>%
  ungroup()

write_csv(cps_coverage_counts, "results/cps_coverage_counts_year.csv") 

# all four immigration-status groups, 2024
cps_coverage2025 = cps_coverage_counts %>%
  filter(year == 2025) %>%
  group_by(immig_status) %>%
  mutate(rate = population / sum(population)) %>%
  ungroup()

ggplot(cps_coverage2025, aes(x = immig_status, y = rate, fill = coverage_type)) +
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
    title = "Health Insurance Coverage Type by Immigration Status (2025)",
    subtitle = "CPS-ASEC",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "Source: CPS ASEC via IPUMS") +
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

ggsave("results/CPS_coverage_2025.png", width = 10, height = 6)

# native-born vs. legal immigrant vs. undocumented, 2024
cps_coverage_2025_grouped = cps_coverage_counts %>%
  filter(year == 2025) %>%
  mutate(
    immig_status = as.character(immig_status),
    group = case_when(
      immig_status == "Native-born"                                 ~ "Native-born",
      immig_status %in% c("Naturalized citizen", "Legal immigrant") ~ "Legal immigrant",
      immig_status == "Undocumented"                                ~ "Undocumented")) %>%
  group_by(group, coverage_type) %>%
  summarise(population = sum(population), .groups = "drop") %>%
  group_by(group) %>%
  mutate(rate = population / sum(population)) %>%
  ungroup() %>%
  mutate(group = factor(group, levels = c("Native-born", "Legal immigrant", "Undocumented")))

ggplot(cps_coverage_2025_grouped, aes(x = group, y = rate, fill = coverage_type)) +
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
    title = "Health Insurance Coverage by Immigration Status (2025)",
    subtitle = "CPS-ASEC",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "Source: CPS ASEC via IPUMS") +
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

ggsave("results/CPS_coverage_2025_grouped.png", width = 8, height = 6)

# native-born vs. all immigrants, 2024
cps_coverage_2025_COMBINED = cps_coverage_counts %>%
  filter(year == 2025) %>%
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

write_csv(cps_coverage_2025_COMBINED, "results/cps_coverage_2025_COMBINED.csv")

ggplot(cps_coverage_2025_COMBINED, aes(x = group, y = rate, fill = coverage_type)) +
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
    title = "Health Insurance Coverage by Immigration Status (2025)",
    subtitle = "CPS-ASEC",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "Source: CPS ASEC via IPUMS") +
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

ggsave("results/CPS_coverage_2025_COMBINED.png", width = 8, height = 6)

# Uninsured rates
cpsdata = cpsdata %>%
  mutate(uninsured = ifelse(
    (esi != 2       | is.na(esi))       &
    (dpcovly != 2   | is.na(dpcovly))   &
    (himcaidly != 2 | is.na(himcaidly)) &
    (himcarely != 2 | is.na(himcarely)) &
    (hichamp != 2   | is.na(hichamp))   &
    (champvaly != 2 | is.na(champvaly)) &
    (inhcovly != 2  | is.na(inhcovly)),
    1, 0))

cps_uninsured_trend = cpsdata %>%
  mutate(uninsured_wt = ifelse(uninsured == 1, asecwt, 0)) %>%
  group_by(year, immig_status) %>%
  summarise(
    total_pop   = sum(asecwt, na.rm = TRUE),
    uninsured   = sum(uninsured_wt, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(uninsured_rate = uninsured / total_pop)

ggplot(cps_uninsured_trend, aes(x = as.numeric(year), y = uninsured_rate, color = immig_status)) +
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
    title = "Uninsured Rate by Immigration Status (2010–2025)",
    subtitle = "CPS-ASEC",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: CPS-ASEC via IPUMS") +
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

ggsave("results/CPS_uninsured_trend.png", width = 10, height = 6)

cps_uninsured2 = cpsdata %>%
  mutate(
    group = ifelse(as.character(immig_status) == "Native-born", "Native-born", "Immigrants"),
    uninsured_wt = ifelse(uninsured == 1, asecwt, 0)) %>%
  group_by(year, group) %>%
  summarise(total_pop = sum(asecwt, na.rm = TRUE),
            uninsured = sum(uninsured_wt, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(uninsured_rate = uninsured / total_pop)

ggplot(cps_uninsured2, aes(x = as.numeric(year), y = uninsured_rate, color = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Native-born" = "#3043B4",
    "Immigrants"  = "#C97703")) +
  scale_x_continuous(breaks = seq(2010, 2025, by = 3), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.05),
    expand = c(0.02, 0)) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = 2014.1, y = 0.20, label = "ACA (2014)",
           hjust = 0, size = 3, color = "gray50") +
  labs(
    title = "Uninsured Rate: Native-Born vs All Immigrants (2010–2025)",
    subtitle = "CPS-ASEC",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: CPS-ASEC via IPUMS") +
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

ggsave("results/CPS_uninsured2.png", width = 10, height = 6)

# Medicaid enrollment rates
cps_medicaid_trend = cpsdata %>%
  mutate(medicaid = ifelse(himcaidly == 2, asecwt, 0)) %>%
  group_by(year, immig_status) %>%
  summarise(
    total_pop = sum(asecwt, na.rm = TRUE),
    medicaid  = sum(medicaid, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(medicaid_rate = medicaid / total_pop)

ggplot(cps_medicaid_trend, aes(x = as.numeric(year), y = medicaid_rate, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703"
  )) +
  scale_x_continuous(breaks = seq(2010, 2025, by = 3), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.05),
    limits = c(0, 0.5),
    expand = c(0.02, 0)) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = 2014, y = 0.45, label = "ACA (2014)",
           hjust = 0, size = 3, color = "gray50") +
  labs(
    title = "Medicaid Coverage Rate by Immigration Status (2010–2025)",
    subtitle = "CPS-ASEC",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: CPS-ASEC via IPUMS") +
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

ggsave("results/CPS_medicaid_trend.png", width = 10, height = 6)

cps_medicaid2 = cpsdata %>%
  mutate(
    group    = ifelse(as.character(immig_status) == "Native-born", "Native-born", "Immigrants"),
    medicaid = ifelse(himcaidly == 2, asecwt, 0)) %>%
  group_by(year, group) %>%
  summarise(
    total_pop = sum(asecwt, na.rm = TRUE),
    medicaid  = sum(medicaid, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(medicaid_rate = medicaid / total_pop)

ggplot(cps_medicaid2, aes(x = as.numeric(year), y = medicaid_rate, color = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Native-born" = "#3043B4",
    "Immigrants"  = "#C97703")) +
  scale_x_continuous(breaks = seq(2010, 2025, by = 3), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.05),
    expand = c(0.02, 0)) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = 2014.1, y = 0.19, label = "ACA (2014)",
           hjust = 0, size = 3, color = "gray50") +
  labs(
    title = "Medicaid Rate: Native-Born vs All Immigrants (2010–2025)",
    subtitle = "CPS-ASEC",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: CPS-ASEC via IPUMS") +
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

ggsave("results/CPS_medicaid2.png", width = 10, height = 6)

# ESI
cps_esi_trend = cpsdata %>%
  mutate(esi_wt = ifelse(esi == 2, asecwt, 0)) %>%
  group_by(year, immig_status) %>%
  summarise(
    total_pop = sum(asecwt, na.rm = TRUE),
    esi       = sum(esi_wt, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(esi_rate = esi / total_pop)

CPS_esi_trend = ggplot(cps_esi_trend, aes(x = as.numeric(year), y = esi_rate, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  scale_x_continuous(breaks = seq(2010, 2025, by = 3), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.05),
    limit = c(0.25, 0.6),
    expand = c(0.02, 0)) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = 2014, y = 0.45, label = "ACA (2014)",
           hjust = 0, size = 3, color = "gray50") +
  labs(
    title = "Employer-Sponsored Coverage Rate by Immigration Status (2010–2025)",
    subtitle = "CPS-ASEC",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: CPS-ASEC via IPUMS") +
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

ggsave("results/CPS_esi_trend.png", CPS_esi_trend, width = 10, height = 6)

# Age
cps_medicaid_age_trend = cpsdata %>%
  filter(himcaidly == 2) %>%
  group_by(year, immig_status) %>%
  summarise(median_age = weightedMedian(age, w = asecwt, na.rm = TRUE),
  .groups = "drop")

CPS_medicaid_age_trend = ggplot(cps_medicaid_age_trend, aes(x = as.numeric(year), y = median_age, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  scale_x_continuous(breaks = seq(2010, 2025, by = 3), expand = c(0.02, 0)) +
  scale_y_continuous(breaks = seq(0, 80, by = 5), expand = c(0.02, 0), limits = c(0, 60)) +
  labs(
    title = "Median Age of Medicaid Enrollees by Immigration Status (2010–2025)",
    subtitle = "CPS-ASEC",
    x = NULL,
    y = "Median age",
    color = NULL,
    caption = "Source: CPS-ASEC via IPUMS, authors' calculations") +
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

ggsave("results/CPS_medicaid_age_trend.png", CPS_medicaid_age_trend, width = 10, height = 6)

# removing california
# removing california
cps_noca = cpsdata %>%
  filter(statefip != 6) %>%
  group_by(year, immig_status) %>%
  summarise(median_age = weightedMedian(age, w = asecwt, na.rm = TRUE), 
    .groups = "drop")

ggplot(cps_noca, aes(x = as.numeric(year), y = median_age, color = immig_status)) +
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
    title = "Median Age of Medicaid Enrollees by Immigration Status (2010–2025)",
    subtitle = "CPS; excluding California",
    x = NULL,
    y = "Median age",
    color = NULL,
    caption = "Source: CPS-ASEC via IPUMS") +
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

ggsave("results/cps_NOCA_medicaid_age_trend.png", width = 10, height = 6)

# age density gif
cps_age_medicaid_all4 = cpsdata %>%
  filter(himcaidly == 2)

CPS_age_density_gif4 = ggplot(cps_age_medicaid_all4, aes(x = age, fill = immig_status, color = immig_status, weight = asecwt)) +
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
    title = "Age Distribution of Medicaid Enrollees — {closest_state}",
    subtitle = "CPS-ASEC",
    x = "Age",
    y = NULL,
    fill = NULL,
    caption = "Source: CPS-ASEC via IPUMS") +
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
    panel.background = element_rect(fill = "white", color = NA)) +
  transition_states(year, transition_length = 2, state_length = 1) +
  ease_aes("cubic-in-out")

animate(CPS_age_density_gif4,
        nframes   = 75,
        fps       = 20,
        width     = 1000,
        height    = 600,
        renderer  = gifski_renderer("results/CPS_age_density_4groups.gif"))

# medicaid rate by age, over time gif
cps_medicaid_age = cpsdata %>%
    mutate(medicaid = ifelse(himcaidly == 2, asecwt, 0)) %>%
    group_by(year, immig_status, age) %>%
    summarise(
        total_pop = sum(asecwt, na.rm = TRUE),
        medicaid = sum(medicaid, na.rm = TRUE),
        .groups = "drop") %>%
    mutate(medicaid_rate = medicaid / total_pop)

write_csv(cps_medicaid_age, "results/CPS_medicaid_age.csv")

CPS_medicaid_age_gif <- ggplot(cps_medicaid_age,
                                aes(x = age, y = medicaid_rate, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  scale_x_continuous(breaks = seq(0, 100, by = 10), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.10),
    expand = c(0.02, 0)) +
  geom_vline(xintercept = 18, linetype = "dashed", color = "gray70", linewidth = 0.5) +
  geom_vline(xintercept = 65, linetype = "dashed", color = "gray70", linewidth = 0.5) +
  annotate("text", x = 18.5, y = 0.95, label = "Age 18",
           hjust = 0, size = 3, color = "gray50") +
  annotate("text", x = 65.5, y = 0.95, label = "Age 65",
           hjust = 0, size = 3, color = "gray50") +
  labs(
    title = "Medicaid Rate by Age and Immigration Status — {closest_state}",
    subtitle = "CPS-ASEC",
    x = "Age",
    y = NULL,
    color = NULL,
    caption = "Source: CPS-ASEC via IPUMS, authors' calculations") +
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
    panel.background = element_rect(fill = "white", color = NA)) +
  transition_states(year, transition_length = 2, state_length = 1) +
  ease_aes("cubic-in-out")

animate(CPS_medicaid_age_gif,
        nframes   = 75,
        fps       = 10,
        width     = 1000,
        height    = 600,
        res       = 100,
        renderer  = gifski_renderer("results/CPS_medicaid_age_animated.gif"))

# why no age 65+ undocumented on medicaid? 

cpsdata = cpsdata %>%
    mutate(coverage_type = case_when(
        esi == 2 ~ "Employer-sponsored",
        dpcovly == 2 ~ "Direct purchase",
        himcaidly == 2 ~ "Medicaid",
        himcarely == 2 ~ "Medicare",
        hichamp == 2 | champvaly == 2 | inhcovly == 2 ~ "Other public",
        TRUE ~ "Uninsured"))

cpsdata %>%
    filter(immig_status == "Undocumented", age >= 65) %>%
    group_by(year, coverage_type) %>%
    summarise(n = n(), pop = sum(asecwt, na.rm = TRUE)) %>%
    print(n = Inf)

cps %>%
  filter(age >= 65, statefip == 6, himcaidly == 2, immigrant == 1) %>%
  group_by(year) %>%
  summarise(n = n(), 
            n_undoc = sum(immig_status == "Undocumented"),
            n_legal = sum(immig_status %in% c("Legal immigrant", "Naturalized citizen")))

cps %>%
  filter(age >= 65, immigrant == 1) %>%
  mutate(reclass_path = as.character(case_when(
    citizen == 2 ~ "naturalized",
    yrimmig < 1982 ~ "pre-1982",
    incss > 0 & incss < 99999 ~ "social_security",
    incssi > 0 & incssi < 99999 ~ "ssi",
    incwelfr > 0 & incwelfr < 99999 ~ "welfare",
    himcarely == 2 ~ "medicare",
    himcaidly == 2 ~ "medicaid_pathway",
    vetstat == 2 ~ "veteran",
    classwkr == 26 ~ "armed_forces",
    bpl == 25000 & yrimmig < 2017 ~ "cuba",
    TRUE ~ "household_imputation_or_undocumented"
  ))) %>%
  dplyr::count(reclass_path)

medicaid_65plus = cps %>%
  filter(age >= 65, immigrant == 1) %>%
  mutate(reclass_path = as.character(case_when(
    citizen == 2 ~ "naturalized",
    yrimmig < 1982 ~ "pre-1982",
    incss > 0 & incss < 99999 ~ "social_security",
    incssi > 0 & incssi < 99999 ~ "ssi",
    incwelfr > 0 & incwelfr < 99999 ~ "welfare",
    himcarely == 2 ~ "medicare",
    himcaidly == 2 ~ "medicaid_pathway",
    vetstat == 2 ~ "veteran",
    classwkr == 26 ~ "armed_forces",
    bpl == 25000 & yrimmig < 2017 ~ "cuba",
    TRUE ~ "household_imputation_or_undocumented"
  ))) %>%
  filter(reclass_path == "medicaid_pathway")

medicaid_65plus %>%
  select(year, statefip, age, immig_status, relate, marst) %>%
  arrange(year, statefip) %>%
  print(n = Inf)

cps %>%
  filter(age >= 65, immigrant == 1) %>%
  mutate(reclass_path = as.character(case_when(
    citizen == 2 ~ "naturalized",
    yrimmig < 1982 ~ "pre-1982",
    incss > 0 & incss < 99999 ~ "social_security",
    incssi > 0 & incssi < 99999 ~ "ssi",
    incwelfr > 0 & incwelfr < 99999 ~ "welfare",
    himcarely == 2 ~ "medicare",
    himcaidly == 2 ~ "medicaid_pathway",
    vetstat == 2 ~ "veteran",
    classwkr == 26 ~ "armed_forces",
    bpl == 25000 & yrimmig < 2017 ~ "cuba",
    TRUE ~ "reached_imputation_step"
  ))) %>%
  filter(reclass_path == "reached_imputation_step") %>%
  mutate(immig_status_chr = as.character(immig_status)) %>%
  dplyr::count(immig_status_chr)

medicaid_65plus = cps %>%
  filter(age >= 65, immigrant == 1) %>%
  mutate(reclass_path = as.character(case_when(
    citizen == 2 ~ "naturalized",
    yrimmig < 1982 ~ "pre-1982",
    incss > 0 & incss < 99999 ~ "social_security",
    incssi > 0 & incssi < 99999 ~ "ssi",
    incwelfr > 0 & incwelfr < 99999 ~ "welfare",
    himcarely == 2 ~ "medicare",
    himcaidly == 2 ~ "medicaid_pathway",
    vetstat == 2 ~ "veteran",
    classwkr == 26 ~ "armed_forces",
    bpl == 25000 & yrimmig < 2017 ~ "cuba",
    TRUE ~ "reached_imputation_step"
  ))) %>%
  filter(reclass_path == "medicaid_pathway")

medicaid_65plus %>%
  mutate(
    statefip_flat = as.integer(as.character(statefip)),
    year_flat = as.integer(as.character(year))
  ) %>%
  group_by(statefip_flat, year_flat) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(statefip_flat, year_flat) %>%
  print(n = Inf)

medicaid_65plus %>%
  select(year, statefip, age, relate, marst, immig_status) %>%
  arrange(year, statefip) %>%
  print(n = Inf)


# State expansions
cpsdata = cpsdata %>%
  mutate(expansion_state = case_when(
    # California — phased expansion
    statefip == 6  & year >= 2016 & age <= 18                        ~ 1,
    statefip == 6  & year >= 2020 & age <= 25                        ~ 1,
    statefip == 6  & year >= 2022 & age >= 50                        ~ 1,
    statefip == 6  & year >= 2024 & age >= 26 & age <= 49            ~ 1,
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

undoc_expansion_cps = cpsdata %>%
  filter(immig_status == "Undocumented")

uninsured_expansion_cps = undoc_expansion_cps %>%
  mutate(uninsured_wt = ifelse(uninsured == 1, asecwt, 0)) %>%
  group_by(year, expansion_label) %>%
  summarise(total_pop = sum(asecwt, na.rm = TRUE),
            uninsured = sum(uninsured_wt, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(uninsured_rate = uninsured / total_pop)

CPS_undoc_uninsured_expansion = ggplot(uninsured_expansion_cps,
  aes(x = as.numeric(year), y = uninsured_rate,
        color = expansion_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Expansion state"     = "#C97703",
    "Non-expansion state" = "#3043B4")) +
  scale_x_continuous(breaks = seq(2010, 2025, by = 3), expand = c(0.02, 0)) +
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
    title = "Uninsured Rate for Undocumented Immigrants (2010–2025)",
    subtitle = "CPS-ASEC; expansion states: California, Oregon, Illinois, New York, Colorado, Washington, Minnesota, DC",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: CPS-ASEC via IPUMS, authors' calculations") +
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

ggsave("results/CPS_undoc_uninsured_expansion.png", width = 10, height = 6)

medicaid_expansion_cps = undoc_expansion_cps %>%
  mutate(medicaid = ifelse(himcaidly == 2, asecwt, 0)) %>%
  group_by(year, expansion_label) %>%
  summarise(total_pop = sum(asecwt, na.rm = TRUE),
            medicaid = sum(medicaid, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(medicaid_rate = medicaid / total_pop)

CPS_undoc_medicaid_expansion = ggplot(medicaid_expansion_cps,
  aes(x = as.numeric(year), y = medicaid_rate, color = expansion_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Expansion state"     = "#C97703",
    "Non-expansion state" = "#3043B4")) +
  scale_x_continuous(breaks = seq(2010, 2025, by = 3), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.05),
    limits = c(0, 0.40),
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
    title = "Medicaid Enrollment Rate for Undocumented Immigrants (2010–2025)",
    subtitle = "CPS-ASEC; expansion states: California, Oregon, Illinois, New York, Colorado, Washington, Minnesota, DC",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: CPS-ASEC via IPUMS, authors' calculations") +
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

ggsave("results/CPS_undoc_medicaid_expansion.png", width = 10, height = 6)

# california 
cps_ca = cpsdata %>%
  filter(statefip == 6)

ca_immig_counts_cps = cps_ca %>%
  group_by(year, immig_status) %>%
  summarise(
    n = n(), population = sum(asecwt, na.rm = TRUE), .groups = "drop")

ggplot(ca_immig_counts_cps, aes(x = as.numeric(year), y = population / 1e6, color = immig_status)) +
  geom_line(linewidth = 1.8) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(2010, 2025, by = 3), expand = c(0.02, 0)) +
  scale_y_continuous(
    breaks = seq(0, 30, by = 5),
    labels = function(x) paste0(x, "M"),
    limits = c(0, 30),
    expand = c(0.02, 0)) +
  labs(
    title = "California Population by Immigration Status (2010-2025)",
    subtitle = "CPS-ASEC",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: CPS-ASEC via IPUMS") +
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

ggsave("results/CPS_CA_population.png", width = 15, height = 10)

ca_uninsured_cps = cps_ca %>%
  mutate(uninsured_wt = ifelse(uninsured == 1, asecwt, 0)) %>%
  group_by(year, immig_status) %>%
  summarise(
    total_pop      = sum(asecwt, na.rm = TRUE),
    uninsured      = sum(uninsured_wt, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(uninsured_rate = uninsured / total_pop)

CPS_CA_uninsured = ggplot(ca_uninsured_cps,
                            aes(x = as.numeric(year), y = uninsured_rate, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  scale_x_continuous(breaks = seq(2010, 2025, by = 3), expand = c(0.02, 0)) +
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
    title = "Uninsured Rate by Immigration Status — California (2010–2025)",
    subtitle = "CPS-ASEC",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: CPS-ASEC via IPUMS") +
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

ggsave("results/CPS_CA_uninsured.png", width = 10, height = 6)

ca_medicaid_cps = cps_ca %>%
  mutate(medicaid = ifelse(himcaidly == 2, asecwt, 0)) %>%
  group_by(year, immig_status) %>%
  summarise(
    total_pop     = sum(asecwt, na.rm = TRUE),
    medicaid      = sum(medicaid, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(medicaid_rate = medicaid / total_pop)

CPS_CA_medicaid = ggplot(ca_medicaid_cps,
                           aes(x = as.numeric(year), y = medicaid_rate, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  scale_x_continuous(breaks = seq(2010, 2025, by = 3), expand = c(0.02, 0)) +
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
    title = "Medicaid Rate by Immigration Status — California (2010–2025)",
    subtitle = "CPS-ASEC",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: CPS-ASEC via IPUMS") +
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

ggsave("results/CPS_CA_medicaid.png", width = 10, height = 6)

ca_medicaid_age_trend_cps = cpsdata %>%
  filter(himcaidly == 2, statefip == 6) %>%
  group_by(year, immig_status) %>%
  summarise(median_age = weightedMedian(age, w = asecwt, na.rm = TRUE),
    .groups = "drop")

CPS_CA_medicaid_age_trend = ggplot(ca_medicaid_age_trend_cps, aes(x = as.numeric(year), y = median_age, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  scale_x_continuous(breaks = seq(2010, 2025, by = 3), expand = c(0.02, 0)) +
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
    title = "Median Age of Medicaid Enrollees by Immigration Status — California (2010–2025)",
    subtitle = "CPS-ASEC",
    x = NULL,
    y = "Median age",
    color = NULL,
    caption = "Source: CPS-ASEC via IPUMS") +
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

ggsave("results/CPS_CA_medicaid_age_trend.png", CPS_CA_medicaid_age_trend, width = 10, height = 6)
