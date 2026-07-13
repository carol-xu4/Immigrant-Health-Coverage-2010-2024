## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats)

# Set working directory
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Immigrant Health Coverage 2010-2024")

# ACS data ------------------------------------------------------------------
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
