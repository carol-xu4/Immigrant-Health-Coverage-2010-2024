if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Immigrant Health Coverage 2010-2024")

acs3 = fread("data/output/acsdata3.csv") %>% select(-good, -slegal, -good1, -hlegal)

colors = c(
  "Native-born citizens"= "#3043B4",
  "Legal immigrants"    = "#7C756D",
  "Illegal immigrants"  = "#C97703")
  
# immig status by year
immig_counts3 = acs3 %>%
  group_by(year, immig_status) %>%
  summarise(
    n = n(),
    population = sum(perwt, na.rm = TRUE)) %>%
  ungroup()

write_csv(immig_counts3, "results/3immig_counts_year.csv")

# health coverage by immig status, year
coverage_counts = acs3 %>%
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

coverage2024 = coverage_counts %>%
  filter(year == 2024) %>%
  group_by(immig_status) %>%
  mutate(rate = population / sum(population)) %>%
  ungroup()

ggplot(coverage2024, aes(x = immig_status, y = rate, fill = coverage_type)) +
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

ggsave("results/3ACS_coverage_2024.png", width = 10, height = 6)

medicaid_trend = acs3 %>%
  mutate(medicaid = ifelse(hinscaid == 2, perwt, 0)) %>%
  group_by(year, immig_status) %>%
  summarise(
    total_pop    = sum(perwt, na.rm = TRUE),
    medicaid     = sum(medicaid, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(medicaid_rate = medicaid / total_pop)

ggplot(medicaid_trend, aes(x = as.numeric(year), y = medicaid_rate, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Native-born citizens" = "#3043B4",
    "Legal immigrants"     = "#7C756D",
    "Illegal immigrants"   = "#C97703"
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

ggsave("results/3medicaid_rate.png", width = 10, height = 6)

medicaid_age_2024 = acs3 %>%
    filter(year == 2024) %>%
    mutate(medicaid = ifelse(hinscaid == 2, perwt, 0)) %>%
    group_by(immig_status, age) %>%
    summarise(
        total_pop = sum(perwt, na.rm = TRUE),
        medicaid = sum(medicaid, na.rm = TRUE),
        .groups = "drop") %>%
    mutate(medicaid_rate = medicaid / total_pop)

ggplot(medicaid_age_2024,
       aes(x = age, y = medicaid_rate, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c(
    "Native-born citizens"         = "#3043B4",
    "Legal immigrants"    = "#7C756D",
    "Illegal immigrants"        = "#C97703")) +
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
    title = "Medicaid Rate by Age and Immigration Status — 2024",
    subtitle = "ACS",
    x = "Age",
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

ggsave("results/3medicaid_age_2024.png", width = 10, height = 6)

# income
acs3 = acs3 %>%
  mutate(
    inctot   = ifelse(inctot %in% c(9999999, 9999998), NA, inctot),
    ftotinc  = ifelse(ftotinc == 9999999, NA, ftotinc),
    poverty  = ifelse(poverty == 0, NA, poverty))

# 2024 factor from CPI99 = 0.531
cpi_2024 = acs3$cpi99[acs3$year == 2024][1]

cpi_2024

acs3 = acs3 %>%
  mutate(
    ftotinc_2024usd  = ftotinc  * cpi99 / cpi_2024,
    inctot_2024usd   = inctot   * cpi99 / cpi_2024)


# family income (pre-tax), by immig status (duplicates within families removed)
ftotinc_by_family = acs3 %>%
    filter(!is.na(ftotinc), pernum == 1) %>%
    group_by(year, immig_status) %>%
    summarise(
        mean_ftotinc = weighted.mean(ftotinc_2024usd, perwt, na.rm = TRUE),
        median_ftotinc = matrixStats::weightedMedian(ftotinc_2024usd, w = perwt, na.rm = TRUE),
        n = n(), .groups = "drop")

ggplot(ftotinc_by_family, aes(x = year, y = mean_ftotinc, color = immig_status)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    scale_y_continuous(labels = scales::label_dollar(scale = 1e-3, suffix = "K"), limits = c(60000, 130000), breaks = seq(60000, 130000, by = 10000)) +
    scale_x_continuous(breaks = unique(ftotinc_by_family$year)[c(TRUE, FALSE)]) +
    scale_color_manual(values = c(
        "Native-born citizens"         = "#3043B4",
        "Legal immigrants"     = "#7C756D",
        "Illegal immigrants"        = "#C97703")) +
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

ggsave("results/3mean_ftotinc.png", width = 15, height = 10)

ggplot(ftotinc_by_family, aes(x = year, y = median_ftotinc, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-3, suffix = "K")) +
  scale_x_continuous(breaks = unique(ftotinc_by_family$year)[c(TRUE, FALSE)]) +
  scale_color_manual(values = c(
     "Native-born citizens"         = "#3043B4",
        "Legal immigrants"     = "#7C756D",
        "Illegal immigrants"        = "#C97703")) +
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

ggsave("results/3median_ftotinc.png", width = 15, height = 10)

# poverty, duplicates within households removed
acspov = acs3 %>%
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
    "Native-born citizens"         = "#3043B4",
    "Legal immigrants"     = "#7C756D",
    "Illegal immigrants"        = "#C97703")) +
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

ggsave("results/3poverty_rate.png", width = 15, height = 10)

# disability rates, age 15+, non GQ
disab_nogq = acs3 %>%
  filter(gq %in% c(1, 2, 5)) %>%
  filter(age >= 15) %>%
  mutate(
    diffany = case_when(
  diffhear == 2 | diffeye == 2 | diffrem == 2 | 
    diffphys == 2 | diffcare == 2 | diffmob == 2 ~ 2,
  diffhear == 1 | diffeye == 1 | diffrem == 1 | 
    diffphys == 1 | diffcare == 1 | diffmob == 1 ~ 1,
  TRUE ~ NA_real_))

disabled_nogq = disab_nogq %>%
  mutate(disabled = diffany == 2) %>%
  group_by(year, immig_status) %>%
  summarise(
    disabled = sum(perwt[disabled], na.rm = TRUE),
    population = sum(perwt, na.rm = TRUE),
    pct = disabled / population * 100, 
    .groups = "drop")

print(disabled_nogq, n = Inf)

ggplot(disabled_nogq, aes(x = as.numeric(year), y = pct, color = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0.02, 0), limits = c(0, 20)) +
  labs(
    title = "Non-GQ Disability Rate by Immigration Status (2010-2024)",
    subtitle = "ACS; age 15+; \n Any difficulty: cognitive, ambulatory, independent living, self-care, vision, hearing",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS via IPUMS") +
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

ggsave("results/acs_disabled_nogq.png", width = 15, height = 10)
  
# disability rates, age 15+, GQ
disab_gq = acs3 %>%
  filter(gq %in% c(3, 4)) %>%
  filter(age >= 15) %>%
  mutate(
    diffany = case_when(
  diffhear == 2 | diffeye == 2 | diffrem == 2 | 
    diffphys == 2 | diffcare == 2 | diffmob == 2 ~ 2,
  diffhear == 1 | diffeye == 1 | diffrem == 1 | 
    diffphys == 1 | diffcare == 1 | diffmob == 1 ~ 1,
  TRUE ~ NA_real_))

disabled_gq = disab_gq %>%
  mutate(disabled = diffany == 2) %>%
  group_by(year, immig_status) %>%
  summarise(
    disabled = sum(perwt[disabled], na.rm = TRUE),
    population = sum(perwt, na.rm = TRUE),
    pct = disabled / population * 100, 
    .groups = "drop")

print(disabled_gq, n = Inf)

ggplot(disabled_gq, aes(x = as.numeric(year), y = pct, color = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0.02, 0), limits = c(0, 50)) +
  labs(
    title = "GQ Disability Rate by Immigration Status (2010-2024)",
    subtitle = "ACS; age 15+; \n Any difficulty: cognitive, ambulatory, independent living, self-care, vision, hearing",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS via IPUMS") +
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

ggsave("results/acs_disabled_gq.png", width = 15, height = 10)

# non gq, age, 2024
disab_nogq_2024 = acs3 %>%
  filter(gq %in% c(1, 2, 5), age >=15, year == 2024) %>%
  mutate(
    diffany = case_when(
      diffhear == 2 | diffeye == 2 | diffrem == 2 | 
        diffphys == 2 | diffcare == 2 | diffmob == 2 ~ 2,
      diffhear == 1 | diffeye == 1 | diffrem == 1 | 
        diffphys == 1 | diffcare == 1 | diffmob == 1 ~ 1,
      TRUE ~ NA_real_))

disabled_by_age_2024 = disab_nogq_2024 %>%
  mutate(disabled = diffany == 2) %>%
  group_by(age, immig_status) %>%
  summarise(
    disabled = sum(perwt[disabled], na.rm = TRUE),
    population = sum(perwt, na.rm = TRUE),
    pct = disabled / population * 100, 
    .groups = "drop")

print(disabled_by_age_2024, n = Inf)

ggplot(disabled_by_age_2024, aes(x = age, y = pct, color = immig_status)) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.8, span = 0.3) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(15, 100, by = 10), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0.02, 0)) +
  labs(
    title = "Non-GQ Disability Rate by Age and Immigration Status (2024)",
    subtitle = "ACS; age 15+; \n Any difficulty: cognitive, ambulatory, independent living, self-care, vision, hearing",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS via IPUMS") +
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

ggsave("results/acs_disabled_by_age_2024.png", width = 15, height = 10)

# disability by age, 65+ pooled
disabled_by_age_2024_65p = disab_nogq_2024 %>%
  mutate(disabled = diffany == 2,
         age_pooled = pmin(age, 65)) %>%
  group_by(age_pooled, immig_status) %>%
  summarise(
    disabled = sum(perwt[disabled], na.rm = TRUE),
    population = sum(perwt, na.rm = TRUE),
    pct = disabled / population * 100,
    .groups = "drop")

print(disabled_by_age_2024_65p, n = Inf)

ggplot(disabled_by_age_2024_65p, aes(x = age_pooled, y = pct, color = immig_status)) +
  geom_line(linewidth = 1.8) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(15, 100, by = 10), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0.02, 0)) +
  labs(
    title = "Non-GQ Disability Rate by Age and Immigration Status (2024)",
    subtitle = "ACS; age 15+; \n Any difficulty: cognitive, ambulatory, independent living, self-care, vision, hearing",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS via IPUMS") +
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

ggsave("results/acs_disabled_by_age_2024_65plus.png", width = 15, height = 10)

# smoothed
ggplot(disabled_by_age_2024_65p, aes(x = age_pooled, y = pct, color = immig_status)) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.8, span = 0.3) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(15, 100, by = 10), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0.02, 0), limits = c(0, 35)) +
  labs(
    title = "Non-GQ Disability Rate by Age and Immigration Status (2024)",
    subtitle = "ACS; age 15+; \n Any difficulty: cognitive, ambulatory, independent living, self-care, vision, hearing",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS via IPUMS") +
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

ggsave("results/acs_disabled_by_age_2024_65plus_loess.png", width = 15, height = 10)

# disability rates, all (no GQ filter), all ages (65+ pooled), 2024
    # diffrem, diffphys, and diffcare universe is persons age 5+, diffmob (independent living difficulty) universe is age 16+

disab_all_2024 = acs3 %>%
  filter(year == 2024) %>%
  mutate(
    diffany = case_when(
      diffhear == 2 | diffeye == 2 | diffrem == 2 | 
        diffphys == 2 | diffcare == 2 | diffmob == 2 ~ 2,
      diffhear == 1 | diffeye == 1 | diffrem == 1 | 
        diffphys == 1 | diffcare == 1 | diffmob == 1 ~ 1,
      TRUE ~ NA_real_))

disabled_all_by_age_2024 = disab_all_2024 %>%
  mutate(disabled = diffany == 2) %>%
  group_by(age, immig_status) %>%
  summarise(
    disabled = sum(perwt[disabled], na.rm = TRUE),
    population = sum(perwt, na.rm = TRUE),
    pct = disabled / population * 100, 
    .groups = "drop")

print(disabled_all_by_age_2024, n = Inf)

disabled_all_by_age_2024_65p = disab_all_2024 %>%
  mutate(disabled = diffany == 2,
         age_pooled = pmin(age, 65)) %>%
  group_by(age_pooled, immig_status) %>%
  summarise(
    disabled = sum(perwt[disabled], na.rm = TRUE),
    population = sum(perwt, na.rm = TRUE),
    pct = disabled / population * 100,
    .groups = "drop")

print(disabled_all_by_age_2024_65p, n = Inf)

ggplot(disabled_all_by_age_2024_65p, aes(x = age_pooled, y = pct, color = immig_status)) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.8, span = 0.3) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(0, 65, by = 10), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0.02, 0), limits = c(0, 35)) +
  labs(
    title = "Disability Rate by Age and Immigration Status, all persons (2024)",
    subtitle = "ACS; Age 65+ pooled \n Any difficulty: cognitive, ambulatory, independent living, self-care, vision, hearing",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS via IPUMS") +
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

ggsave("results/acs_disabled_all_by_age_2024_65plus.png", width = 15, height = 10)


# differences, all (2024)
print(disabled_all_by_age_2024_65p, n = Inf)

disab_rates_all_2024 = disab_all_2024 %>%
  mutate(disabled = diffany == 2) %>%
  group_by(immig_status) %>%
  summarise(
    disabled = sum(perwt[disabled], na.rm = TRUE),
    population = sum(perwt, na.rm = TRUE),
    pct = disabled / population * 100, 
    .groups = "drop")

print(disab_rates_all_2024)

disab_rates_by_age_2024 = disab_all_2024 %>%
  mutate(disabled = diffany == 2) %>%
  group_by(age, immig_status) %>%
  summarise(
    disabled = sum(perwt[disabled], na.rm = TRUE),
    population = sum(perwt, na.rm = TRUE),
    pct = disabled / population * 100,
    .groups = "drop"
  ) %>%
  select(age, immig_status, pct) %>%
  pivot_wider(names_from = immig_status, values_from = pct) %>%
  rename(
    illegal = `Illegal immigrants`,
    legal   = `Legal immigrants`,
    native  = `Native-born citizens`
  ) %>%
  mutate(
    diff_illegal_native = native - illegal,      # percentage-point gap
    diff_legal_native   = native - legal,        # percentage-point gap
    ratio_illegal_native = native / illegal,     
    ratio_legal_native   = native / legal
  )

print(disab_rates_by_age_2024, n = Inf, width = Inf)

# working age adults, 18-64
disab_rates_workingage_2024_wide = disab_all_2024 %>%
  filter(age >= 18, age <= 64) %>%
  mutate(disabled = diffany == 2) %>%
  group_by(immig_status) %>%
  summarise(
    disabled = sum(perwt[disabled], na.rm = TRUE),
    population = sum(perwt, na.rm = TRUE),
    pct = disabled / population * 100,
    .groups = "drop"
  ) %>%
  select(immig_status, pct) %>%
  pivot_wider(names_from = immig_status, values_from = pct) %>%
  rename(
    illegal = `Illegal immigrants`,
    legal   = `Legal immigrants`,
    native  = `Native-born citizens`
  ) %>%
  mutate(
    diff_illegal_native  = native - illegal,
    diff_legal_native    = native - legal,
    ratio_illegal_native = native / illegal,
    ratio_legal_native   = native / legal
  )

print(disab_rates_workingage_2024_wide, width = Inf)

# disabled rates over time, all
disab_all = acs3 %>%
  mutate(
    diffany = case_when(
  diffhear == 2 | diffeye == 2 | diffrem == 2 | 
    diffphys == 2 | diffcare == 2 | diffmob == 2 ~ 2,
  diffhear == 1 | diffeye == 1 | diffrem == 1 | 
    diffphys == 1 | diffcare == 1 | diffmob == 1 ~ 1,
  TRUE ~ NA_real_))

disabled_rates_all = disab_all %>%
  mutate(disabled = diffany == 2) %>%
  group_by(year, immig_status) %>%
  summarise(
    disabled = sum(perwt[disabled], na.rm = TRUE),
    population = sum(perwt, na.rm = TRUE),
    pct = disabled / population * 100, 
    .groups = "drop")

print(disabled_rates_all, n = Inf)

ggplot(disabled_rates_all, aes(x = as.numeric(year), y = pct, color = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0.02, 0), limits = c(0, 15)) +
  labs(
    title = "Disability Rate by Immigration Status (2010-2024)",
    subtitle = "ACS; \n Any difficulty: cognitive, ambulatory, independent living, self-care, vision, hearing",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS via IPUMS") +
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

ggsave("results/acs_disabled_rates_all.png", width = 15, height = 10)
  

# per capita $ social security and supplemental security consumption
acs_ss_ssi_percapita = acs3 %>%
  filter(year == 2024) %>%
  mutate(
    incss_clean   = ifelse(incss   > 0 & incss   < 99999, incss,   0),
    incsupp_clean = ifelse(incsupp > 0 & incsupp < 99999, incsupp, 0)
  ) %>%
  group_by(immig_status) %>%
  summarise(
    n = n(),
    population = sum(perwt, na.rm = TRUE),
    percapita_ss  = sum(perwt * incss_clean, na.rm = TRUE)   / sum(perwt, na.rm = TRUE),
    percapita_ssi = sum(perwt * incsupp_clean, na.rm = TRUE) / sum(perwt, na.rm = TRUE),
    percapita_ss_ssi = percapita_ss + percapita_ssi,
    .groups = "drop"
  )

print(acs_ss_ssi_percapita)