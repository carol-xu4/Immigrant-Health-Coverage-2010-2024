if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Immigrant Health Coverage 2010-2024")

cps = fread("data/output/cpsdata_disability.csv")

colors = c(
  "Native-born citizens"= "#3043B4",
  "Legal immigrants"    = "#7C756D",
  "Illegal immigrants"  = "#C97703")

# total immigrant population in ASEC
CPS_immig_counts = cps %>%
  group_by(year, immig_status) %>%
  summarise(
    n = n(),
    population = sum(asecwt, na.rm = TRUE)) %>%
  ungroup()

write_csv(CPS_immig_counts, "results/CPS_immig_counts_year.csv")

# disabled populations (diffany universe: civilians age 15+)
disabled_pop = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  mutate(disabled = diffany == 2) %>%
  group_by(year, immig_status) %>%
  summarise(
    disabled = sum(asecwt[disabled], na.rm = TRUE),
    population = sum(asecwt, na.rm = TRUE),
    pct = disabled / population * 100, 
    .groups = "drop")

print(disabled_pop, n = Inf)

ggplot(disabled_pop, aes(x = as.numeric(year), y = pct, color = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0.02, 0), limits = c(0, 15)) +
  labs(
    title = "Disability Rate by Immigration Status (2010-2025)",
    subtitle = "CPS ASEC; Civilians ages 15+; \n Any difficulty: hearing, vision, remembering, physical, disability limiting mobility, personal care limitation",
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

ggsave("results/disabled_pop.png", width = 15, height = 10)

# how many getting disability insurance 
disability_income = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(incdisab >= 0, incdisab < 9999999) %>%  
  mutate(
    disabled = diffany == 2,
    receives_DI = incdisab > 0) %>%
  filter(disabled) %>%   
  group_by(year, immig_status) %>%
  summarise(
    n = n(),
    disab_population = sum(asecwt, na.rm = TRUE),
    receives_DI = sum(asecwt[receives_DI], na.rm = TRUE),
    pct_receiving = receives_DI / disab_population * 100,
    .groups = "drop")

print(disability_income, n = Inf)

disability_income = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(incdisab >= 0, incdisab < 9999999) %>%
  mutate(
    disabled = diffany == 2,
    receives_DI = incdisab > 0) %>%
  filter(disabled) %>%
  group_by(year, immig_status) %>%
  summarise(
    n = n(),
    disab_population = sum(asecwt, na.rm = TRUE),
    receives_DI = sum(asecwt[receives_DI], na.rm = TRUE),
    pct_receiving = receives_DI / disab_population * 100,
    .groups = "drop")

disability_income_allimm = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(incdisab >= 0, incdisab < 9999999) %>%
  filter(immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  mutate(
    disabled = diffany == 2,
    receives_DI = incdisab > 0) %>%
  filter(disabled) %>%
  group_by(year) %>%
  summarise(
    immig_status = "All immigrants",
    n = n(),
    disab_population = sum(asecwt, na.rm = TRUE),
    receives_DI = sum(asecwt[receives_DI], na.rm = TRUE),
    pct_receiving = receives_DI / disab_population * 100,
    .groups = "drop")

disability_income = bind_rows(disability_income, disability_income_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(disability_income, n = Inf)

# how many getting disability insurance, or social security income because of disability
full_disability_income = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(incdisab >= 0, incdisab < 999999) %>%
  mutate(
    disabled = diffany == 2,
    ss_disability   = (whyss1 == 2 | whyss2 == 2) & incss > 0,
    ssi_disability  = (whyssi1 == 1 | whyssi2 == 1) & incssi > 0,   # self only, excludes codes 3/4
    other_disability = incdisab > 0,
    any_disability_income = ss_disability | ssi_disability | other_disability
  ) %>%
  filter(disabled) %>%
  group_by(year, immig_status) %>%
  summarise(
    n = n(),
    population = sum(asecwt, na.rm = TRUE),
    pct_ss = sum(asecwt[ss_disability], na.rm = TRUE) / population * 100,
    pct_ssi = sum(asecwt[ssi_disability], na.rm = TRUE) / population * 100,
    pct_other = sum(asecwt[other_disability], na.rm = TRUE) / population * 100,
    pct_any = sum(asecwt[any_disability_income], na.rm = TRUE) / population * 100,
    .groups = "drop"
  )

print(full_disability_income, n = Inf)

# disability rates by single age, 2024
disab_by_age_2024 = cps %>%
  filter(age >= 15) %>%
  filter(year == 2024) %>%
  filter(diffany %in% c(1, 2)) %>%
  mutate(disabled = diffany == 2) %>%
  group_by(age, immig_status) %>%
  summarise(
    disabled = sum(asecwt[disabled], na.rm = TRUE),
    population = sum(asecwt, na.rm = TRUE),
    pct = disabled / population * 100,
    .groups = "drop")

print(disab_by_age_2024, n = Inf)

ggplot(disab_by_age_2024, aes(x = age, y = pct, color = immig_status)) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.8, span = 0.3) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(15, 90, by = 10), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0.02, 0)) +
  labs(
    title = "Disability Rate by Age and Immigration Status (2024)",
    subtitle = "CPS ASEC; civilians age 15+\nAny difficulty: hearing, vision, remembering, physical, mobility, or personal care limitation",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: CPS ASEC via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 18, color = "gray40", hjust = 0, margin = margin(b = 18)),
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

ggsave("results/cps_disabled_by_age_2024.png", width = 15, height = 10)

disab_by_age_2024 %>%
  filter(immig_status == "Illegal immigrants", age >= 60) %>%
  print(n = Inf)

ggplot(disab_by_age_2024, aes(x = age, y = pct, color = immig_status)) +
  geom_line(linewidth = 1.8) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(15, 90, by = 10), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0.02, 0)) +
  labs(
    title = "Disability Rate by Age and Immigration Status (2024)",
    subtitle = "CPS ASEC; civilians age 15+\nAny difficulty: hearing, vision, remembering, physical, mobility, or personal care limitation",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: CPS ASEC via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 18, color = "gray40", hjust = 0, margin = margin(b = 18)),
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

ggsave("results/cps_disabled_by_age_2024_lnotsmooth.png", width = 15, height = 10)

# disability by age, 65+ pooled
disab_by_age_2024_65p = cps %>%
  filter(age >= 15) %>%
  filter(year == 2024) %>%
  filter(diffany %in% c(1, 2)) %>%
  mutate(disabled = diffany == 2,
         age_pooled = pmin(age, 65)) %>%
  group_by(age_pooled, immig_status) %>%
  summarise(
    disabled = sum(asecwt[disabled], na.rm = TRUE),
    population = sum(asecwt, na.rm = TRUE),
    pct = disabled / population * 100,
    .groups = "drop")

print(disab_by_age_2024_65p, n = Inf)

# original
ggplot(disab_by_age_2024_65p, aes(x = age_pooled, y = pct, color = immig_status)) +
  geom_line(linewidth = 1.8) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(15, 90, by = 10), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0.02, 0), limits = c(0, 35)) +
  labs(
    title = "Disability Rate by Age and Immigration Status (2024)",
    subtitle = "CPS ASEC; civilians age 15+\nAny difficulty: hearing, vision, remembering, physical, mobility, or personal care limitation",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: CPS ASEC via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 18, color = "gray40", hjust = 0, margin = margin(b = 18)),
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

ggsave("results/cps_disabled_by_age_2024_65plus.png", width = 15, height = 10)

# smoothed
ggplot(disab_by_age_2024_65p, aes(x = age_pooled, y = pct, color = immig_status)) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.8, span = 0.3) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(15, 90, by = 10), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0.02, 0)) +
  labs(
    title = "Disability Rate by Age and Immigration Status (2024)",
    subtitle = "CPS ASEC; civilians age 15+\nAny difficulty: hearing, vision, remembering, physical, mobility, or personal care limitation",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: CPS ASEC via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 18, color = "gray40", hjust = 0, margin = margin(b = 18)),
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

ggsave("results/cps_disabled_by_age_2024_65plus_loess.png", width = 15, height = 10)

# income pooled
disability_income_pooled = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(incdisab >= 0, incdisab < 9999999) %>%
  filter(year >= 2020, year <= 2025) %>%   
  mutate(disabled = diffany == 2, receives_DI = incdisab > 0) %>%
  filter(disabled) %>%
  group_by(immig_status) %>%
  summarise(
    n = n(),
    disab_population = sum(asecwt, na.rm = TRUE),
    receives_DI = sum(asecwt[receives_DI], na.rm = TRUE),
    pct_receiving = receives_DI / disab_population * 100,
    .groups = "drop")

print(disability_income_pooled)


n_years = 6  # 2020-2025

disability_income_pooled = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(incdisab >= 0, incdisab < 9999999) %>%
  filter(year >= 2020, year <= 2025) %>%
  mutate(disabled = diffany == 2, receives_DI = incdisab > 0) %>%
  filter(disabled) %>%
  group_by(immig_status) %>%
  summarise(
    n = n(),
    pct_receiving = sum(asecwt[receives_DI], na.rm = TRUE) / sum(asecwt, na.rm = TRUE) * 100,
    disab_population_avg = sum(asecwt, na.rm = TRUE) / n_years,
    receives_DI_avg = sum(asecwt[receives_DI], na.rm = TRUE) / n_years,
    .groups = "drop")

print(disability_income_pooled)


# checking universes for incssi and whyssi1
cps %>%
  filter(!is.na(whyssi1)) %>%
  group_by(whyssi1) %>%
  summarise(
    n = n(),
    pct_positive_incssi = mean(incssi > 0, na.rm = TRUE) * 100,
    mean_age = mean(age, na.rm = TRUE),
    pct_age65plus = mean(age >= 65, na.rm = TRUE) * 100,
    pct_self_disabled = mean(diffany == 2, na.rm = TRUE) * 100,
    .groups = "drop")

cps %>%
  filter(!is.na(whyss1)) %>%
  group_by(whyss1) %>%
  summarise(
    n = n(),
    pct_positive_incss = mean(incss > 0, na.rm = TRUE) * 100,
    mean_age = mean(age, na.rm = TRUE),
    pct_age65plus = mean(age >= 65, na.rm = TRUE) * 100,
    pct_self_disabled = mean(diffany == 2, na.rm = TRUE) * 100,
    .groups = "drop")

full_disability_income = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(incdisab >= 0, incdisab < 999999) %>%
  mutate(
    disabled = diffany == 2,
    ss_disability   = (whyss1 == 2 | whyss2 == 2) & incss > 0,
    ssi_disability  = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0,  # disabled or blind, self only
    other_disability = incdisab > 0,
    any_disability_income = ss_disability | ssi_disability | other_disability
  ) %>%
  filter(disabled) %>%
  group_by(year, immig_status) %>%
  summarise(
    n = n(),
    population = sum(asecwt, na.rm = TRUE),
    pct_ss = sum(asecwt[ss_disability], na.rm = TRUE) / population * 100,
    pct_ssi = sum(asecwt[ssi_disability], na.rm = TRUE) / population * 100,
    pct_other = sum(asecwt[other_disability], na.rm = TRUE) / population * 100,
    pct_any = sum(asecwt[any_disability_income], na.rm = TRUE) / population * 100,
    .groups = "drop"
  )

print(full_disability_income, n = Inf)

n_years <- 6  # 2020-2025

full_disability_income_pooled = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(incdisab >= 0, incdisab < 999999) %>%
  filter(year >= 2020, year <= 2025) %>%
  mutate(
    disabled = diffany == 2,
    ss_disability     = (whyss1 == 2 | whyss2 == 2) & incss > 0,
    ssi_disability    = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0,
    other_disability  = incdisab > 0,
    any_disability_income = ss_disability | ssi_disability | other_disability
  ) %>%
  filter(disabled) %>%
  group_by(immig_status) %>%
  summarise(
    n = n(),
    pct_ss    = sum(asecwt[ss_disability], na.rm = TRUE) / sum(asecwt, na.rm = TRUE) * 100,
    pct_ssi   = sum(asecwt[ssi_disability], na.rm = TRUE) / sum(asecwt, na.rm = TRUE) * 100,
    pct_other = sum(asecwt[other_disability], na.rm = TRUE) / sum(asecwt, na.rm = TRUE) * 100,
    pct_any   = sum(asecwt[any_disability_income], na.rm = TRUE) / sum(asecwt, na.rm = TRUE) * 100,
    population_avg = sum(asecwt, na.rm = TRUE) / n_years,
    .groups = "drop")

print(full_disability_income_pooled)

# per capita $ social security and supplemental security consumption
cps_ss_ssi_percapita_2025 = cps %>%
  filter(year == 2025) %>%
  mutate(
    incss_clean  = ifelse(incss  > 0 & incss  < 99999, incss,  0),
    incssi_clean = ifelse(incssi > 0 & incssi < 99999, incssi, 0)
  ) %>%
  group_by(immig_status) %>%
  summarise(
    n = n(),
    population = sum(asecwt, na.rm = TRUE),
    percapita_ss  = sum(asecwt * incss_clean, na.rm = TRUE)  / sum(asecwt, na.rm = TRUE),
    percapita_ssi = sum(asecwt * incssi_clean, na.rm = TRUE) / sum(asecwt, na.rm = TRUE),
    percapita_ss_ssi = percapita_ss + percapita_ssi,
    .groups = "drop"
  )

print(cps_ss_ssi_percapita_2025)

n_years <- 6
cpi_to_2024 <- 1.883

cps_ss_ssi_percapita = cps %>%
  filter(year >= 2020, year <= 2025) %>%
  mutate(
    incss_clean  = ifelse(incss  > 0 & incss  < 99999, incss,  0),
    incssi_clean = ifelse(incssi > 0 & incssi < 99999, incssi, 0),
    incss_2024   = incss_clean  * cpi99 * cpi_to_2024,
    incssi_2024  = incssi_clean * cpi99 * cpi_to_2024
  ) %>%
  group_by(immig_status) %>%
  summarise(
    n = n(),
    population_avg = sum(asecwt, na.rm = TRUE) / n_years,
    percapita_ss  = sum(asecwt * incss_2024, na.rm = TRUE)  / sum(asecwt, na.rm = TRUE),
    percapita_ssi = sum(asecwt * incssi_2024, na.rm = TRUE) / sum(asecwt, na.rm = TRUE),
    percapita_ss_ssi = percapita_ss + percapita_ssi,
    .groups = "drop"
  )

print(cps_ss_ssi_percapita)


cps_disability_percapita_2025 = cps %>%
  filter(year == 2025) %>%
  filter(diffany %in% c(1, 2)) %>%
  mutate(
    disabled       = diffany == 2,
    incss_clean    = ifelse(incss    > 0 & incss    < 99999,   incss,    0),
    incssi_clean   = ifelse(incssi   > 0 & incssi   < 99999,   incssi,   0),
    incdisab_clean = ifelse(incdisab > 0 & incdisab < 9999999, incdisab, 0)
  ) %>%
  filter(disabled) %>%
  group_by(immig_status) %>%
  summarise(
    n = n(),
    disab_population = sum(asecwt, na.rm = TRUE),
    percapita_ss    = sum(asecwt * incss_clean, na.rm = TRUE)    / sum(asecwt, na.rm = TRUE),
    percapita_ssi   = sum(asecwt * incssi_clean, na.rm = TRUE)   / sum(asecwt, na.rm = TRUE),
    percapita_other = sum(asecwt * incdisab_clean, na.rm = TRUE) / sum(asecwt, na.rm = TRUE),
    percapita_any   = percapita_ss + percapita_ssi + percapita_other,
    .groups = "drop"
  )

print(cps_disability_percapita_2025)

cps_disability_percapita_2025 = cps %>%
  filter(year == 2025) %>%
  filter(diffany %in% c(1, 2)) %>%
  mutate(
    disabled        = diffany == 2,
    ss_disability   = (whyss1 == 2 | whyss2 == 2) & incss > 0 & incss < 99999,
    ssi_disability  = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0 & incssi < 99999,
    incss_clean     = ifelse(ss_disability,  incss,  0),
    incssi_clean    = ifelse(ssi_disability, incssi, 0),
    incdisab_clean  = ifelse(incdisab > 0 & incdisab < 9999999, incdisab, 0)
  ) %>%
  filter(disabled) %>%
  group_by(immig_status) %>%
  summarise(
    n = n(),
    disab_population = sum(asecwt, na.rm = TRUE),
    percapita_ss    = sum(asecwt * incss_clean, na.rm = TRUE)    / sum(asecwt, na.rm = TRUE),
    percapita_ssi   = sum(asecwt * incssi_clean, na.rm = TRUE)   / sum(asecwt, na.rm = TRUE),
    percapita_other = sum(asecwt * incdisab_clean, na.rm = TRUE) / sum(asecwt, na.rm = TRUE),
    percapita_any   = percapita_ss + percapita_ssi + percapita_other,
    .groups = "drop"
  )

print(cps_disability_percapita_2025)

cps_disability_percapita_2025 = cps %>%
  filter(year == 2025) %>%
  filter(diffany %in% c(1, 2)) %>%
  mutate(
    disabled        = diffany == 2,
    ss_disability   = (whyss1 == 2 | whyss2 == 2) & incss > 0 & incss < 99999,
    ssi_disability  = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0 & incssi < 99999,
    incss_clean     = ifelse(ss_disability,  incss,  0),
    incssi_clean    = ifelse(ssi_disability, incssi, 0),
    incdisab_clean  = ifelse(incdisab > 0 & incdisab < 9999999, incdisab, 0)
  ) %>%
  filter(disabled) %>%
  group_by(immig_status) %>%
  summarise(
    n = n(),
    disab_population = sum(asecwt, na.rm = TRUE),
    n_receives_ss    = sum(ss_disability, na.rm = TRUE),
    n_receives_ssi   = sum(ssi_disability, na.rm = TRUE),
    receives_ss_pop  = sum(asecwt[ss_disability], na.rm = TRUE),
    receives_ssi_pop = sum(asecwt[ssi_disability], na.rm = TRUE),
    percapita_ss     = sum(asecwt * incss_clean, na.rm = TRUE)    / sum(asecwt, na.rm = TRUE),
    percapita_ssi    = sum(asecwt * incssi_clean, na.rm = TRUE)   / sum(asecwt, na.rm = TRUE),
    percapita_other  = sum(asecwt * incdisab_clean, na.rm = TRUE) / sum(asecwt, na.rm = TRUE),
    percapita_any    = percapita_ss + percapita_ssi + percapita_other,
    .groups = "drop"
  )

print(cps_disability_percapita_2025, n == Inf, width = Inf)

# table 2: number of people with disability, and receives income
disability_income_pooled = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(incdisab >= 0, incdisab < 9999999) %>%
  filter(year >= 2020, year <= 2025) %>%
  mutate(disabled = diffany == 2, receives_DI = incdisab > 0) %>%
  filter(disabled) %>%
  group_by(immig_status) %>%
  summarise(
    n = n(),
    pct_receiving = sum(asecwt[receives_DI], na.rm = TRUE) / sum(asecwt, na.rm = TRUE) * 100,
    disab_population_avg = sum(asecwt, na.rm = TRUE) / n_years,
    receives_DI_avg = sum(asecwt[receives_DI], na.rm = TRUE) / n_years,
    .groups = "drop")

disability_income_pooled_allimm = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(incdisab >= 0, incdisab < 9999999) %>%
  filter(year >= 2020, year <= 2025) %>%
  filter(immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  mutate(disabled = diffany == 2, receives_DI = incdisab > 0) %>%
  filter(disabled) %>%
  summarise(
    immig_status = "All immigrants",
    n = n(),
    pct_receiving = sum(asecwt[receives_DI], na.rm = TRUE) / sum(asecwt, na.rm = TRUE) * 100,
    disab_population_avg = sum(asecwt, na.rm = TRUE) / n_years,
    receives_DI_avg = sum(asecwt[receives_DI], na.rm = TRUE) / n_years)

disability_income_pooled = bind_rows(disability_income_pooled, disability_income_pooled_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(disability_income_pooled)

full_disability_income_pooled = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(incdisab >= 0, incdisab < 999999) %>%
  filter(year >= 2020, year <= 2025) %>%
  mutate(
    disabled = diffany == 2,
    ss_disability     = (whyss1 == 2 | whyss2 == 2) & incss > 0,
    ssi_disability    = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0,
    other_disability  = incdisab > 0,
    any_disability_income = ss_disability | ssi_disability | other_disability
  ) %>%
  filter(disabled) %>%
  group_by(immig_status) %>%
  summarise(
    n = n(),
    pct_ss    = sum(asecwt[ss_disability], na.rm = TRUE) / sum(asecwt, na.rm = TRUE) * 100,
    pct_ssi   = sum(asecwt[ssi_disability], na.rm = TRUE) / sum(asecwt, na.rm = TRUE) * 100,
    pct_other = sum(asecwt[other_disability], na.rm = TRUE) / sum(asecwt, na.rm = TRUE) * 100,
    pct_any   = sum(asecwt[any_disability_income], na.rm = TRUE) / sum(asecwt, na.rm = TRUE) * 100,
    population_avg = sum(asecwt, na.rm = TRUE) / n_years,
    .groups = "drop")

full_disability_income_pooled_allimm = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(incdisab >= 0, incdisab < 999999) %>%
  filter(year >= 2020, year <= 2025) %>%
  filter(immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  mutate(
    disabled = diffany == 2,
    ss_disability     = (whyss1 == 2 | whyss2 == 2) & incss > 0,
    ssi_disability    = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0,
    other_disability  = incdisab > 0,
    any_disability_income = ss_disability | ssi_disability | other_disability
  ) %>%
  filter(disabled) %>%
  summarise(
    immig_status = "All immigrants",
    n = n(),
    pct_ss    = sum(asecwt[ss_disability], na.rm = TRUE) / sum(asecwt, na.rm = TRUE) * 100,
    pct_ssi   = sum(asecwt[ssi_disability], na.rm = TRUE) / sum(asecwt, na.rm = TRUE) * 100,
    pct_other = sum(asecwt[other_disability], na.rm = TRUE) / sum(asecwt, na.rm = TRUE) * 100,
    pct_any   = sum(asecwt[any_disability_income], na.rm = TRUE) / sum(asecwt, na.rm = TRUE) * 100,
    population_avg = sum(asecwt, na.rm = TRUE) / n_years)

full_disability_income_pooled = bind_rows(full_disability_income_pooled, full_disability_income_pooled_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(full_disability_income_pooled)

# table 4: per cap SS & SSI (all reasons)
cps_ss_ssi_percapita_2025 = cps %>%
  filter(year == 2025) %>%
  mutate(
    incss_clean  = ifelse(incss  > 0 & incss  < 99999, incss,  0),
    incssi_clean = ifelse(incssi > 0 & incssi < 99999, incssi, 0)
  ) %>%
  group_by(immig_status) %>%
  summarise(
    n = n(),
    population = sum(asecwt, na.rm = TRUE),
    percapita_ss  = sum(asecwt * incss_clean, na.rm = TRUE)  / sum(asecwt, na.rm = TRUE),
    percapita_ssi = sum(asecwt * incssi_clean, na.rm = TRUE) / sum(asecwt, na.rm = TRUE),
    percapita_ss_ssi = percapita_ss + percapita_ssi,
    .groups = "drop"
  )

cps_ss_ssi_percapita_2025_allimm = cps %>%
  filter(year == 2025) %>%
  filter(immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  mutate(
    incss_clean  = ifelse(incss  > 0 & incss  < 99999, incss,  0),
    incssi_clean = ifelse(incssi > 0 & incssi < 99999, incssi, 0)
  ) %>%
  summarise(
    immig_status = "All immigrants",
    n = n(),
    population = sum(asecwt, na.rm = TRUE),
    percapita_ss  = sum(asecwt * incss_clean, na.rm = TRUE)  / sum(asecwt, na.rm = TRUE),
    percapita_ssi = sum(asecwt * incssi_clean, na.rm = TRUE) / sum(asecwt, na.rm = TRUE),
    percapita_ss_ssi = percapita_ss + percapita_ssi)

cps_ss_ssi_percapita_2025 = bind_rows(cps_ss_ssi_percapita_2025, cps_ss_ssi_percapita_2025_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(cps_ss_ssi_percapita_2025)

# table 5: per capita disability benefits 2025
cps_disability_percapita_2025 = cps %>%
  filter(year == 2025) %>%
  filter(diffany %in% c(1, 2)) %>%
  mutate(
    disabled        = diffany == 2,
    ss_disability   = (whyss1 == 2 | whyss2 == 2) & incss > 0 & incss < 99999,
    ssi_disability  = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0 & incssi < 99999,
    incss_clean     = ifelse(ss_disability,  incss,  0),
    incssi_clean    = ifelse(ssi_disability, incssi, 0),
    incdisab_clean  = ifelse(incdisab > 0 & incdisab < 9999999, incdisab, 0)
  ) %>%
  filter(disabled) %>%
  group_by(immig_status) %>%
  summarise(
    n = n(),
    disab_population = sum(asecwt, na.rm = TRUE),
    receives_ss_pop  = sum(asecwt[ss_disability], na.rm = TRUE),
    receives_ssi_pop = sum(asecwt[ssi_disability], na.rm = TRUE),
    percapita_ss     = sum(asecwt * incss_clean, na.rm = TRUE)    / sum(asecwt, na.rm = TRUE),
    percapita_ssi    = sum(asecwt * incssi_clean, na.rm = TRUE)   / sum(asecwt, na.rm = TRUE),
    percapita_other  = sum(asecwt * incdisab_clean, na.rm = TRUE) / sum(asecwt, na.rm = TRUE),
    percapita_any    = percapita_ss + percapita_ssi + percapita_other,
    .groups = "drop"
  )

cps_disability_percapita_2025_allimm = cps %>%
  filter(year == 2025) %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  mutate(
    disabled        = diffany == 2,
    ss_disability   = (whyss1 == 2 | whyss2 == 2) & incss > 0 & incss < 99999,
    ssi_disability  = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0 & incssi < 99999,
    incss_clean     = ifelse(ss_disability,  incss,  0),
    incssi_clean    = ifelse(ssi_disability, incssi, 0),
    incdisab_clean  = ifelse(incdisab > 0 & incdisab < 9999999, incdisab, 0)
  ) %>%
  filter(disabled) %>%
  summarise(
    immig_status = "All immigrants",
    n = n(),
    disab_population = sum(asecwt, na.rm = TRUE),
    receives_ss_pop  = sum(asecwt[ss_disability], na.rm = TRUE),
    receives_ssi_pop = sum(asecwt[ssi_disability], na.rm = TRUE),
    percapita_ss     = sum(asecwt * incss_clean, na.rm = TRUE)    / sum(asecwt, na.rm = TRUE),
    percapita_ssi    = sum(asecwt * incssi_clean, na.rm = TRUE)   / sum(asecwt, na.rm = TRUE),
    percapita_other  = sum(asecwt * incdisab_clean, na.rm = TRUE) / sum(asecwt, na.rm = TRUE),
    percapita_any    = percapita_ss + percapita_ssi + percapita_other)

cps_disability_percapita_2025 = bind_rows(cps_disability_percapita_2025, cps_disability_percapita_2025_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(cps_disability_percapita_2025, width = Inf)

# table 6: average $$ per recipient of disability-related income (2025)
cps_disability_per_recipient_2025 = cps %>%
  filter(year == 2025) %>%
  filter(diffany %in% c(1, 2)) %>%
  mutate(
    disabled         = diffany == 2,
    ss_disability    = (whyss1 == 2 | whyss2 == 2) & incss > 0 & incss < 99999,
    ssi_disability   = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0 & incssi < 99999,
    other_disability = incdisab > 0 & incdisab < 9999999,
    any_disability_income = ss_disability | ssi_disability | other_disability,
    incss_clean      = ifelse(ss_disability,    incss,    0),
    incssi_clean     = ifelse(ssi_disability,   incssi,   0),
    incdisab_clean   = ifelse(other_disability, incdisab, 0),
    incany_clean     = incss_clean + incssi_clean + incdisab_clean
  ) %>%
  filter(disabled) %>%
  group_by(immig_status) %>%
  summarise(
    n = n(),
    disab_population = sum(asecwt, na.rm = TRUE),
    ss_recipients     = sum(asecwt[ss_disability], na.rm = TRUE),
    ssi_recipients    = sum(asecwt[ssi_disability], na.rm = TRUE),
    other_recipients  = sum(asecwt[other_disability], na.rm = TRUE),
    any_recipients    = sum(asecwt[any_disability_income], na.rm = TRUE),
    per_recipient_ss    = sum(asecwt * incss_clean, na.rm = TRUE)    / ss_recipients,
    per_recipient_ssi   = sum(asecwt * incssi_clean, na.rm = TRUE)   / ssi_recipients,
    per_recipient_other = sum(asecwt * incdisab_clean, na.rm = TRUE) / other_recipients,
    per_recipient_any   = sum(asecwt * incany_clean, na.rm = TRUE)   / any_recipients,
    .groups = "drop")

cps_disability_per_recipient_2025_allimm = cps %>%
  filter(year == 2025) %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  mutate(
    disabled         = diffany == 2,
    ss_disability    = (whyss1 == 2 | whyss2 == 2) & incss > 0 & incss < 99999,
    ssi_disability   = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0 & incssi < 99999,
    other_disability = incdisab > 0 & incdisab < 9999999,
    any_disability_income = ss_disability | ssi_disability | other_disability,
    incss_clean      = ifelse(ss_disability,    incss,    0),
    incssi_clean     = ifelse(ssi_disability,   incssi,   0),
    incdisab_clean   = ifelse(other_disability, incdisab, 0),
    incany_clean     = incss_clean + incssi_clean + incdisab_clean
  ) %>%
  filter(disabled) %>%
  summarise(
    immig_status = "All immigrants",
    n = n(),
    disab_population = sum(asecwt, na.rm = TRUE),
    ss_recipients     = sum(asecwt[ss_disability], na.rm = TRUE),
    ssi_recipients    = sum(asecwt[ssi_disability], na.rm = TRUE),
    other_recipients  = sum(asecwt[other_disability], na.rm = TRUE),
    any_recipients    = sum(asecwt[any_disability_income], na.rm = TRUE),
    per_recipient_ss    = sum(asecwt * incss_clean, na.rm = TRUE)    / ss_recipients,
    per_recipient_ssi   = sum(asecwt * incssi_clean, na.rm = TRUE)   / ssi_recipients,
    per_recipient_other = sum(asecwt * incdisab_clean, na.rm = TRUE) / other_recipients,
    per_recipient_any   = sum(asecwt * incany_clean, na.rm = TRUE)   / any_recipients)

cps_disability_per_recipient_2025 = bind_rows(cps_disability_per_recipient_2025, cps_disability_per_recipient_2025_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(cps_disability_per_recipient_2025, width = Inf)


# table 6 pooled
n_years <- 6
cpi_to_2024 <- 1.883

cps_disability_per_recipient_pooled = cps %>%
  filter(year >= 2020, year <= 2025) %>%
  filter(diffany %in% c(1, 2)) %>%
  mutate(
    disabled         = diffany == 2,
    ss_disability    = (whyss1 == 2 | whyss2 == 2) & incss > 0 & incss < 99999,
    ssi_disability   = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0 & incssi < 99999,
    other_disability = incdisab > 0 & incdisab < 9999999,
    any_disability_income = ss_disability | ssi_disability | other_disability,
    incss_clean      = ifelse(ss_disability,    incss    * cpi99 * cpi_to_2024, 0),
    incssi_clean     = ifelse(ssi_disability,   incssi   * cpi99 * cpi_to_2024, 0),
    incdisab_clean   = ifelse(other_disability, incdisab * cpi99 * cpi_to_2024, 0),
    incany_clean     = incss_clean + incssi_clean + incdisab_clean
  ) %>%
  filter(disabled) %>%
  group_by(immig_status) %>%
  summarise(
    n = n(),
    disab_population_avg = sum(asecwt, na.rm = TRUE) / n_years,
    ss_recipients_avg     = sum(asecwt[ss_disability], na.rm = TRUE) / n_years,
    ssi_recipients_avg    = sum(asecwt[ssi_disability], na.rm = TRUE) / n_years,
    other_recipients_avg  = sum(asecwt[other_disability], na.rm = TRUE) / n_years,
    any_recipients_avg    = sum(asecwt[any_disability_income], na.rm = TRUE) / n_years,
    per_recipient_ss    = sum(asecwt * incss_clean, na.rm = TRUE)    / sum(asecwt[ss_disability], na.rm = TRUE),
    per_recipient_ssi   = sum(asecwt * incssi_clean, na.rm = TRUE)   / sum(asecwt[ssi_disability], na.rm = TRUE),
    per_recipient_other = sum(asecwt * incdisab_clean, na.rm = TRUE) / sum(asecwt[other_disability], na.rm = TRUE),
    per_recipient_any    = sum(asecwt * incany_clean, na.rm = TRUE)  / sum(asecwt[any_disability_income], na.rm = TRUE),
    .groups = "drop")

cps_disability_per_recipient_pooled_allimm = cps %>%
  filter(year >= 2020, year <= 2025) %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  mutate(
    disabled         = diffany == 2,
    ss_disability    = (whyss1 == 2 | whyss2 == 2) & incss > 0 & incss < 99999,
    ssi_disability   = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0 & incssi < 99999,
    other_disability = incdisab > 0 & incdisab < 9999999,
    any_disability_income = ss_disability | ssi_disability | other_disability,
    incss_clean      = ifelse(ss_disability,    incss    * cpi99 * cpi_to_2024, 0),
    incssi_clean     = ifelse(ssi_disability,   incssi   * cpi99 * cpi_to_2024, 0),
    incdisab_clean   = ifelse(other_disability, incdisab * cpi99 * cpi_to_2024, 0),
    incany_clean     = incss_clean + incssi_clean + incdisab_clean
  ) %>%
  filter(disabled) %>%
  summarise(
    immig_status = "All immigrants",
    n = n(),
    disab_population_avg = sum(asecwt, na.rm = TRUE) / n_years,
    ss_recipients_avg     = sum(asecwt[ss_disability], na.rm = TRUE) / n_years,
    ssi_recipients_avg    = sum(asecwt[ssi_disability], na.rm = TRUE) / n_years,
    other_recipients_avg  = sum(asecwt[other_disability], na.rm = TRUE) / n_years,
    any_recipients_avg    = sum(asecwt[any_disability_income], na.rm = TRUE) / n_years,
    per_recipient_ss    = sum(asecwt * incss_clean, na.rm = TRUE)    / sum(asecwt[ss_disability], na.rm = TRUE),
    per_recipient_ssi   = sum(asecwt * incssi_clean, na.rm = TRUE)   / sum(asecwt[ssi_disability], na.rm = TRUE),
    per_recipient_other = sum(asecwt * incdisab_clean, na.rm = TRUE) / sum(asecwt[other_disability], na.rm = TRUE),
    per_recipient_any    = sum(asecwt * incany_clean, na.rm = TRUE)  / sum(asecwt[any_disability_income], na.rm = TRUE))

cps_disability_per_recipient_pooled = bind_rows(cps_disability_per_recipient_pooled, cps_disability_per_recipient_pooled_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(cps_disability_per_recipient_pooled, width = Inf)

# tables 2 and 3, 2025 only
disability_income_2025 = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(incdisab >= 0, incdisab < 9999999) %>%
  filter(year == 2025) %>%
  mutate(disabled = diffany == 2, receives_DI = incdisab > 0) %>%
  filter(disabled) %>%
  group_by(immig_status) %>%
  summarise(
    n = n(),
    disab_population = sum(asecwt, na.rm = TRUE),
    receives_DI = sum(asecwt[receives_DI], na.rm = TRUE),
    pct_receiving = receives_DI / disab_population * 100,
    .groups = "drop")

disability_income_2025_allimm = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(incdisab >= 0, incdisab < 9999999) %>%
  filter(year == 2025) %>%
  filter(immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  mutate(disabled = diffany == 2, receives_DI = incdisab > 0) %>%
  filter(disabled) %>%
  summarise(
    immig_status = "All immigrants",
    n = n(),
    disab_population = sum(asecwt, na.rm = TRUE),
    receives_DI = sum(asecwt[receives_DI], na.rm = TRUE),
    pct_receiving = receives_DI / disab_population * 100)

disability_income_2025 = bind_rows(disability_income_2025, disability_income_2025_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(disability_income_2025)


full_disability_income_2025 = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(incdisab >= 0, incdisab < 999999) %>%
  filter(year == 2025) %>%
  mutate(
    disabled = diffany == 2,
    ss_disability     = (whyss1 == 2 | whyss2 == 2) & incss > 0,
    ssi_disability    = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0,
    other_disability  = incdisab > 0,
    any_disability_income = ss_disability | ssi_disability | other_disability
  ) %>%
  filter(disabled) %>%
  group_by(immig_status) %>%
  summarise(
    n = n(),
    population = sum(asecwt, na.rm = TRUE),
    pct_ss    = sum(asecwt[ss_disability], na.rm = TRUE) / population * 100,
    pct_ssi   = sum(asecwt[ssi_disability], na.rm = TRUE) / population * 100,
    pct_other = sum(asecwt[other_disability], na.rm = TRUE) / population * 100,
    pct_any   = sum(asecwt[any_disability_income], na.rm = TRUE) / population * 100,
    .groups = "drop")

full_disability_income_2025_allimm = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(incdisab >= 0, incdisab < 999999) %>%
  filter(year == 2025) %>%
  filter(immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  mutate(
    disabled = diffany == 2,
    ss_disability     = (whyss1 == 2 | whyss2 == 2) & incss > 0,
    ssi_disability    = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0,
    other_disability  = incdisab > 0,
    any_disability_income = ss_disability | ssi_disability | other_disability
  ) %>%
  filter(disabled) %>%
  summarise(
    immig_status = "All immigrants",
    n = n(),
    population = sum(asecwt, na.rm = TRUE),
    pct_ss    = sum(asecwt[ss_disability], na.rm = TRUE) / population * 100,
    pct_ssi   = sum(asecwt[ssi_disability], na.rm = TRUE) / population * 100,
    pct_other = sum(asecwt[other_disability], na.rm = TRUE) / population * 100,
    pct_any   = sum(asecwt[any_disability_income], na.rm = TRUE) / population * 100)

full_disability_income_2025 = bind_rows(full_disability_income_2025, full_disability_income_2025_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(full_disability_income_2025)

# new table 2: Number of People who Receive Disability Income (2025)
disability_benefit_pop_2025 = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(year == 2025) %>%
  mutate(
    ss_disability          = (whyss1 == 2 | whyss2 == 2) & incss > 0 & incss < 99999,
    ssi_disability         = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0 & incssi < 99999,
    other_disability       = incdisab > 0 & incdisab < 9999999,
    any_disability_benefit = ss_disability | ssi_disability | other_disability
  ) %>%
  group_by(immig_status) %>%
  summarise(
    n = n(),
    population = sum(asecwt, na.rm = TRUE),
    pct_ss    = sum(asecwt[ss_disability], na.rm = TRUE)          / population * 100,
    pct_ssi   = sum(asecwt[ssi_disability], na.rm = TRUE)         / population * 100,
    pct_other = sum(asecwt[other_disability], na.rm = TRUE)       / population * 100,
    pct_any   = sum(asecwt[any_disability_benefit], na.rm = TRUE) / population * 100,
    .groups = "drop")

disability_benefit_pop_2025_allimm = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(year == 2025) %>%
  filter(immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  mutate(
    ss_disability          = (whyss1 == 2 | whyss2 == 2) & incss > 0 & incss < 99999,
    ssi_disability         = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0 & incssi < 99999,
    other_disability       = incdisab > 0 & incdisab < 9999999,
    any_disability_benefit = ss_disability | ssi_disability | other_disability
  ) %>%
  summarise(
    immig_status = "All immigrants",
    n = n(),
    population = sum(asecwt, na.rm = TRUE),
    pct_ss    = sum(asecwt[ss_disability], na.rm = TRUE)          / population * 100,
    pct_ssi   = sum(asecwt[ssi_disability], na.rm = TRUE)         / population * 100,
    pct_other = sum(asecwt[other_disability], na.rm = TRUE)       / population * 100,
    pct_any   = sum(asecwt[any_disability_benefit], na.rm = TRUE) / population * 100)

disability_benefit_pop_2025 = bind_rows(disability_benefit_pop_2025, disability_benefit_pop_2025_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(disability_benefit_pop_2025)

# new table 3: shares of those with disab who receive any disab benefits
full_disability_income = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(incdisab >= 0, incdisab < 999999) %>%
  mutate(
    disabled = diffany == 2,
    ss_disability     = (whyss1 == 2 | whyss2 == 2) & incss > 0,
    ssi_disability    = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0,
    other_disability  = incdisab > 0,
    any_disability_income = ss_disability | ssi_disability | other_disability
  ) %>%
  filter(disabled) %>%
  group_by(year, immig_status) %>%
  summarise(
    n = n(),
    population = sum(asecwt, na.rm = TRUE),
    pct_ss    = sum(asecwt[ss_disability], na.rm = TRUE)    / population * 100,
    pct_ssi   = sum(asecwt[ssi_disability], na.rm = TRUE)   / population * 100,
    pct_other = sum(asecwt[other_disability], na.rm = TRUE) / population * 100,
    pct_any   = sum(asecwt[any_disability_income], na.rm = TRUE) / population * 100,
    .groups = "drop"
  )

full_disability_income_allimm = cps %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(incdisab >= 0, incdisab < 999999) %>%
  filter(immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  mutate(
    disabled = diffany == 2,
    ss_disability     = (whyss1 == 2 | whyss2 == 2) & incss > 0,
    ssi_disability    = (whyssi1 %in% c(1, 2) | whyssi2 %in% c(1, 2)) & incssi > 0,
    other_disability  = incdisab > 0,
    any_disability_income = ss_disability | ssi_disability | other_disability
  ) %>%
  filter(disabled) %>%
  group_by(year) %>%
  summarise(
    immig_status = "All immigrants",
    n = n(),
    population = sum(asecwt, na.rm = TRUE),
    pct_ss    = sum(asecwt[ss_disability], na.rm = TRUE)    / population * 100,
    pct_ssi   = sum(asecwt[ssi_disability], na.rm = TRUE)   / population * 100,
    pct_other = sum(asecwt[other_disability], na.rm = TRUE) / population * 100,
    pct_any   = sum(asecwt[any_disability_income], na.rm = TRUE) / population * 100,
    .groups = "drop"
  )

full_disability_income = bind_rows(full_disability_income, full_disability_income_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(full_disability_income, n = Inf)
