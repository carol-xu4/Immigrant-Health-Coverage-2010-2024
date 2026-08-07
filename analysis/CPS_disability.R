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
