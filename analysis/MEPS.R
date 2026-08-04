if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Immigrant Health Coverage 2010-2024")

ddi_meps = read_ipums_ddi("data/input/meps_00002.xml")
meps = read_ipums_micro(ddi_meps)

meps = meps %>%
    rename_with(tolower)

meps <- meps %>%
  select(
    # identifiers / design
    year, mepsid, panel, psuann, stratann, perweight,
    # demographics
    age, sex, racea,
    # nativity
    usborn, yrsinusc, yrsinusg,
    # income
    inctot, ftotinccps, cpi2009,
    # health status / access barriers
    health, ybarcare,
    # insurance coverage
    hinotcov, himachip, covertype,
    # total expenditures (Medicaid vs. self-pay only)
    exptot, expmapay, expselfpay,
    # ER utilization and expenditures (Medicaid vs. self-pay only)
    ertotvis, erexptot, erexpma, erexpself
  )

meps_immig_counts = meps %>%
  filter(usborn %in% c(10, 11, 12, 20)) %>%
  mutate(
    nativity = case_when(
      usborn %in% c(11, 20) ~ "US-born",
      usborn %in% c(10, 12) ~ "Immigrant")) %>%
  group_by(year, nativity) %>%
  summarise(
    n = n(),
    population = sum(perweight, na.rm = TRUE),
    .groups = "drop")

print(meps_immig_counts, n = Inf)

# medicaid enrollment
meps_medicaid_nativity <- meps %>%
  filter(usborn %in% c(10, 11, 12, 20),
         himachip %in% c(1, 2)) %>%
  mutate(
    nativity = case_when(
      usborn %in% c(11, 20) ~ "US-born",
      usborn %in% c(10, 12) ~ "Immigrant"),
    medicaid = himachip == 2) %>%
  group_by(year, nativity) %>%
  summarise(
    n = n(),
    population = sum(perweight, na.rm = TRUE),
    medicaid_n = sum(medicaid, na.rm = TRUE),
    medicaid_pop = sum(perweight[medicaid], na.rm = TRUE),
    medicaid_rate = medicaid_pop / population,
    .groups = "drop")

ggplot(meps_medicaid_nativity, aes(x = as.numeric(year), y = medicaid_rate, color = nativity)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "US-born"    = "#3043B4",
    "Immigrant" = "#C97703")) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.05),
    expand = c(0.02, 0), limits = c(0.1, 0.25)) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = 2014.1, y = 0.19, label = "ACA (2014)",
           hjust = 0, size = 3, color = "gray50") +
  labs(
    title = "Medicaid Rate: US-Born vs Immigrants (2010–2023)",
    subtitle = "MEPS",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: MEPS via IPUMS") +
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

ggsave("results/MEPS_medicaid_nativity.png", width = 10, height = 6)

# average medicaid expenditures by nativity
meps_medicaid_cost_nativity = meps %>%
  mutate(
    himachip = as.numeric(as.character(himachip)),
    usborn = as.numeric(as.character(usborn))) %>%
  filter(usborn %in% c(10, 20),
         himachip == 2) %>%
  mutate(
    nativity = case_when(
      usborn %in% c(20) ~ "US-born",
      usborn %in% c(10) ~ "Immigrant")) %>%
  group_by(nativity) %>%
  summarise(
    n = n(),
    population = sum(perweight, na.rm = TRUE),
    avg_medicaid_cost = weighted.mean(expmapay, w = perweight, na.rm = TRUE),
    .groups = "drop")

meps_medicaid_cost_nativity

# average medicaid expenditures by age
meps_medicaid_cost_age = meps %>%
  mutate(himachip = as.numeric(as.character(himachip))) %>%
  filter(himachip == 2,
         !is.na(age), age < 996) %>%
  mutate(
    age_group = case_when(
      age < 18              ~ "0-17",
      age >= 18 & age < 35  ~ "18-34",
      age >= 35 & age < 50  ~ "35-49",
      age >= 50 & age < 65  ~ "50-64",
      age >= 65             ~ "65+"
    ),
    age_group = factor(age_group, levels = c("0-17", "18-34", "35-49", "50-64", "65+"))) %>%
  group_by(age_group) %>%
  summarise(
    n = n(),
    population = sum(perweight, na.rm = TRUE),
    avg_medicaid_cost = weighted.mean(expmapay, w = perweight, na.rm = TRUE),
    .groups = "drop")

meps_medicaid_cost_age

# average medicaid expenditures by nativity & age
meps_medicaid_cost_nativity_age = meps %>%
  mutate(
    himachip = as.numeric(as.character(himachip)),
    usborn = as.numeric(as.character(usborn))) %>%
  filter(usborn %in% c(10, 20),
         himachip == 2,
         !is.na(age), age < 996) %>%
  mutate(
    nativity = case_when(
      usborn == 20 ~ "US-born",
      usborn == 10 ~ "Immigrant"),
    age_group = case_when(
      age < 18              ~ "0-17",
      age >= 18 & age < 35  ~ "18-34",
      age >= 35 & age < 50  ~ "35-49",
      age >= 50 & age < 65  ~ "50-64",
      age >= 65             ~ "65+"),
    age_group = factor(age_group, levels = c("0-17", "18-34", "35-49", "50-64", "65+"))) %>%
  group_by(nativity, age_group) %>%
  summarise(
    n = n(),
    population = sum(perweight, na.rm = TRUE),
    avg_medicaid_cost = weighted.mean(expmapay, w = perweight, na.rm = TRUE),
    .groups = "drop")

meps_medicaid_cost_nativity_age

# average medicaid expenditures by nativity (2023 only) 
meps_medicaid_cost_nativity_2023 = meps %>%
  mutate(
    himachip = as.numeric(as.character(himachip)),
    usborn = as.numeric(as.character(usborn))) %>%
  filter(year == 2023,
         usborn %in% c(10, 20),
         himachip == 2) %>%
  mutate(
    nativity = case_when(
      usborn %in% c(20) ~ "US-born",
      usborn %in% c(10) ~ "Immigrant")) %>%
  group_by(nativity) %>%
  summarise(
    n = n(),
    population = sum(perweight, na.rm = TRUE),
    avg_medicaid_cost = weighted.mean(expmapay, w = perweight, na.rm = TRUE),
    .groups = "drop")

meps_medicaid_cost_nativity_2023

# average medicaid expenditures by age (2023 only) 
meps_medicaid_cost_age_2023 = meps %>%
  mutate(himachip = as.numeric(as.character(himachip))) %>%
  filter(year == 2023,
         himachip == 2,
         !is.na(age), age < 996) %>%
  mutate(
    age_group = case_when(
      age < 18              ~ "0-17",
      age >= 18 & age < 35  ~ "18-34",
      age >= 35 & age < 50  ~ "35-49",
      age >= 50 & age < 65  ~ "50-64",
      age >= 65             ~ "65+"
    ),
    age_group = factor(age_group, levels = c("0-17", "18-34", "35-49", "50-64", "65+"))) %>%
  group_by(age_group) %>%
  summarise(
    n = n(),
    population = sum(perweight, na.rm = TRUE),
    avg_medicaid_cost = weighted.mean(expmapay, w = perweight, na.rm = TRUE),
    .groups = "drop")

meps_medicaid_cost_age_2023

# average medicaid expenditures by nativity & age (2023 only) 
meps_medicaid_cost_nativity_age_2023 = meps %>%
  mutate(
    himachip = as.numeric(as.character(himachip)),
    usborn = as.numeric(as.character(usborn))) %>%
  filter(year == 2023,
         usborn %in% c(10, 20),
         himachip == 2,
         !is.na(age), age < 996) %>%
  mutate(
    nativity = case_when(
      usborn == 20 ~ "US-born",
      usborn == 10 ~ "Immigrant"),
    age_group = case_when(
      age < 18              ~ "0-17",
      age >= 18 & age < 35  ~ "18-34",
      age >= 35 & age < 50  ~ "35-49",
      age >= 50 & age < 65  ~ "50-64",
      age >= 65             ~ "65+"),
    age_group = factor(age_group, levels = c("0-17", "18-34", "35-49", "50-64", "65+"))) %>%
  group_by(nativity, age_group) %>%
  summarise(
    n = n(),
    population = sum(perweight, na.rm = TRUE),
    avg_medicaid_cost = weighted.mean(expmapay, w = perweight, na.rm = TRUE),
    .groups = "drop")

meps_medicaid_cost_nativity_age_2023
