## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory 
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Immigrant Health Coverage 2010-2024")

# CPS data -----------------------------------------------------------------
ddi_cps = read_ipums_ddi("data/input/cps_00007.xml")
cps = read_ipums_micro(ddi_cps)

cps = cps %>%
    rename_with(tolower)

# remove yrimmig banding: midpoint & round up?
cps = cps %>%
  mutate(yrimmig = case_when(
    yrimmig == 0  ~ NA_real_,  # NIU
    yrimmig == 1  ~ 1949,      # 1949 or earlier (anchor)
    yrimmig == 2  ~ 1955,      # 1950-1959
    yrimmig == 3  ~ 1962,      # 1960-1964
    yrimmig == 4  ~ 1967,      # 1965-1969
    yrimmig == 5  ~ 1972,      # 1970-1974
    yrimmig == 6  ~ 1977,      # 1975-1979
    yrimmig == 7  ~ 1981,      # 1980-1981
    yrimmig == 8  ~ 1983,      # 1982-1983
    yrimmig == 9  ~ 1985,      # 1984-1985
    yrimmig == 10 ~ 1987,      # 1986-1987
    yrimmig == 11 ~ 1989,      # 1988-1989
    yrimmig == 12 ~ 1991,      # 1990-1991
    yrimmig == 13 ~ 1993,      # 1992-1993
    yrimmig == 16 ~ 1995,      # 1994-1995
    yrimmig == 19 ~ 1997,      # 1996-1997
    yrimmig == 23 ~ 1999,      # 1998-1999
    yrimmig == 26 ~ 2001,      # 2000-2001
    yrimmig == 29 ~ 2003,      # 2002-2003
    yrimmig == 32 ~ 2005,      # 2004-2005
    yrimmig == 35 ~ 2007,      # 2006-2007
    yrimmig == 38 ~ 2009,      # 2008-2009
    yrimmig == 39 ~ 2009,      # 2008-2010
    yrimmig == 41 ~ 2011,      # 2010-2011
    yrimmig == 43 ~ 2012,      # 2010-2013
    yrimmig == 44 ~ 2013,      # 2012-2013
    yrimmig == 45 ~ 2013,      # 2012-2014
    yrimmig == 46 ~ 2014,      # 2012-2015
    yrimmig == 47 ~ 2015,      # 2014-2015
    yrimmig == 48 ~ 2015,      # 2014-2016
    yrimmig == 49 ~ 2016,      # 2014-2017
    yrimmig == 50 ~ 2017,      # 2016-2017
    yrimmig == 51 ~ 2017,      # 2016-2018
    yrimmig == 52 ~ 2018,      # 2016-2019
    yrimmig == 53 ~ 2019,      # 2018-2019
    yrimmig == 54 ~ 2019,      # 2018-2020
    yrimmig == 55 ~ 2020,      # 2018-2021
    yrimmig == 56 ~ 2021,      # 2020-2021
    yrimmig == 57 ~ 2021,      # 2020-2022
    yrimmig == 58 ~ 2022,      # 2020-2023
    yrimmig == 60 ~ 2023,      # 2022-2024
    yrimmig == 61 ~ 2024,      # 2022-2025
    TRUE ~ NA_real_))

# 2014 experimental survey (keep only 3/8 file)
cps = cps %>%
    filter(!(year == 2014 & hflag == 0))

# recode birthplace, citizenship, and welfare variables to match ACS
cps = cps %>%
  mutate(citizen = case_when(
    citizen %in% c(1, 2, 9) ~ 0,
    citizen == 3 ~ 1,
    citizen == 4 ~ 2,
    citizen == 5 ~ 3,
    TRUE ~ NA_real_)) %>%
  mutate(relate = case_when(
    relate == 101 ~ 1,
    relate %in% c(201, 202, 203) ~ 2,
    relate %in% c(301, 303) ~ 3,
    relate == 901 ~ 9,
    TRUE ~ 10)) %>%
  mutate(incssi = ifelse(incssi == 999999, 99999, incssi)) %>%
  mutate(incss = ifelse(incss == 999999, 99999, incss)) %>%
  mutate(incwelfr = ifelse(incwelfr == 999999, 99999, incwelfr))

cps = cps %>%
  mutate(immigrant = ifelse(citizen == 2 |
                            citizen == 3, 1, 0)) %>%
  mutate(immig_status = ifelse(bpl < 15000 | immigrant == 0 | citizen == 1, 1, NA)) %>%
  mutate(foreign_born = ifelse(bpl >= 15000, 1, 0)) %>%
  mutate(immig_status = case_when(
    immigrant == 1 & citizen == 2 ~ 2,
    immigrant == 1 & yrimmig < 1982 ~ 2,
    immigrant == 1 & incss > 0 & incss < 99999 ~ 2,
    immigrant == 1 & incssi > 0 & incssi < 99999 ~ 2,
    immigrant == 1 & incwelfr > 0 & incwelfr < 99999 ~ 2,
    immigrant == 1 & himcarely == 2 ~ 2,
    immigrant == 1 & himcaidly == 2 &
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
    immigrant == 1 & classwkr == 26 ~ 2,
    # occupations requiring licensing/lawful status, per Pew (2018) methodology -----
    immigrant == 1 & occ2010 %in% c(
      2100, 3850, 3060, 3255,                                        # lawyers, police, physicians, RNs
      3000, 3010, 3030, 3040, 3050, 3110, 3120, 3140, 3150,
      3160, 3200, 3210, 3220, 3230, 3245, 3250, 3256, 3258, 3260,
      3310, 3500,                                                     # health care professionals
      2040, 2050, 2060,                                               # religious workers
      2600, 2630, 2700, 2710, 2720, 2740, 2750, 2760,                 # athletes/artists/entertainers
      9800, 9810, 9820, 9830                                          # current miligary
    ) ~ 2,
    # inferred from Pew's named visa categories (visiting scholars, high-tech workers) --
    immigrant == 1 & occ2010 %in% c(
      2200,                                                           # visiting scholars -> postsecondary teachers
      1005, 1006, 1007, 1010, 1020, 1030, 1050, 1060, 1105, 1106, 1107,  # high-tech: computer occupations
      1320, 1340, 1350, 1360, 1400, 1410, 1420, 1430, 1440, 1450, 1460, 1520, 1530  # high-tech: engineers
    ) ~ 2,
    immigrant == 1 & bpl == 25000 & yrimmig < 2017 ~ 2,
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
  mutate(asecwt = ifelse(undercount > 0, asecwt * undercount, asecwt)) %>%
  mutate(immig_status = case_when(
    immig_status == 1 ~ "Native-born citizens",
    immig_status == 2 ~ "Legal immigrants",
    immig_status == 3 ~ "Illegal immigrants",
  )) %>%
  mutate(immig_status = factor(immig_status,
                               levels = c("Native-born citizens",
                                          "Legal immigrants",
                                          "Illegal immigrants")))

# rewrite final CPS dataset
fwrite(cps, "data/output/cpsdata_veterans.csv")

nrow(cps)
table(cps$year)
names(cps)

# analysis
cpsvets = fread("data/output/cpsdata_veterans.csv")

# vets by immig status (vetstat universe is civilians age 17+)
vets = cpsvets %>%
    filter(vetstat == 2) %>%
    group_by(year, immig_status) %>%
    summarise(n = n(),
    population = sum(asecwt),
    .groups = "drop")

print(vets, n = Inf)

# disabled vets by immig status
disabled_vets_pop = cpsvets %>%
  filter(vetstat == 2) %>%
  filter(diffany %in% c(1, 2)) %>%
  mutate(disabled = diffany == 2) %>%
  group_by(year, immig_status) %>%
  summarise(
    disabled = sum(asecwt[disabled], na.rm = TRUE),
    population = sum(asecwt, na.rm = TRUE),
    pct = disabled / population * 100,
    .groups = "drop")

print(disabled_vets_pop, n = Inf)

# all disabled, excluding vets (age 18+)
disabled_novets_by_age_2024_65p = cpsvets %>%
  filter(age >= 18) %>%
  filter(year == 2024) %>%
  filter(vetstat != 2) %>%
  filter(diffany %in% c(1, 2)) %>%
  mutate(disabled = diffany == 2,
         age_pooled = pmin(age, 65)) %>%
  group_by(age_pooled, immig_status) %>%
  summarise(
    disabled = sum(asecwt[disabled], na.rm = TRUE),
    population = sum(asecwt, na.rm = TRUE),
    pct = disabled / population * 100,
    .groups = "drop")

disabled_novets_by_age_2024_65p_allimm = cpsvets %>%
  filter(age >= 18) %>%
  filter(year == 2024) %>%
  filter(vetstat != 2) %>%
  filter(diffany %in% c(1, 2)) %>%
  filter(immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  mutate(disabled = diffany == 2,
         age_pooled = pmin(age, 65)) %>%
  group_by(age_pooled) %>%
  summarise(
    immig_status = "All immigrants",
    disabled = sum(asecwt[disabled], na.rm = TRUE),
    population = sum(asecwt, na.rm = TRUE),
    pct = disabled / population * 100,
    .groups = "drop")

disabled_novets_by_age_2024_65p = bind_rows(disabled_novets_by_age_2024_65p, disabled_novets_by_age_2024_65p_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(disabled_novets_by_age_2024_65p, n = Inf)

colors_4 = c(
  "Native-born citizens" = "#3043B4",
  "Legal immigrants"     = "#7C756D",
  "Illegal immigrants"   = "#C97703",
  "All immigrants"       = "#0D0E51")

linetypes_4 = c(
  "Native-born citizens" = "solid",
  "Legal immigrants"     = "solid",
  "Illegal immigrants"   = "solid",
  "All immigrants"       = "dotted")

ggplot(disabled_novets_by_age_2024_65p, aes(x = age_pooled, y = pct, color = immig_status, linetype = immig_status)) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.8, span = 0.3) +
  scale_color_manual(values = colors_4) +
  scale_linetype_manual(values = linetypes_4) +
  scale_x_continuous(breaks = seq(20, 65, by = 10), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0.02, 0), limits = c(0, 35)) +
  labs(
    title = "Disability Rate by Age and Immigration Status, Non-Veterans (2024)",
    subtitle = "CPS ASEC; civilians age 18+, excluding veterans; Age 65+ pooled\nAny difficulty: hearing, vision, remembering, physical, mobility, or personal care limitation",
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
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

ggsave("results/cps_disabled_novets_by_age_2024_65plus.png", width = 15, height = 10)

### INCVET + GOTVDISA
# VA disability estimate: disabled veterans who report receiving disability compensation (2025)
disabled_vets_gotvdisa_2025 = cps %>%
  filter(year == 2025) %>%
  filter(vetstat == 2) %>%
  filter(diffany == 2) %>%
  filter(gotvdisa == 2) %>%
  mutate(incvet_clean = ifelse(incvet > 0 & incvet < 9999999, incvet, 0)) %>%
  summarise(
    n = n(),
    population = sum(asecwt, na.rm = TRUE),
    total_incvet = sum(asecwt * incvet_clean, na.rm = TRUE),
    percapita_incvet = total_incvet / population)

print(disabled_vets_gotvdisa_2025)

# total INCVET across everyone, 2025 (for scale/comparison)
total_incvet_2025 = cps %>%
  filter(year == 2025) %>%
  mutate(incvet_clean = ifelse(incvet > 0 & incvet < 9999999, incvet, 0)) %>%
  summarise(
    n = n(),
    population = sum(asecwt, na.rm = TRUE),
    total_incvet = sum(asecwt * incvet_clean, na.rm = TRUE))

print(total_incvet_2025)

total_incvet_2025 = cps %>%
  filter(year == 2025) %>%
  filter(vetstat == 2) %>%
  mutate(incvet_clean = ifelse(incvet > 0 & incvet < 9999999, incvet, 0)) %>%
  filter(incvet_clean > 0) %>%
  summarise(
    n = n(),
    population = sum(asecwt, na.rm = TRUE),
    total_incvet = sum(asecwt * incvet_clean, na.rm = TRUE))

print(total_incvet_2025)
