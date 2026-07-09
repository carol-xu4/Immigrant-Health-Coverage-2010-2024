## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory 
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Immigrant Health Coverage 2010-2024")

# ACS data -----------------------------------------------------------------
ddi_acs = read_ipums_ddi("data/input/usa_00002.xml")
acs = read_ipums_micro(ddi_acs)

acs = acs %>%
    rename_with(tolower)

acs = acs %>%
  select(
    year, serial, pernum, cluster, strata, perwt, statefip,
    sex, age, race, hispan, marst,
    citizen, yrimmig, bpl, bpld, vetstat,
    incss, incwelfr, incsupp,
    classwkr, classwkrd, relate, related,
    hcovany, hcovpriv, hinsemp, hinspur,
    hcovpub, hinscaid, hinscare, hinsva, hinstri)

# before residual method -----------------------------------------------------
# medicaid enrollment among non-citizens by state and year
before_medicaid = acs %>%
  filter(
    citizen == 3, 
    bpl >= 150,    
    hinscaid == 2) %>%
  group_by(year, statefip) %>%
  summarise(
    n = n(),
    population = sum(perwt, na.rm = TRUE),
    .groups = "drop")

write_csv(before_medicaid, "results/before_medicaid.csv")

# total non-citizen foreign born population 
before_noncitizen_states = acs %>%
  filter(citizen == 3, bpl >= 150) %>%
  group_by(year, statefip) %>%
  summarise(
    n = n(),
    population = sum(perwt, na.rm = TRUE),
    .groups = "drop")

write_csv(before_noncitizen_states, "results/before_noncitizen_states.csv")

# residual method -------------------------------------------------------------
acs = acs %>%
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
fwrite(acs, "data/output/acsdata.csv")

# after residual method --------------------------------------------------------
# medicaid enrollment among non-citizens by state and year
after_medicaid = acs %>%
  filter(
    citizen == 3,
    bpl >= 150,
    hinscaid == 2) %>%
  group_by(year, statefip) %>%
  summarise(
    n = n(),
    population = sum(perwt, na.rm = TRUE),
    .groups = "drop")

write_csv(after_medicaid, "results/after_medicaid.csv")

# total non-citizen foreign born population
after_noncitizen_states = acs %>%
  filter(citizen == 3, bpl >= 150) %>%
  group_by(year, statefip) %>%
  summarise(
    n = n(),
    population = sum(perwt, na.rm = TRUE),
    .groups = "drop")

write_csv(after_noncitizen_states, "results/after_noncitizen_states.csv")

# combine before and after tables
medicaid_comparison = before_medicaid %>%
  rename(n_before = n, pop_before = population) %>%
  left_join(
    after_medicaid %>% rename(n_after = n, pop_after = population),
    by = c("year", "statefip")) %>%
  mutate(
    pop_change = pop_after - pop_before,
    pct_change = pop_change / pop_before * 100)

noncitizen_comparison = before_noncitizen_states %>%
  rename(n_before = n, pop_before = population) %>%
  left_join(
    after_noncitizen_states %>% rename(n_after = n, pop_after = population),
    by = c("year", "statefip")) %>%
  mutate(
    pop_change = pop_after - pop_before,
    pct_change = pop_change / pop_before * 100)

write_csv(medicaid_comparison,    "results/medicaid_comparison.csv")
write_csv(noncitizen_comparison,  "results/noncitizen_comparison.csv")

# how many undocumented immigrants arrived before 1982?
acs %>% filter(citizen == 3, bpl >= 150, yrimmig < 1982) %>%
  group_by(year) %>%
  summarise(
    n = n(),
    population = sum(perwt, na.rm = TRUE),
    .groups = "drop")

acs %>% filter(year == 2024,citizen == 3, bpl >= 150, yrimmig < 1982,
  hinscaid == 2) %>%
  group_by(year, age) %>%
  summarise(
    n = n(),
    population = sum(perwt, na.rm = TRUE),
    .groups = "drop")

acs %>% filter(year == 2010, citizen == 3, bpl >= 150, yrimmig < 1982,
  hinscaid == 2) %>%
  group_by(year, age) %>%
  summarise(
    n = n(),
    population = sum(perwt, na.rm = TRUE),
    .groups = "drop") %>%
  print(n = Inf)

acs %>% filter(year == 2024, immig_status == "Undocumented") %>%
  group_by(year, age) %>%
  summarise(
    n = n(),
    population = sum(perwt, na.rm = TRUE),
    .groups = "drop") %>%
  print(n = Inf)

# how many 65+ non-citizens are receiving any social security income
acs %>% filter(age >= 65, citizen == 3, bpl >= 150) %>%
  group_by(year) %>%
  summarise(
    total              = n(),
    has_ss             = sum(incss > 0 & incss < 99999, na.rm = TRUE),
    has_ssi            = sum(incsupp > 0 & incsupp < 99999, na.rm = TRUE),
    has_either         = sum((incss > 0 & incss < 99999) | (incsupp > 0 & incsupp < 99999), na.rm = TRUE),
    has_neither        = sum((incss == 0 | incss == 99999) & (incsupp == 0 | incsupp == 99999), na.rm = TRUE),
    .groups = "drop") %>%
  print(n = Inf)

# people on both medicaid and medicare
acs %>% filter(hinscare == 2, hinscaid == 2) %>%
  group_by(year, citizen) %>%
  summarise(
    n = n(),
    population = sum(perwt, na.rm = TRUE),
    .groups = "drop") %>%
  print(n = Inf)

# non-citizens age 65+, medicaid only, no medicare
acs %>% filter(age >= 65, citizen == 3, bpl >= 150, hinscaid == 2, hinscare == 1) %>%
  group_by(year) %>%
  summarise(
    n = n(),
    population = sum(perwt, na.rm = TRUE),
    .groups = "drop") %>% 
  print(n = Inf)

acs %>% filter(age >= 65, hinscaid == 2, hinscare == 1) %>%
  group_by(year) %>%
  summarise(
    n = n(),
    population = sum(perwt, na.rm = TRUE),
    .groups = "drop") %>% 
  print(n = Inf)

acs %>%
  filter(citizen == 3, bpl >= 150, age >= 65) %>%
  group_by(year, hinscare, hinscaid) %>%
  summarise(
    n          = n(),
    population = sum(perwt, na.rm = TRUE),
    .groups = "drop") %>%
  print(n = Inf)

# non-citizens 65+ on other insurance types
acs %>% filter(citizen == 3, bpl >= 150, age >= 65) %>%
  mutate(coverage_type = case_when(
      hcovany == 1                ~ "Uninsured",
      hinsemp == 2                ~ "Employer-sponsored",
      hinspur == 2                ~ "Direct purchase",
      hinstri == 2 | hinsva == 2 ~ "Other public",
      hinscare == 2               ~ "Medicare",
      hinscaid == 2               ~ "Medicaid",
      TRUE                        ~ "Unknown")) %>%
  group_by(year, coverage_type) %>%
  summarise(
    n          = n(),
    population = sum(perwt, na.rm = TRUE),
    .groups = "drop") %>% 
  arrange(year, coverage_type) %>%
  print(n = Inf)
