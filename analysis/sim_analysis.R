## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory 
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Immigrant Health Coverage 2010-2024")

# ACS data -----------------------------------------------------------------
ddi_acs = read_ipums_ddi("data/input/usa_00002.xml")
acs = read_ipums_micro(ddi_acs)

acs2 = acs %>%
    rename_with(tolower)

acs2 = acs2 %>%
  select(
    year, serial, pernum, cluster, strata, perwt, statefip,
    sex, age, race, hispan, marst,
    citizen, yrimmig, bpl, bpld, vetstat,
    incss, incwelfr, incsupp,
    classwkr, classwkrd, relate, related,
    hcovany, hcovpriv, hinsemp, hinspur,
    hcovpub, hinscaid, hinscare, hinsva, hinstri, gq)

# remove group quarters
# 47595496 rows before, 45258095 rows after
acs2 = acs2 %>%
  filter(gq == 1 | gq == 2)

# acs residual method
acs2 = acs2 %>%
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

# CPS data -----------------------------------------------------------------
ddi_cps2 = read_ipums_ddi("data/input/cps_00005.xml")
cps2 = read_ipums_micro(ddi_cps2)

cps2 = cps2 %>%
    rename_with(tolower)

# remove group quarters (non-institutionalized)
# 2855740 rows before, 2854267 rows after
cps2 = cps2 %>%
    filter( gq == 1)

cps2 = cps2 %>%
  mutate(yrimmig = case_when(
    yrimmig == 0  ~ NA_real_,  
    yrimmig == 1  ~ 1949,      
    yrimmig == 2  ~ 1955,      
    yrimmig == 3  ~ 1962,      
    yrimmig == 4  ~ 1967,      
    yrimmig == 5  ~ 1972,      
    yrimmig == 6  ~ 1977,      
    yrimmig == 7  ~ 1981,     
    yrimmig == 8  ~ 1983,     
    yrimmig == 9  ~ 1985,      
    yrimmig == 10 ~ 1987,      
    yrimmig == 11 ~ 1989,      
    yrimmig == 12 ~ 1991,      
    yrimmig == 13 ~ 1993,      
    yrimmig == 16 ~ 1995,      
    yrimmig == 19 ~ 1997,      
    yrimmig == 23 ~ 1999,      
    yrimmig == 26 ~ 2001,      
    yrimmig == 29 ~ 2003,      
    yrimmig == 32 ~ 2005,      
    yrimmig == 35 ~ 2007,      
    yrimmig == 38 ~ 2009,      
    yrimmig == 39 ~ 2009,     
    yrimmig == 41 ~ 2011,      
    yrimmig == 43 ~ 2012,     
    yrimmig == 44 ~ 2013,     
    yrimmig == 45 ~ 2013,      
    yrimmig == 46 ~ 2014,     
    yrimmig == 47 ~ 2015,      
    yrimmig == 48 ~ 2015,      
    yrimmig == 49 ~ 2016,      
    yrimmig == 50 ~ 2017,     
    yrimmig == 51 ~ 2017,      
    yrimmig == 52 ~ 2018,     
    yrimmig == 53 ~ 2019,      
    yrimmig == 54 ~ 2019,      
    yrimmig == 55 ~ 2020,      
    yrimmig == 56 ~ 2021,      
    yrimmig == 57 ~ 2021,      
    yrimmig == 58 ~ 2022,      
    yrimmig == 60 ~ 2023,      
    yrimmig == 61 ~ 2024,      
    TRUE ~ NA_real_))

cps2 = cps2 %>%
    filter(!(year == 2014 & hflag == 0))

cps2 = cps2 %>%
    mutate(esi = ifelse(grpownly == 2 | grpdeply == 2, 2, 1))

cps2 = cps2 %>%
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

# cps residual method 
cps2 = cps2 %>%
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

# restrict to 2010-2025
cps2 = cps2 %>%
    filter(year != 2025)

# similarity analysis --------------------------------------------------------------------------------
# undocumented population
acs_undoc_summary = acs2 %>%
    group_by(year) %>%
    summarise(
        p_undoc = sum(perwt[immig_status == "Undocumented"]) / sum(perwt),
        n_eff = (sum(perwt))^2 / sum(perwt^2),
        .groups = "drop") %>%
    mutate(source = "ACS")

cps_undoc_summary = cps2 %>%
    group_by(year) %>%
    summarise(
        p_undoc = sum(asecwt[immig_status == "Undocumented"]) / sum(asecwt),
        n_eff = (sum(asecwt))^2 / sum(asecwt^2),
        .groups = "drop") %>%
    mutate(source = "CPS")
    
undoc_compare = acs_undoc_summary %>%
  select(year, p_acs = p_undoc, n_acs = n_eff) %>%
  inner_join(
    cps_undoc_summary %>% select(year, p_cps = p_undoc, n_cps = n_eff),
    by = "year") %>%
  mutate(
    se_diff = sqrt((p_acs * (1 - p_acs)) / n_acs + (p_cps * (1 - p_cps)) / n_cps),
    diff    = p_acs - p_cps,
    z       = diff / se_diff,
    p_value = 2 * pnorm(-abs(z)))

print(undoc_compare, n = Inf)

write_csv(undoc_compare, "results/undoc_z_test")
