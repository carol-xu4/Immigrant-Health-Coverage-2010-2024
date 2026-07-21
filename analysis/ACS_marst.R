## Preliminaries ------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats, gt)

# Set working directory
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Immigrant Health Coverage 2010-2024")

# ACS marital status & spouse  data ------------------------------------------------
ddi_acs = read_ipums_ddi("data/input/usa_00006.xml")
acs = read_ipums_micro(ddi_acs)

acs = acs %>%
    rename_with(tolower)

acs = acs %>%
  select(
    year, serial, pernum, age, perwt, sploc, marst)

# how many married people not living with their spouse
married_no_spouse_present = acs %>%
    filter(marst %in% c(1, 2), sploc == 0)

nrow(married_no_spouse_present)
sum(married_no_spouse_present$perwt)

married_spouse_check_by_year = acs %>%
  mutate(spouse_coresident = sploc > 0) %>%
  filter(marst %in% c(1, 2)) %>%
  group_by(year, marst, spouse_coresident) %>%
  summarise(n = n(), pop = sum(perwt), .groups = "drop")

print(married_spouse_check_by_year, n = Inf)

write.csv(married_spouse_check_by_year, "results/married_spouse_check_by_year.csv", row.names = FALSE)

# Three ways to identify "married, spouse not co-resident" --------------------------------------

# 1. marst == 2 alone (self-reported "married, spouse absent")
def_marst2 = acs %>%
  filter(marst == 2) %>%
  group_by(year) %>%
  summarise(n_marst2 = n(), pop_marst2 = sum(perwt))

# 2. sploc == 0 alone, regardless of what marst says
#    (includes never-married, divorced, widowed, separated -- anyone with no spouse pointer)
def_sploc0 = acs %>%
  filter(sploc == 0) %>%
  group_by(year) %>%
  summarise(n_sploc0 = n(), pop_sploc0 = sum(perwt))

# 3. combination: currently married (marst 1 or 2) AND no co-resident spouse per sploc
def_combo = acs %>%
  filter(marst %in% c(1, 2), sploc == 0) %>%
  group_by(year) %>%
  summarise(n_combo = n(), pop_combo = sum(perwt))

comparison = def_marst2 %>%
  full_join(def_sploc0, by = "year") %>%
  full_join(def_combo, by = "year")

print(comparison, n = Inf)

write.csv(comparison, "results/marst_sploc_definition_comparison.csv", row.names = FALSE)


# separated
separated = acs %>%
  filter(marst == 3) %>%
  group_by(year) %>%
  summarise(n = n(), pop = sum(perwt))

print(separated, n = Inf)

# married people (spouse present) but not living with spouse
married_present_no_spouse = acs %>%
  filter(marst == 1, sploc == 0) %>%
  group_by(year) %>%
  summarise(n = n(), pop = sum(perwt))

print(married_present_no_spouse, n = Inf)

# married spouse absent, and sploc == 0
acs %>%
  filter(marst == 2) %>%
  dplyr::count(sploc == 0)

acs %>%
  filter(marst == 2) %>%
  dplyr::count(sploc > 0)

sum(acs$marst == 2 & acs$sploc > 0, na.rm = TRUE)
