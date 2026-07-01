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

# remove group quarters &limit to working-age adults (18-64)
acs = acs %>%
    filter(gq %in% c(1, 2),
           age >= 18, age <= 64)

    # 47,595,496 total rows before
    # 26,762,659 total rows after

# rewrite final ACS dataset
acs = acs %>%
  select(
    year, serial, pernum, cluster, strata, perwt, statefip,
    sex, age, race, hispan, marst,
    citizen, yrimmig, bpl, bpld, vetstat,
    incss, incwelfr, incsupp,
    classwkr, relate, related,
    hcovany, hcovpriv, hinsemp, hinspur,
    hcovpub, hinscaid, hinscare, hinsva, hinstri)

fwrite(acs, "data/output/acsdata.csv")

# CPS data -----------------------------------------------------------------