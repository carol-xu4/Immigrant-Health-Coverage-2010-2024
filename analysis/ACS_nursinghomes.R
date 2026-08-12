## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats)

# Set working directory
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Immigrant Health Coverage 2010-2024")

## READ ME BEFORE INTERPRETING ----------------------------------------------
# Three tables, each by year:
#   1. immig_status  (3 categories from the residual method)
#   2. citizen vs noncitizen  (raw CITIZEN variable, untouched by the method)
#   3. native-born vs foreign-born  (raw BPL variable, untouched by the method)
#
# WHY 2 AND 3 EXIST: the occupational licensing carveout in the residual method
# presumes legal status for occ2010 codes 3255 (RNs), 3256 (nurse anesthetists),
# 3258 (NPs/midwives) and 3500 (LPNs). Any immigrant in those four occupations
# is assigned status 2 before the residual is taken, so "Illegal immigrants"
# will be EXACTLY ZERO in those four panels of table 1, every year. That flat
# zero line is the carveout, not a finding, and not a data error. Only aides
# (3600) are free to vary. Tables 2 and 3 are built from raw ACS variables the
# method never touches, so they are the defensible cuts for nurses.
#
# The "Nursing home employees" row DOES show nonzero illegal immigrants because
# it spans all occupations (aides, food service, cleaning). But it is biased
# DOWNWARD, since the RNs and LPNs inside it are forced to legal.
#
# ROWS OVERLAP AND DO NOT SUM TO A TOTAL. An RN employed in a nursing home
# appears in both "Registered nurses" and "Nursing home employees". Groups are
# built by binding separate filtered frames, not by a mutually exclusive
# case_when, so this is deliberate. Never sum the worker_group rows.

# acs_nursinghomes.csv is ~5.8 GB across 43 columns. Only fifteen are used
# here, so read those directly rather than pulling the whole file into memory.
acs = fread("data/output/acs_nursinghomes.csv",
            select = c("year", "age", "statefip", "occ2010", "ind1990", "classwkr",
                       "citizen", "bpl", "perwt", "immig_status",
                       "hcovany", "hcovpub", "hinscaid", "hinscare", "hinsva"))

## Industry definition -------------------------------------------------------
# VERIFIED against the DDI of extract usa_00022 (data/input/usa_00022.xml):
#   832 = "Nursing and personal care facilities"      <- nursing homes
#   870 = "Residential care facilities, without nursing"  <- assisted living
#   831 = "Hospitals"
# IND1990 keeps assisted living separate from nursing facilities, which is why
# it is the right primary variable here. INDNAICS is a character variable with
# no enumerated codes in the DDI and re-bases four times across 2010-2024
# (2007/2012/2017/2022 NAICS), so it is not used.
NURSING_HOME_IND1990 = c(832)

# Contrast group in tables 1-3: assisted living without nursing.
RESIDENTIAL_CARE_IND1990 = c(870)

## Employment universe -------------------------------------------------------
# NOTE: usa_00022 has no EMPSTAT, LABFORCE, UHRSWORK or WKSWORK, so "currently
# employed" cannot be defined. The IPUMS documentation for INDNAICS is explicit:
# for people out of the labor force, industry and occupation "refer to their
# most recent job, if it was within the previous five years." So retired nurses
# and former nursing home staff sit in these denominators. CLASSWKR is the only
# available proxy. Adding EMPSTAT to the next pull would fix this.
work = acs %>%
  filter(
    age >= 16,
    !occ2010 %in% c(0, 9920, 9999),
    classwkr %in% c(1, 2)) %>%          # self-employed or wage/salary
  mutate(
    # --- table 2: citizenship, straight from CITIZEN ---
    # 0 N/A (US born), 1 born abroad of American parents, 2 naturalized,
    # 3 not a citizen, 4 not a citizen but has first papers,
    # 5 foreign born, citizenship not reported -> NA, not silently recoded
    citizen_status = case_when(
      citizen %in% c(0, 1, 2) ~ "Citizen",
      citizen %in% c(3, 4)    ~ "Noncitizen",
      TRUE                    ~ NA_character_),
    # --- table 3: nativity, straight from BPL ---
    # Census definition: born abroad to American parents counts as native-born,
    # so the citizen == 1 test must come FIRST (those records have bpl >= 150).
    # This differs from foreign_born in the residual method, which omits it.
    nativity = case_when(
      bpl < 150 | citizen == 1 ~ "Native-born",
      bpl >= 150               ~ "Foreign-born",
      TRUE                     ~ NA_character_),
    citizen_status = factor(citizen_status, levels = c("Citizen", "Noncitizen")),
    nativity       = factor(nativity, levels = c("Native-born", "Foreign-born")),
    immig_status   = factor(immig_status,
                            levels = c("Native-born citizens",
                                       "Legal immigrants",
                                       "Illegal immigrants")))

# surface dropped records rather than losing them quietly
message("Unclassifiable citizenship: ", sum(is.na(work$citizen_status)),
        " | Unclassifiable nativity: ", sum(is.na(work$nativity)),
        " | Unclassifiable immig_status: ", sum(is.na(work$immig_status)))

## Worker group definitions --------------------------------------------------
# status_predetermined flags groups whose immig_status is assigned by the
# residual method's licensing carveout. Carried into the CSVs so the caveat
# cannot get separated from the numbers downstream.
group_defs = list(
  list(label = "Registered nurses",               rows = quote(occ2010 == 3255), predetermined = TRUE),
  list(label = "Nurse anesthetists",              rows = quote(occ2010 == 3256), predetermined = TRUE),
  list(label = "Nurse practitioners & midwives",  rows = quote(occ2010 == 3258), predetermined = TRUE),
  list(label = "LPNs/LVNs",                       rows = quote(occ2010 == 3500), predetermined = TRUE),
  list(label = "Nursing/psych/home health aides", rows = quote(occ2010 == 3600), predetermined = FALSE),
  list(label = "Nursing home employees (all occupations)",
       rows = quote(ind1990 %in% NURSING_HOME_IND1990), predetermined = FALSE),
  # contrast group: assisted living / residential care WITHOUT nursing (870).
  # IND1990 keeps this separate from 832, so the two can be compared directly.
  list(label = "Residential care, no nursing (all occupations)",
       rows = quote(ind1990 %in% RESIDENTIAL_CARE_IND1990), predetermined = FALSE))

## Table builder -------------------------------------------------------------
tabulate_group = function(df, group_label, predetermined, status_col) {
  df %>%
    filter(!is.na(.data[[status_col]])) %>%
    group_by(year, status = .data[[status_col]], .drop = FALSE) %>%
    summarise(n = n(), workers = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
    group_by(year) %>%
    mutate(share = workers / sum(workers)) %>%
    ungroup() %>%
    mutate(worker_group = group_label,
           status_predetermined = predetermined,
           .before = 1)
}

build_table = function(status_col) {
  map_dfr(group_defs, function(g) {
    tabulate_group(filter(work, !!g$rows), g$label, g$predetermined, status_col)
  }) %>%
    mutate(worker_group = factor(worker_group, levels = map_chr(group_defs, "label")))
}

table1_immig_status = build_table("immig_status")
table2_citizenship  = build_table("citizen_status")
table3_nativity     = build_table("nativity")

write_csv(table1_immig_status, "results/nh_table1_immig_status.csv")
write_csv(table2_citizenship,  "results/nh_table2_citizenship.csv")
write_csv(table3_nativity,     "results/nh_table3_nativity.csv")

# unweighted cell counts -- CHECK THESE before trusting any thin series.
# Nurse anesthetists and NPs/midwives are small occupations; crossed with
# noncitizen status and split by year they may be too sparse to plot.
table1_immig_status %>%
  group_by(worker_group, status) %>%
  summarise(min_n = min(n), median_n = median(n), .groups = "drop") %>%
  arrange(min_n) %>%
  print(n = Inf)

## Plot style ----------------------------------------------------------------
house_theme = theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    legend.key.width = unit(1.5, "cm"),
    strip.text = element_text(size = 17, face = "bold", color = "black"),
    panel.spacing = unit(1.2, "lines"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 18, color = "gray40"),
    axis.text.y = element_text(size = 18, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

colors_immig = c(
  "Native-born citizens" = "#3043B4",
  "Legal immigrants"     = "#7C756D",
  "Illegal immigrants"   = "#C97703")

colors_citizen  = c("Citizen"     = "#3043B4", "Noncitizen"   = "#C97703")
colors_nativity = c("Native-born" = "#3043B4", "Foreign-born" = "#C97703")

plot_shares = function(tbl, palette, plot_title, plot_subtitle, plot_caption) {
  ggplot(tbl, aes(x = as.numeric(year), y = share, color = status)) +
    geom_line(linewidth = 1.8) +
    facet_wrap(~ worker_group, ncol = 3) +
    scale_color_manual(values = palette) +
    scale_x_continuous(breaks = seq(2010, 2024, by = 4), expand = c(0.02, 0)) +
    scale_y_continuous(labels = scales::percent, expand = c(0.02, 0)) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = NULL, y = NULL, color = NULL,
      caption = plot_caption) +
    house_theme
}

## 1. by immig_status --------------------------------------------------------
nh_plot1 = plot_shares(
  table1_immig_status, colors_immig,
  "Nursing Workforce by Immigration Status (2010-2024)",
  "Share of workers, ages 16+; ACS",
  paste0("Source: ACS PUMS via IPUMS. RNs, nurse anesthetists, NPs/midwives and LPNs are presumed legal by the residual method's licensing\n",
         "carveout, so their illegal-immigrant share is zero by construction. Only aides vary freely. Groups overlap; do not sum."))

ggsave("results/nh_table1_immig_status.png", nh_plot1, width = 18, height = 11)

## 2. by citizenship ---------------------------------------------------------
nh_plot2 = plot_shares(
  table2_citizenship, colors_citizen,
  "Nursing Workforce by Citizenship (2010-2024)",
  "Share of workers, ages 16+; ACS. Citizenship from the raw CITIZEN variable",
  "Source: ACS PUMS via IPUMS. Unaffected by the residual method's licensing carveout. Groups overlap; do not sum.")

ggsave("results/nh_table2_citizenship.png", nh_plot2, width = 18, height = 11)

## 3. by nativity ------------------------------------------------------------
nh_plot3 = plot_shares(
  table3_nativity, colors_nativity,
  "Nursing Workforce by Nativity (2010-2024)",
  "Share of workers, ages 16+; ACS. Nativity from the raw BPL variable",
  "Source: ACS PUMS via IPUMS. Native-born includes those born abroad to American parents. Groups overlap; do not sum.")

ggsave("results/nh_table3_nativity.png", nh_plot3, width = 18, height = 11)

## ===========================================================================
## NURSING HOME WORKERS -- who actually staffs the facilities
## ===========================================================================
# Everything below is restricted to ind1990 == 832 and broken out by
# occupation, which is the original question: who is working there.
#
# Unlike the worker_group rows above, nh_occ_group IS mutually exclusive, so
# these shares DO sum to 100% within year. Order matters in the case_when:
# 3255 and 3500 must be caught before the 3000:3540 bin, and 3600 before the
# 3610:3655 bin.

nh_workers = work %>%
  filter(ind1990 %in% NURSING_HOME_IND1990) %>%
  mutate(nh_occ_group = case_when(
    occ2010 == 3255        ~ "Registered nurses",
    occ2010 == 3500        ~ "LPNs/LVNs",
    occ2010 == 3600        ~ "Nursing/psych/home health aides",
    occ2010 == 4610        ~ "Personal care aides",
    occ2010 %in% 3000:3540 ~ "Other healthcare practitioners",
    occ2010 %in% 3610:3655 ~ "Other healthcare support",
    occ2010 %in% 4000:4150 ~ "Food preparation & serving",
    occ2010 %in% 4200:4250 ~ "Building cleaning & maintenance",
    occ2010 %in% 10:430    ~ "Management",
    occ2010 %in% 5000:5940 ~ "Office & administrative support",
    TRUE                   ~ "All other occupations"))

nh_occ_levels = c(
  "Nursing/psych/home health aides", "Registered nurses", "LPNs/LVNs",
  "Personal care aides", "Other healthcare practitioners", "Other healthcare support",
  "Food preparation & serving", "Building cleaning & maintenance",
  "Office & administrative support", "Management", "All other occupations")

nh_workers = nh_workers %>%
  mutate(nh_occ_group = factor(nh_occ_group, levels = nh_occ_levels))

## Table 4: occupational composition of the nursing home workforce -----------
table4_nh_composition = nh_workers %>%
  group_by(year, nh_occ_group, .drop = FALSE) %>%
  summarise(n = n(), workers = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  mutate(share = workers / sum(workers)) %>%
  ungroup()

write_csv(table4_nh_composition, "results/nh_table4_composition.csv")

## Tables 5-7: nursing home workers by occupation and status -----------------
# "All nursing home workers" is appended as an aggregate row so each table
# carries its own total line.
tabulate_nh = function(status_col) {
  by_occ = nh_workers %>%
    filter(!is.na(.data[[status_col]])) %>%
    group_by(year, nh_occ_group, status = .data[[status_col]], .drop = FALSE) %>%
    summarise(n = n(), workers = sum(perwt, na.rm = TRUE), .groups = "drop")

  overall = nh_workers %>%
    filter(!is.na(.data[[status_col]])) %>%
    group_by(year, status = .data[[status_col]], .drop = FALSE) %>%
    summarise(n = n(), workers = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
    mutate(nh_occ_group = "All nursing home workers")

  bind_rows(by_occ, overall) %>%
    group_by(year, nh_occ_group) %>%
    mutate(share = workers / sum(workers)) %>%
    ungroup() %>%
    mutate(nh_occ_group = factor(nh_occ_group,
                                 levels = c("All nursing home workers", nh_occ_levels)))
}

table5_nh_immig_status = tabulate_nh("immig_status")
table6_nh_citizenship  = tabulate_nh("citizen_status")
table7_nh_nativity     = tabulate_nh("nativity")

write_csv(table5_nh_immig_status, "results/nh_table5_immig_status.csv")
write_csv(table6_nh_citizenship,  "results/nh_table6_citizenship.csv")
write_csv(table7_nh_nativity,     "results/nh_table7_nativity.csv")

# unweighted cell counts for the nursing home cuts
table5_nh_immig_status %>%
  group_by(nh_occ_group, status) %>%
  summarise(min_n = min(n), median_n = median(n), .groups = "drop") %>%
  arrange(min_n) %>%
  print(n = Inf)

## Composition plot ----------------------------------------------------------
colors_nh_occ = c(
  "Nursing/psych/home health aides" = "#3043B4",
  "Registered nurses"               = "#0D0E51",
  "LPNs/LVNs"                       = "#5B7FD4",
  "Personal care aides"             = "#2A6B7C",
  "Other healthcare practitioners"  = "#6B8E23",
  "Other healthcare support"        = "#8FB03E",
  "Food preparation & serving"      = "#C97703",
  "Building cleaning & maintenance" = "#E8A33D",
  "Office & administrative support" = "#7C756D",
  "Management"                      = "#A8A29A",
  "All other occupations"           = "#C0392B")

nh_plot4 = ggplot(table4_nh_composition,
                  aes(x = as.numeric(year), y = share, fill = nh_occ_group)) +
  geom_col(width = 0.85) +
  scale_fill_manual(values = colors_nh_occ) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1),
                     expand = c(0, 0)) +
  labs(
    title = "Who Works in Nursing Homes (2010-2024)",
    subtitle = "Occupational composition of nursing and personal care facilities; ACS, ages 16+",
    x = NULL, y = NULL, fill = NULL,
    caption = "Source: ACS PUMS via IPUMS. Industry IND1990 == 832. Occupations are mutually exclusive and sum to 100%.") +
  house_theme +
  theme(legend.text = element_text(size = 15))

ggsave("results/nh_table4_composition.png", nh_plot4, width = 18, height = 11)

## Status plots --------------------------------------------------------------
plot_nh_shares = function(tbl, palette, plot_title, plot_subtitle, plot_caption) {
  ggplot(tbl, aes(x = as.numeric(year), y = share, color = status)) +
    geom_line(linewidth = 1.6) +
    facet_wrap(~ nh_occ_group, ncol = 4) +
    scale_color_manual(values = palette) +
    scale_x_continuous(breaks = seq(2010, 2024, by = 6), expand = c(0.02, 0)) +
    scale_y_continuous(labels = scales::percent, expand = c(0.02, 0)) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = NULL, y = NULL, color = NULL,
      caption = plot_caption) +
    house_theme +
    theme(strip.text = element_text(size = 14, face = "bold", color = "black"))
}

nh_plot5 = plot_nh_shares(
  table5_nh_immig_status, colors_immig,
  "Nursing Home Workers by Immigration Status (2010-2024)",
  "Share of workers within each occupation; ACS, ages 16+",
  paste0("Source: ACS PUMS via IPUMS. Industry IND1990 == 832. RNs and LPNs are presumed legal by the residual method's licensing carveout,\n",
         "so their illegal-immigrant share is zero by construction and the 'All nursing home workers' line is biased downward."))

ggsave("results/nh_table5_immig_status.png", nh_plot5, width = 20, height = 12)

nh_plot6 = plot_nh_shares(
  table6_nh_citizenship, colors_citizen,
  "Nursing Home Workers by Citizenship (2010-2024)",
  "Share of workers within each occupation; ACS, ages 16+. Citizenship from the raw CITIZEN variable",
  "Source: ACS PUMS via IPUMS. Industry IND1990 == 832. Unaffected by the residual method's licensing carveout.")

ggsave("results/nh_table6_citizenship.png", nh_plot6, width = 20, height = 12)

nh_plot7 = plot_nh_shares(
  table7_nh_nativity, colors_nativity,
  "Nursing Home Workers by Nativity (2010-2024)",
  "Share of workers within each occupation; ACS, ages 16+. Nativity from the raw BPL variable",
  "Source: ACS PUMS via IPUMS. Industry IND1990 == 832. Native-born includes those born abroad to American parents.")

ggsave("results/nh_table7_nativity.png", nh_plot7, width = 20, height = 12)

## ===========================================================================
## COVERAGE OF NURSING HOME WORKERS -- are the people paid to deliver care
## insured themselves?
## ===========================================================================
# WHAT THIS EXTRACT CANNOT DO: usa_00022 dropped HINSEMP, HINSPUR, HINSTRI and
# HCOVPRIV, so the mutually exclusive coverage-TYPE breakdown used in
# ACS_analysis.R is impossible here. Only HCOVANY, HCOVPUB, HINSCAID, HINSCARE
# and HINSVA survived. What follows is therefore independent coverage RATES,
# which overlap (a worker can hold Medicaid and Medicare), not a 100% stack.
# Restoring the type breakdown needs another pull with HINSEMP and HINSPUR.

summarise_coverage = function(df, ...) {
  df %>%
    group_by(..., .drop = FALSE) %>%
    summarise(
      n          = n(),
      workers    = sum(perwt, na.rm = TRUE),
      uninsured  = weighted.mean(hcovany == 1,  w = perwt, na.rm = TRUE),
      medicaid   = weighted.mean(hinscaid == 2, w = perwt, na.rm = TRUE),
      medicare   = weighted.mean(hinscare == 2, w = perwt, na.rm = TRUE),
      any_public = weighted.mean(hcovpub == 2,  w = perwt, na.rm = TRUE),
      va         = weighted.mean(hinsva == 2,   w = perwt, na.rm = TRUE),
      .groups    = "drop")
}

tabulate_nh_coverage = function(status_col) {
  by_occ = nh_workers %>%
    filter(!is.na(.data[[status_col]])) %>%
    summarise_coverage(year, nh_occ_group, status = .data[[status_col]])

  overall = nh_workers %>%
    filter(!is.na(.data[[status_col]])) %>%
    summarise_coverage(year, status = .data[[status_col]]) %>%
    mutate(nh_occ_group = "All nursing home workers")

  bind_rows(by_occ, overall) %>%
    mutate(nh_occ_group = factor(nh_occ_group,
                                 levels = c("All nursing home workers", nh_occ_levels)))
}

table8_nh_coverage_immig    = tabulate_nh_coverage("immig_status")
table9_nh_coverage_citizen  = tabulate_nh_coverage("citizen_status")
table10_nh_coverage_nativity = tabulate_nh_coverage("nativity")

write_csv(table8_nh_coverage_immig,     "results/nh_table8_coverage_immig_status.csv")
write_csv(table9_nh_coverage_citizen,   "results/nh_table9_coverage_citizenship.csv")
write_csv(table10_nh_coverage_nativity, "results/nh_table10_coverage_nativity.csv")

plot_nh_rate = function(tbl, rate_col, palette, plot_title, plot_subtitle, plot_caption) {
  ggplot(tbl, aes(x = as.numeric(year), y = .data[[rate_col]], color = status)) +
    geom_line(linewidth = 1.6) +
    facet_wrap(~ nh_occ_group, ncol = 4) +
    scale_color_manual(values = palette) +
    scale_x_continuous(breaks = seq(2010, 2024, by = 6), expand = c(0.02, 0)) +
    scale_y_continuous(labels = scales::percent, expand = c(0.02, 0)) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = NULL, y = NULL, color = NULL,
      caption = plot_caption) +
    house_theme +
    theme(strip.text = element_text(size = 14, face = "bold", color = "black"))
}

nh_plot8 = plot_nh_rate(
  table8_nh_coverage_immig, "uninsured", colors_immig,
  "Uninsured Rate Among Nursing Home Workers (2010-2024)",
  "Share with no health coverage, by immigration status; ACS, ages 16+",
  "Source: ACS PUMS via IPUMS. Industry IND1990 == 832.")

ggsave("results/nh_uninsured_immig_status.png", nh_plot8, width = 20, height = 12)

nh_plot9 = plot_nh_rate(
  table9_nh_coverage_citizen, "uninsured", colors_citizen,
  "Uninsured Rate Among Nursing Home Workers (2010-2024)",
  "Share with no health coverage, by citizenship; ACS, ages 16+",
  "Source: ACS PUMS via IPUMS. Industry IND1990 == 832. Unaffected by the residual method's licensing carveout.")

ggsave("results/nh_uninsured_citizenship.png", nh_plot9, width = 20, height = 12)

nh_plot10 = plot_nh_rate(
  table8_nh_coverage_immig, "medicaid", colors_immig,
  "Medicaid Enrollment Among Nursing Home Workers (2010-2024)",
  "Share enrolled in Medicaid, by immigration status; ACS, ages 16+",
  "Source: ACS PUMS via IPUMS. Industry IND1990 == 832.")

ggsave("results/nh_medicaid_immig_status.png", nh_plot10, width = 20, height = 12)

## ===========================================================================
## 2024 PROFILE AND GEOGRAPHY
## ===========================================================================
# NOTE: SEX, INCTOT, FTOTINC and POVERTY were all dropped from usa_00022, so
# the share-female, median-income and poverty columns from the earlier draft
# cannot be rebuilt on this extract. Age and coverage are what remain.

table11_nh_profile_2024 = nh_workers %>%
  filter(year == 2024) %>%
  summarise_coverage(nh_occ_group, immig_status) %>%
  left_join(
    nh_workers %>%
      filter(year == 2024) %>%
      group_by(nh_occ_group, immig_status, .drop = FALSE) %>%
      summarise(median_age = weightedMedian(age, w = perwt, na.rm = TRUE),
                .groups = "drop"),
    by = c("nh_occ_group", "immig_status"))

write_csv(table11_nh_profile_2024, "results/nh_table11_profile_2024.csv")
print(table11_nh_profile_2024, n = Inf)

# state geography, 2024. Cells get thin fast -- n is carried so sparse states
# can be filtered or suppressed before anything is mapped or quoted.
table12_nh_states_2024 = nh_workers %>%
  filter(year == 2024) %>%
  group_by(statefip, immig_status, .drop = FALSE) %>%
  summarise(n = n(), workers = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
  group_by(statefip) %>%
  mutate(share = workers / sum(workers)) %>%
  ungroup()

write_csv(table12_nh_states_2024, "results/nh_table12_states_2024.csv")
