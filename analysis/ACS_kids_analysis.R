## Preliminaries ------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats, gt)

# Set working directory
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Immigrant Health Coverage 2010-2024")

# ACS kids data ------------------------------------------------------------------
# new data extract only 2014-2024, to reduce sample size 

ddi_acs = read_ipums_ddi("data/input/usa_00005.xml")
acs = read_ipums_micro(ddi_acs)

acs = acs %>%
    rename_with(tolower)

acs = acs %>%
  select(
    year, serial, pernum, cluster, strata, perwt, statefip,
    sex, age, race, hispan, marst,
    citizen, yrimmig, bpl, bpld, vetstat,
    incss, incwelfr, incsupp, gq,
    classwkr, classwkrd, relate, related,
    hcovany, hcovpriv, hinsemp, hinspur,
    hcovpub, hinscaid, hinscare, hinsva, hinstri,
    momloc, momloc2, poploc, poploc2,
    bpld_mom, bpld_mom2, citizen_mom, citizen_mom2, yrimmig_mom, yrimmig_mom2,
    bpld_pop, bpld_pop2, citizen_pop, citizen_pop2, yrimmig_pop, yrimmig_pop2)

# residual method 
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

# rewrite final ACS kids dataset
fwrite(acs, "data/output/acskids.csv")

# new acs data, --------------------------------------------------------------------------------------
acskids = fread("data/output/acskids.csv")

# kids on medicaid/CHIP by immigration status
medicaid_kids = acskids %>%
    filter(age < 18) %>%
    group_by(year, immig_status) %>%
    summarise(
        pct_medicaid = 100 * sum(perwt[hinscaid == 2]) / sum(perwt),
        n_kids = sum(perwt),
        .groups = "drop") %>%
    print(n = Inf)

ggplot(medicaid_kids, aes(x = as.numeric(year), y = pct_medicaid, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20), expand = c(0.02, 0),
                     limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(
    title = "Medicaid Coverage Among Children by Immigration Status (2014–2024)",
    subtitle = "Share of children under 18 covered by Medicaid/CHIP",
    x = NULL,
    y = "On Medicaid",
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

ggsave("results/medicaid_coverage_children_trend.png", width = 10, height = 6)

kids_2024 = medicaid_kids %>% filter(year == 2024)

ggplot(kids_2024, aes(x = reorder(immig_status, n_kids), y = n_kids,
                      fill = immig_status)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = scales::comma(round(n_kids))),
            hjust = -0.1, size = 3.5, color = "gray30") +
  coord_flip() +
  scale_fill_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Number of Children by Immigration Status (2024)",
    subtitle = "ACS; Children under 18",
    x = NULL, y = "Children (millions)", fill = NULL,
    caption = "Source: ACS PUMS via IPUMS, authors' calculations. Status is the child's own.") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/kids_by_status_2024.png", width = 10, height = 6)

# how many native-born children have foreign-born parents
foreignparents = acskids %>%
  filter(bpl < 150) %>%
  filter(bpld_mom >= 15000 | bpld_mom2 >= 15000 |
         bpld_pop >= 15000 | bpld_pop2 >= 15000) %>%
  group_by(year) %>%
  summarise(population = sum(perwt)) 

ggplot(foreignparents, aes(x = year, y = population)) +
  geom_line(color = "#3043B4", linewidth = 1.2) +
  geom_point(color = "#3043B4", size = 2) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  scale_x_continuous(breaks = unique(foreignparents$year)[c(TRUE, FALSE)]) +
  labs(
    title = "Native-born children with at least one foreign-born parent",
    subtitle = "ACS 2014-2024",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
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

ggsave("results/foreignparents.png", width = 15, height = 10)

# add immig_status of parents
parent_lookup = acskids %>%
  select(year, serial, pernum, immig_status)

acskids = acskids %>%
  left_join(
    parent_lookup %>% rename(immig_status_mom = immig_status),
    by = c("year", "serial", "momloc" = "pernum")
  ) %>%
  left_join(
    parent_lookup %>% rename(immig_status_pop = immig_status),
    by = c("year", "serial", "poploc" = "pernum")
  ) %>%
  left_join(
    parent_lookup %>% rename(immig_status_mom2 = immig_status),
    by = c("year", "serial", "momloc2" = "pernum")
  ) %>%
  left_join(
    parent_lookup %>% rename(immig_status_pop2 = immig_status),
    by = c("year", "serial", "poploc2" = "pernum"))


# memory limit
setDT(acskids)

parent_lookup_dt = acskids[, .(year, serial, pernum, immig_status)]

acskids[parent_lookup_dt, immig_status_mom  := i.immig_status, on = .(year, serial, momloc  = pernum)]
acskids[parent_lookup_dt, immig_status_pop  := i.immig_status, on = .(year, serial, poploc  = pernum)]
acskids[parent_lookup_dt, immig_status_mom2 := i.immig_status, on = .(year, serial, momloc2 = pernum)]
acskids[parent_lookup_dt, immig_status_pop2 := i.immig_status, on = .(year, serial, poploc2 = pernum)]

# sanity checks
names(acskids)

table(acskids$immig_status_mom, useNA = "always")
table(acskids$immig_status_mom2, useNA = "always")
table(acskids$immig_status_pop, useNA = "always")
table(acskids$immig_status_pop2, useNA = "always")

# sample household with mom present
sample_row = acskids[momloc > 0][1]
sample_row[, .(year, serial, momloc, immig_status_mom)]
# look up same mom
acskids[year == sample_row$year & serial == sample_row$serial & pernum == sample_row$momloc,
    .(year, serial, pernum, immig_status)]

# no moms
sum(acskids$momloc == 0)
sum(is.na(acskids$immig_status_mom == 0))

# filter acskids
acskids2 = acskids[age < 18]

# how many immigrant children
child_status_table_data = acskids2 %>%
  group_by(year, immig_status) %>%
  summarise(population = sum(perwt), .groups = "drop") %>%
  group_by(year) %>%
  mutate(pct = population / sum(population) * 100) %>%
  ungroup() %>%
  select(year, immig_status, population, pct) %>%
  pivot_wider(names_from = immig_status, values_from = c(population, pct))

child_status_table = child_status_table_data %>%
  gt() %>%
  tab_header(
    title = "Children by Own Immigration Status",
    subtitle = "ACS 2014-2024") %>%
  fmt_number(columns = starts_with("population"), decimals = 0, use_seps = TRUE) %>%
  fmt_number(columns = starts_with("pct"), decimals = 1) %>%
  cols_label(
    year = "Year",
    `population_Native-born` = "Native-born (N)",
    `pct_Native-born` = "Native-born (%)",
    `population_Naturalized citizen` = "Naturalized (N)",
    `pct_Naturalized citizen` = "Naturalized (%)",
    `population_Legal immigrant` = "Legal immigrant (N)",
    `pct_Legal immigrant` = "Legal immigrant (%)",
    `population_Undocumented` = "Undocumented (N)",
    `pct_Undocumented` = "Undocumented (%)") %>%
  tab_source_note(source_note = "Source: ACS PUMS via IPUMS") %>%
  tab_options(
    heading.title.font.size = 24,
    heading.subtitle.font.size = 16,
    table.font.size = 14,
    column_labels.font.weight = "bold")

child_status_table 

gtsave(child_status_table, "results/child_status_table.html")

# how many immigrant parents per child (0, 1, or 2)
acskids2$n_immigrant_parents = 
  (!is.na(acskids2$immig_status_mom)  & acskids2$immig_status_mom  != "Native-born") +
  (!is.na(acskids2$immig_status_pop)  & acskids2$immig_status_pop  != "Native-born") +
  (!is.na(acskids2$immig_status_mom2) & acskids2$immig_status_mom2 != "Native-born") +
  (!is.na(acskids2$immig_status_pop2) & acskids2$immig_status_pop2 != "Native-born")

table(acskids2$n_immigrant_parents)

sum(acskids2$perwt[acskids2$n_immigrant_parents > 0])
sum(acskids2$perwt)

# priority: both parents native born > at least one naturalized parent > at least one legal immigrant parent > at least one undocumented parent
acskids2 = acskids2 %>%
  mutate(
    has_naturalized = if_any(c(immig_status_mom, immig_status_pop, immig_status_mom2, immig_status_pop2),
                              ~ .x == "Naturalized citizen"),
    has_legal = if_any(c(immig_status_mom, immig_status_pop, immig_status_mom2, immig_status_pop2),
                        ~ .x == "Legal immigrant"),
    has_undocumented = if_any(c(immig_status_mom, immig_status_pop, immig_status_mom2, immig_status_pop2),
                               ~ .x == "Undocumented"),
    parent_group = case_when(
      has_naturalized ~ "At least one naturalized citizen parent",
      has_legal ~ "At least one legal immigrant parent",
      has_undocumented ~ "At least one undocumented parent",
      TRUE ~ "Both parents native-born"))

table(acskids2$parent_group)

parent_year_group_all = acskids2 %>%
  group_by(year, parent_group) %>%
  summarise(population = sum(perwt), .groups = "drop")

ggplot(parent_year_group_all, aes(x = year, y = population, fill = parent_group)) +
  geom_col(position = "fill", width = 0.7) +
  scale_y_continuous(labels = scales::label_number(suffix = "%", scale = 100)) +
  scale_x_continuous(breaks = unique(parent_year_group_all$year)[c(TRUE, FALSE)]) +
  scale_fill_manual(values = c(
    "At least one legal immigrant parent" = "#7C756D",
    "Both parents native-born" = "#3043B4",
    "At least one naturalized citizen parent" = "#0D0E51",
    "At least one undocumented parent" = "#C97703")) +
  labs(
    title = "Children by parental immigration status (All Children)",
    subtitle = "ACS; age < 18; all children",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
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
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave("results/parent_status_composition_all.png", width = 15, height = 10)

parent_year_group_native = acskids2 %>%
  filter(bpl < 150) %>%
  group_by(year, parent_group) %>%
  summarise(population = sum(perwt), .groups = "drop")

ggplot(parent_year_group_native, aes(x = year, y = population, fill = parent_group)) +
  geom_col(position = "fill", width = 0.7) +
  scale_y_continuous(labels = scales::label_number(suffix = "%", scale = 100)) +
  scale_x_continuous(breaks = unique(parent_year_group_native$year)[c(TRUE, FALSE)]) +
  scale_fill_manual(values = c(
    "At least one legal immigrant parent" = "#7C756D",
    "Both parents native-born" = "#3043B4",
    "At least one naturalized citizen parent" = "#0D0E51",
    "At least one undocumented parent" = "#C97703")) +
  labs(
    title = "Native-born children by parental immigration status",
    subtitle = "ACS; age < 18; native-born children only",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
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
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave("results/parent_status_composition_native.png", width = 15, height = 10)

# older dependents (adult kids living w parents, unmarried)
bums = acskids %>%
  filter(age >= 18, marst == 6, momloc > 0 | poploc > 0 | momloc2 > 0 | poploc2 > 0) %>%
  group_by(year, immig_status) %>%
  summarise(population = sum(perwt), .groups = "drop") 

# medicaid
medicaid_parent_native = acskids2 %>%
  filter(bpl < 150) %>%
  group_by(year, parent_group) %>%
  summarise( pct_medicaid = 100 * sum(perwt[hinscaid == 2]) / sum(perwt),
    pop_medicaid = sum(perwt[hinscaid == 2]),
    .groups = "drop")

print(medicaid_parent_native, n = Inf)

ggplot(medicaid_parent_native, aes(x = year, y = pct_medicaid, color = parent_group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
      limits = c(30, 70)) +
  scale_x_continuous(breaks = unique(medicaid_parent_native$year)[c(TRUE, FALSE)]) +
  scale_color_manual(values = c(
    "At least one legal immigrant parent" = "#7C756D",
    "Both parents native-born" = "#3043B4",
    "At least one naturalized citizen parent" = "#0D0E51",
    "At least one undocumented parent" = "#C97703")) +
  labs(
    title = "Medicaid coverage among native-born children",
    subtitle = "By parental immigration status, ACS; age < 18; native-born children only",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
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
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

ggsave("results/medicaid_pct_native_by_parent_status.png", width = 15, height = 10)

# mixed households
mixedkids = acskids2 %>%
  filter(bpl < 150) %>%
  mutate(
    n_present = rowSums(!is.na(cbind(immig_status_mom, immig_status_pop,
                                      immig_status_mom2, immig_status_pop2))),
    n_undoc = rowSums(cbind(immig_status_mom, immig_status_pop,
                             immig_status_mom2, immig_status_pop2) == "Undocumented",
                       na.rm = TRUE)) %>%
  mutate(
    undoc_group = case_when(
      n_present == 0 ~ NA_character_,
      n_undoc == n_present & n_present == 1 ~ "One parent undocumented",
      n_undoc == n_present & n_present >= 2 ~ "Both parents undocumented",
      n_undoc >= 1 & n_undoc < n_present ~ "One parent undocumented",
      TRUE ~ NA_character_)) %>%
  filter(!is.na(undoc_group))

mixedkids %>%
  group_by(undoc_group) %>%
  summarise(pop = sum(perwt)) %>%
  mutate(pct = pop / sum(pop) * 100)

medicaid_undoc_mixed = mixedkids %>%
  group_by(year, undoc_group) %>%
  summarise(
    pct_medicaid = 100 * sum(perwt[hinscaid == 2]) / sum(perwt),
    pop_medicaid = sum(perwt[hinscaid == 2]),
    .groups = "drop")

print(medicaid_undoc_mixed, n = Inf)

ggplot(medicaid_undoc_mixed, aes(x = year, y = pct_medicaid, color = undoc_group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous(breaks = unique(medicaid_undoc_mixed$year)[c(TRUE, FALSE)]) +
  scale_color_manual(values = c(
    "Both parents undocumented" = "#C97703",
    "One parent undocumented" = "#3043B4")) +
  labs(
    title = "Medicaid coverage among native-born children",
    subtitle = "Both vs. one undocumented parent, ACS; age < 18; native-born children only",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
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

ggsave("results/medicaid_undoc_mixed.png", width = 15, height = 10)

# mixed household composition
mat = as.matrix(acskids2 %>% filter(bpl < 150) %>%
  select(immig_status_mom, immig_status_pop, immig_status_mom2, immig_status_pop2))

other_status = apply(mat, 1, function(x) {
  x = x[!is.na(x)]
  if (length(x) == 1) return("No second parent")
  x[x != "Undocumented"][1]})

onekids = acskids2 %>%
  filter(bpl < 150) %>%
  mutate(
    other_status = other_status,
    n_present = rowSums(!is.na(cbind(immig_status_mom, immig_status_pop,
                                      immig_status_mom2, immig_status_pop2))),
    n_undoc = rowSums(cbind(immig_status_mom, immig_status_pop,
                             immig_status_mom2, immig_status_pop2) == "Undocumented",
                       na.rm = TRUE)
  ) %>%
  filter(n_present > 0, n_undoc >= 1, n_undoc < n_present | n_present == 1)

one_comp = onekids %>%
  group_by(year, other_status) %>%
  summarise(population = sum(perwt), .groups = "drop")

both_total = mixedkids %>%
  filter(undoc_group == "Both parents undocumented") %>%
  group_by(year) %>%
  summarise(population = sum(perwt))

ggplot(one_comp, aes(x = year, y = population, fill = other_status)) +
  geom_col(width = 0.7) +
  geom_line(data = both_total, aes(x = year, y = population),
            inherit.aes = FALSE, linetype = "dashed", linewidth = 1, color = "gray30") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
   annotate("text", x = 2021.5, y = 2750000, label = "Total, both parents undocumented",                
        hjust = 0, size = 6, color = "gray30") +
  scale_x_continuous(breaks = unique(one_comp$year)[c(TRUE, FALSE)]) +
  scale_fill_manual(values = c(
    "Native-born" = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant" = "#7C756D",
    "No second parent" = "#C97703")) +
  labs(
    title = "Children with one undocumented parent: who is the other parent?",
    subtitle = "ACS; age <18",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 18, color = "gray40", hjust = 0, margin = margin(b = 20)),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.5, "cm"),
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
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave("results/one_undoc_parent_composition.png", width = 15, height = 10)

# medicaid, all children
medicaid_allkids = acskids2 %>%
  filter(hinscaid == 2) %>%
  group_by(year)

# family breakdown
undoc_parents = acskids %>%
  filter(immig_status == "Undocumented", age >= 18, relate %in% c(1, 2)) %>%
  group_by(year) %>%
  summarise(pct_medicaid = 100 * sum(perwt[hinscaid == 2]) / sum(perwt),
            group = "Undocumented parents")

undoc_children = acskids %>%
  filter(immig_status == "Undocumented", age < 18) %>%
  group_by(year) %>%
  summarise(pct_medicaid = 100 * sum(perwt[hinscaid == 2]) / sum(perwt),
            group = "Undocumented children")

undoc_all = acskids %>%
  filter(immig_status == "Undocumented") %>%
  group_by(year) %>%
  summarise(pct_medicaid = 100 * sum(perwt[hinscaid == 2]) / sum(perwt))

undoc_all_labeled = undoc_all %>%
  mutate(group = "All undocumented (any age)")

undoc_combined = bind_rows(undoc_breakdown, undoc_all_labeled)

ggplot(undoc_combined, aes(x = year, y = pct_medicaid, color = group, linetype = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = filter(undoc_combined, group != "All undocumented (any age)"),
             size = 2) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 20)) +
  scale_x_continuous(breaks = unique(undoc_combined$year)[c(TRUE, FALSE)]) +
  scale_color_manual(values = c(
    "Undocumented parents" = "#C97703",
    "Undocumented children" = "#3043B4",
    "All undocumented (any age)" = "gray40")) +
  scale_linetype_manual(values = c(
    "Undocumented parents" = "solid",
    "Undocumented children" = "solid",
    "All undocumented (any age)" = "dashed")) +
  labs(
    title = "Medicaid coverage among undocumented immigrants",
    subtitle = "Parents vs. children",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    legend.key.width = unit(1.2, "cm"),
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

ggsave("results/medicaid_undoc_parents_vs_children.png", width = 15, height = 10)

# married parents, where one is out of the household
# married, spouse absent or separated
undoc_spouse_absent = acskids %>%
  filter(immig_status == "Undocumented", marst %in% c(2, 3), age >= 18) %>%
  group_by(year) %>%
  summarise(pop = sum(perwt), n = n())

# married spouse absent, separated, divorced, or widowed
undoc_no_spouse = acskids %>%
  filter(immig_status == "Undocumented", marst %in% c(2, 3, 4, 5), age >= 18) %>%
  group_by(year) %>%
  summarise(pop = sum(perwt), n = n())

print(undoc_spouse_absent, n = Inf)
print(undoc_no_spouse, n = Inf)

undoc_total = acskids %>%
  filter(immig_status == "Undocumented", age >= 18) %>%
  group_by(year) %>%
  summarise(total_pop = sum(perwt))

undoc_spouse_absent %>%
  left_join(undoc_total, by = "year") %>%
  mutate(pct = pop / total_pop * 100)

undoc_spouse_absent_pct = undoc_spouse_absent %>%
  left_join(undoc_total, by = "year") %>%
  mutate(pct = pop / total_pop * 100)

ggplot(undoc_spouse_absent_pct, aes(x = year, y = pop)) +
  geom_line(color = "#3043B4", linewidth = 1.2) +
  geom_point(color = "#3043B4", size = 2) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  scale_x_continuous(breaks = unique(undoc_spouse_absent_pct$year)[c(TRUE, FALSE)]) +
  labs(
    title = "Undocumented adults, married with spouse absent or separated",
    subtitle = "ACS; age ≥ 18",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
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

ggsave("results/undoc_spouse_absent_pop.png", width = 15, height = 10)

ggplot(undoc_spouse_absent_pct, aes(x = year, y = pct)) +
  geom_line(color = "#3043B4", linewidth = 1.2) +
  geom_point(color = "#3043B4", size = 2) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous(breaks = unique(undoc_spouse_absent_pct$year)[c(TRUE, FALSE)]) +
  labs(
    title = "Share of undocumented adults, married with spouse absent or separated",
    subtitle = "ACS; age ≥ 18",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
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

ggsave("results/undoc_spouse_absent_pct.png", width = 15, height = 10)

undoc_no_spouse_pct = undoc_no_spouse %>%
  left_join(undoc_total, by = "year") %>%
  mutate(pct = pop / total_pop * 100)

ggplot(undoc_no_spouse_pct, aes(x = year, y = pop)) +
  geom_line(color = "#C97703", linewidth = 1.2) +
  geom_point(color = "#C97703", size = 2) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M"), limits = c(1500000, 3000000)) +
  scale_x_continuous(breaks = unique(undoc_no_spouse_pct$year)[c(TRUE, FALSE)]) +
  labs(
    title = "Undocumented adults with no current spouse",
    subtitle = "Spouse absent, separated, divorced, or widowed; ACS, age ≥ 18",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
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

ggsave("results/undoc_no_spouse_pop.png", width = 15, height = 10)

ggplot(undoc_no_spouse_pct, aes(x = year, y = pct)) +
  geom_line(color = "#C97703", linewidth = 1.2) +
  geom_point(color = "#C97703", size = 2) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous(breaks = unique(undoc_no_spouse_pct$year)[c(TRUE, FALSE)]) +
  labs(
    title = "Share of undocumented adults with no current spouse",
    subtitle = "Spouse absent, separated, divorced, or widowed; ACS, age ≥ 18",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
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

ggsave("results/undoc_no_spouse_pct.png", width = 15, height = 10)

# sanity check: undocumented parents who are separated, who is in the household?
separated_parents = acskids %>%
  filter(immig_status == "Undocumented", marst == 3, age >= 18) %>%
  distinct(year, serial, pernum)

nrow(separated_parents)

separated_households = separated_parents %>% distinct(year, serial)

spouse_present_check_sep = acskids %>%
  semi_join(separated_households, by = c("year", "serial")) %>%
  filter(relate == 2) %>%
  distinct(year, serial)

nrow(spouse_present_check_sep)
nrow(separated_households)
nrow(spouse_present_check_sep) / nrow(separated_households) * 100

separated_true_absent = separated_parents %>%
  anti_join(spouse_present_check_sep, by = c("year", "serial"))

nrow(separated_true_absent)

undoc_parents_marital = acskids %>%
  filter(immig_status == "Undocumented", age >= 18, relate %in% c(1, 2)) %>%
  mutate(year_serial_pernum = paste(year, serial, pernum)) 

# flag which separated parents have a spouse still present (the 11%)
sep_with_spouse = separated_parents %>%
  semi_join(spouse_present_check_sep, by = c("year", "serial")) %>%
  mutate(flag = "sep_spouse_present")

undoc_parents_marital = undoc_parents_marital %>%
  left_join(sep_with_spouse %>% select(year, serial, pernum, flag),
            by = c("year", "serial", "pernum")) %>%
  mutate(
    marital_group = case_when(
      marst == 1 ~ "Married, spouse present",
      marst == 3 & !is.na(flag) ~ "Married, spouse present",  # reclassify the 11%
      marst == 2 | marst == 3 ~ "Separated, spouse not present",
      marst %in% c(4, 5) ~ "No spouse (divorced/widowed)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(marital_group))

table(undoc_parents_marital$marital_group)

marital_group_year = undoc_parents_marital %>%
  group_by(year, marital_group) %>%
  summarise(pop = sum(perwt), .groups = "drop")

print(marital_group_year, n = Inf)

ggplot(marital_group_year, aes(x = year, y = pop, color = marital_group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  scale_x_continuous(breaks = unique(marital_group_year$year)[c(TRUE, FALSE)]) +
  scale_color_manual(values = c(
    "Married, spouse present" = "#3043B4",
    "Separated, spouse not present" = "#C97703",
    "No spouse (divorced/widowed)" = "#7C756D")) +
  labs(
    title = "Undocumented parents by marital status",
    subtitle = "ACS; parents defined as coresident household head or spouse",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
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

ggsave("results/undoc_parents_marital_status.png", width = 15, height = 10)

marital_group_year_pct = marital_group_year %>%
  group_by(year) %>%
  mutate(pct = pop / sum(pop) * 100) %>%
  ungroup()

ggplot(marital_group_year_pct, aes(x = year, y = pct, fill = marital_group)) +
  geom_col(position = "fill", width = 0.7) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous(breaks = unique(marital_group_year_pct$year)[c(TRUE, FALSE)]) +
  scale_fill_manual(values = c(
    "Married, spouse present" = "#3043B4",
    "Separated, spouse not present" = "#C97703",
    "No spouse (divorced/widowed)" = "#7C756D")) +
  labs(
    title = "Undocumented parents by marital status",
    subtitle = "Share of total; ACS, parents defined as coresident household head or spouse",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
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
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

ggsave("results/undoc_parents_marital_status_pct.png", width = 15, height = 10)

# restrict to parents only
parent_pernums = acskids %>%
  filter(momloc > 0 | poploc > 0 | momloc2 > 0 | poploc2 > 0) %>%
  select(year, serial, momloc, poploc, momloc2, poploc2) %>%
  pivot_longer(cols = c(momloc, poploc, momloc2, poploc2),
               values_to = "pernum") %>%
  filter(pernum > 0) %>%
  distinct(year, serial, pernum)

undoc_parents_marital = acskids %>%
  filter(immig_status == "Undocumented", age >= 18) %>%
  semi_join(parent_pernums, by = c("year", "serial", "pernum"))

sep_with_spouse = separated_parents %>%
  semi_join(spouse_present_check_sep, by = c("year", "serial")) %>%
  mutate(flag = "sep_spouse_present")

undoc_parents_marital = undoc_parents_marital %>%
  left_join(sep_with_spouse %>% select(year, serial, pernum, flag),
            by = c("year", "serial", "pernum")) %>%
  mutate(
    marital_group = case_when(
    marst == 1 ~ "Married, spouse present",
    marst == 3 & !is.na(flag) ~ "Married, spouse present",  # reclassify the 11%
    marst == 2 | marst == 3 ~ "Separated, spouse not present",
    marst %in% c(4, 5, 6) ~ "No spouse (divorced/widowed/never married)",
    TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(marital_group))

table(undoc_parents_marital$marital_group)

# lookup: undocumented parent's marital_group, keyed by (year, serial, pernum)
parent_marital_dt = as.data.table(undoc_parents_marital)[, .(year, serial, pernum, marital_group)]

acskids2[parent_marital_dt, marital_group_mom  := i.marital_group, on = .(year, serial, momloc  = pernum)]
acskids2[parent_marital_dt, marital_group_pop  := i.marital_group, on = .(year, serial, poploc  = pernum)]
acskids2[parent_marital_dt, marital_group_mom2 := i.marital_group, on = .(year, serial, momloc2 = pernum)]
acskids2[parent_marital_dt, marital_group_pop2 := i.marital_group, on = .(year, serial, poploc2 = pernum)]

acskids2[, marital_group_child := fcoalesce(marital_group_mom, marital_group_pop,
                                             marital_group_mom2, marital_group_pop2)]

kids_undoc_parents = acskids2[!is.na(marital_group_child)]

acskids2[!is.na(marital_group_mom) & !is.na(marital_group_pop) &
         marital_group_mom != marital_group_pop, .N]
acskids2[!is.na(marital_group_mom) & !is.na(marital_group_pop) &
         marital_group_mom != marital_group_pop,
         .N, by = .(marital_group_mom, marital_group_pop)][order(-N)]

medicaid_by_marital = kids_undoc_parents[, .(
    pct_medicaid = 100 * sum(perwt[hinscaid == 2]) / sum(perwt),
    pop_medicaid = sum(perwt[hinscaid == 2]),
    pop = sum(perwt)
  ), by = .(year, marital_group_child)]

print(medicaid_by_marital[order(year, marital_group_child)], nrow = Inf)

ggplot(medicaid_by_marital, aes(x = year, y = pct_medicaid, color = marital_group_child)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous(breaks = unique(medicaid_by_marital$year)[c(TRUE, FALSE)]) +
  scale_color_manual(values = c(
    "Married, spouse present" = "#3043B4",
    "Separated, spouse not present" = "#C97703",
    "No spouse (divorced/widowed/never married)" = "#7C756D")) +
  labs(
    title = "Medicaid coverage among children of undocumented parents",
    subtitle = "By undocumented parent's marital status",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
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

ggsave("results/medicaid_kids_by_undoc_parent_marital_status.png", width = 15, height = 10)

# single parent households
# defined as both single co-resident parent AND that parent has no spouse present in household

# any household head or spouse, 18+, marst == 3 (separated) - check if a spouse still co-resides
separated_adults_all = acskids %>%
  filter(age >= 18, relate %in% c(1, 2), marst == 3) %>%
  distinct(year, serial, pernum)

separated_households_all = separated_adults_all %>% distinct(year, serial)

spouse_present_check_all = acskids %>%
  semi_join(separated_households_all, by = c("year", "serial")) %>%
  filter(relate == 2) %>%
  distinct(year, serial)

sep_with_spouse_all = separated_adults_all %>%
  semi_join(spouse_present_check_all, by = c("year", "serial")) %>%
  mutate(flag_spouse_present = TRUE)

adult_spouse_status = acskids %>%
  filter(age >= 18, relate %in% c(1, 2)) %>%
  left_join(sep_with_spouse_all %>% select(year, serial, pernum, flag_spouse_present),
            by = c("year", "serial", "pernum")) %>%
  mutate(
    spouse_present = case_when(
      marst == 1 ~ TRUE,                              # married, spouse present
      marst == 3 & !is.na(flag_spouse_present) ~ TRUE, # separated, but spouse actually co-resides
      TRUE ~ FALSE                                     # spouse absent, divorced, widowed, never married
    )
  ) %>%
  select(year, serial, pernum, spouse_present)

setDT(adult_spouse_status)

acskids2[adult_spouse_status, spouse_present_mom  := i.spouse_present, on = .(year, serial, momloc  = pernum)]
acskids2[adult_spouse_status, spouse_present_pop  := i.spouse_present, on = .(year, serial, poploc  = pernum)]
acskids2[adult_spouse_status, spouse_present_mom2 := i.spouse_present, on = .(year, serial, momloc2 = pernum)]
acskids2[adult_spouse_status, spouse_present_pop2 := i.spouse_present, on = .(year, serial, poploc2 = pernum)]

acskids2[, n_parents_present := (momloc > 0) + (poploc > 0) + (momloc2 > 0) + (poploc2 > 0)]

# which slot is the (sole) present parent, and is that parent's spouse present?
acskids2[, spouse_present_single := fcase(
  n_parents_present == 1 & momloc  > 0, spouse_present_mom,
  n_parents_present == 1 & poploc  > 0, spouse_present_pop,
  n_parents_present == 1 & momloc2 > 0, spouse_present_mom2,
  n_parents_present == 1 & poploc2 > 0, spouse_present_pop2
)]

acskids2[, single_parent_hh := n_parents_present == 1 & spouse_present_single == FALSE]

table(acskids2$single_parent_hh, useNA = "always")

acskids2[, parent_immig_status_single := fcase(
  n_parents_present == 1 & momloc  > 0, as.character(immig_status_mom),
  n_parents_present == 1 & poploc  > 0, as.character(immig_status_pop),
  n_parents_present == 1 & momloc2 > 0, as.character(immig_status_mom2),
  n_parents_present == 1 & poploc2 > 0, as.character(immig_status_pop2)
)]

single_parent_kids = acskids2[single_parent_hh == TRUE & !is.na(parent_immig_status_single)]

table(single_parent_kids$parent_immig_status_single)

medicaid_single_parent = single_parent_kids[, .(
    pct_medicaid = 100 * sum(perwt[hinscaid == 2]) / sum(perwt),
    pop_medicaid = sum(perwt[hinscaid == 2]),
    pop = sum(perwt)
  ), by = .(year, parent_immig_status_single)]

print(medicaid_single_parent[order(year, parent_immig_status_single)], nrow = Inf)

ggplot(medicaid_single_parent,
       aes(x = year, y = pct_medicaid, color = parent_immig_status_single)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(40, 100)) +
  scale_x_continuous(breaks = unique(medicaid_single_parent$year)[c(TRUE, FALSE)]) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  labs(
    title = "Medicaid coverage among children in single-parent households",
    subtitle = "By single parent's immigration status; single parent = sole co-resident parent, no spouse present",
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

ggsave("results/medicaid_single_parent_by_immig_status.png", width = 15, height = 10)

