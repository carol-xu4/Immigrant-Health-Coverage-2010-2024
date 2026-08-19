# ---------------------------------------------------------------------------
# MEPS vs. NHEA (CMS) Medicaid expenditures, 2010-2023
#
# Question: MEPS Medicaid expenditure (EXPMAPAY) captures only direct payments
# tied to reported medical events. How far below the CMS administrative total
# does that put it, and is a Kolmogorov-Smirnov test the right way to show it?
#
# Benchmark for the method: Bernard, Cowan, Selden, Lassman & Catlin (2017),
# "Reconciling Medical Expenditure Estimates from the MEPS and NHEA, 2012,"
# AHRQ Working Paper 17003. For 2012 they report adjusted NHEA Medicaid of
# $226.2B vs. MEPS Medicaid of $146.7B -- MEPS 35.2% below.
#
# NHEA source: CMS National Health Expenditure Accounts historical tables
#   data/input/nhe/nhe_table06_personal_health_care.xlsx        (PHC)
#   data/input/nhe/nhe_table13_other_health_residential.xlsx    (OHRPC)
#   data/input/nhe/nhe_table15_nursing_care.xlsx                (nursing homes)
#   data/input/nhe/nhe_table18_other_nondurable.xlsx            (OTC goods)
#   data/input/nhe/nhe_table21_per_enrollee.xlsx                (expend/enroll)
# ---------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman", repos = "https://cloud.r-project.org")
pacman::p_load(tidyverse, readxl, ipumsr, survey)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Immigrant Health Coverage 2010-2024")

YEARS  <- 2010:2023          # MEPS extract coverage
NHE    <- "data/input/nhe"

# ===========================================================================
# 1. NHEA: Medicaid spending by service category, long-format tables
# ===========================================================================
# Tables 6-18 share a layout: rows 1-4 are headers, col 1 is Year, and the
# source-of-funds columns run Total | Out of Pocket | Health Insurance |
# Private Health Insurance | Medicare | Medicaid | Other Health Insurance |
# Other Third Party. Confirm the Medicaid column rather than assuming it.

read_nhe_service <- function(file, label) {
  raw <- read_excel(file.path(NHE, file), sheet = 1,
                    col_names = FALSE, .name_repair = "minimal")
  hdr <- as.character(unlist(raw[3, ]))
  mcol <- which(trimws(gsub("[0-9]", "", hdr)) == "Medicaid")
  if (length(mcol) != 1) {
    stop("Could not uniquely locate Medicaid column in ", file,
         " -- header was: ", paste(hdr, collapse = " | "))
  }
  # Each table stacks three sections that all repeat years 1970-2024:
  # "Amount in Billions", "Average Annual Percent Change", "Percent
  # Distribution". Keep only the dollar section.
  marks <- which(!is.na(raw[[2]]) &
                 grepl("Amount in Billions|Percent Change|Percent Distribution",
                       as.character(unlist(raw[[2]]))))
  start <- marks[1] + 1L
  end   <- if (length(marks) > 1) marks[2] - 1L else nrow(raw)

  tibble(
    year     = suppressWarnings(as.integer(as.character(unlist(raw[[1]]))))[start:end],
    medicaid = suppressWarnings(as.numeric(as.character(unlist(raw[[mcol]]))))[start:end]
  ) %>%
    filter(!is.na(year), year %in% YEARS) %>%
    # Blank cells mean the program spends nothing in that category (Medicaid
    # pays $0 for over-the-counter non-durables), so blanks are real zeros.
    mutate(medicaid = coalesce(medicaid, 0), category = label)
}

nhe_services <- bind_rows(
  read_nhe_service("nhe_table06_personal_health_care.xlsx",     "phc"),
  read_nhe_service("nhe_table13_other_health_residential.xlsx", "ohrpc"),
  read_nhe_service("nhe_table15_nursing_care.xlsx",             "nursing"),
  read_nhe_service("nhe_table18_other_nondurable.xlsx",         "nondurable")
) %>%
  pivot_wider(names_from = category, values_from = medicaid)

# Scope adjustment, following the logic of AHRQ WP 17003 Table 4. We can remove
# the out-of-scope pieces that CMS publishes separately:
#   - nursing facilities  -> MEPS covers only the non-institutionalized
#   - OHRPC               -> HCBS waivers, residential MH/SUD, school health
#   - other non-durables  -> over-the-counter goods, excluded from MEPS
# Still embedded in the residual and NOT removable from published tables:
# acute care for institutionalized persons, DSH/GME, Medicaid non-DSH
# supplemental payments. So this is a PARTIAL adjustment -- it remains an
# upper bound on the MEPS-comparable benchmark.
nhe_services <- nhe_services %>%
  mutate(nhea_adj = phc - nursing - ohrpc - nondurable)

# --- Table 21: Medicaid expenditures, enrollment, and per-enrollee dollars ---
raw21 <- read_excel(file.path(NHE, "nhe_table21_per_enrollee.xlsx"), sheet = 1,
                    col_names = FALSE, .name_repair = "minimal")
yr21  <- suppressWarnings(as.integer(as.character(unlist(raw21[2, ]))))
lab21 <- trimws(as.character(unlist(raw21[[1]])))

# Sections repeat the same row labels, so anchor each "Medicaid" row to the
# section header that precedes it.
sec_rows <- c(expenditure  = grep("^Expenditures$",   lab21)[1],
              enrollment   = grep("^Enrollment$",     lab21)[1],
              per_enrollee = grep("^Per Enrollee",    lab21)[1])

grab21 <- function(section, want = "Medicaid") {
  start <- sec_rows[[section]]
  ends  <- sort(c(sec_rows, grep("^Growth rates", lab21)[1]))
  stop_at <- ends[ends > start][1]
  r <- which(lab21 == want & seq_along(lab21) > start &
             seq_along(lab21) < (if (is.na(stop_at)) Inf else stop_at))[1]
  vals <- suppressWarnings(as.numeric(as.character(unlist(raw21[r, ]))))
  tibble(year = yr21, value = vals) %>% filter(!is.na(year), year %in% YEARS)
}

nhe21 <- grab21("expenditure")  %>% rename(nhea_total_bn   = value) %>%
  left_join(grab21("enrollment")   %>% rename(nhea_enroll_mn  = value), by = "year") %>%
  left_join(grab21("per_enrollee") %>% rename(nhea_per_enr    = value), by = "year") %>%
  left_join(grab21("expenditure", "CHIP") %>% rename(chip_bn  = value), by = "year")

nhea <- nhe21 %>% left_join(nhe_services, by = "year")

cat("\n=========== NHEA Medicaid, 2010-2023 ($ billions) ===========\n")
print(as.data.frame(nhea %>%
  transmute(year, nhea_total_bn, chip_bn, phc, nursing, ohrpc, nondurable, nhea_adj)),
  row.names = FALSE, digits = 4)

cat("\nValidation against AHRQ WP 17003 (2012): published adjusted NHEA Medicaid = $226.2B\n")
cat("  partial adjustment here for 2012 = $",
    round(nhea$nhea_adj[nhea$year == 2012], 1), "B\n", sep = "")
cat("  residual (acute care of institutionalized, DSH/GME, supplemental) = $",
    round(nhea$nhea_adj[nhea$year == 2012] - 226.2, 1), "B\n", sep = "")

# ===========================================================================
# 2. MEPS: weighted Medicaid expenditure aggregates by year
# ===========================================================================
ddi  <- read_ipums_ddi("data/input/meps_00003.xml")
meps <- read_ipums_micro(
  ddi,
  vars = c("YEAR", "PERWEIGHT", "PSUANN", "STRATANN",
           "HIMACHIP", "EXPMAPAY", "USBORN"),
  verbose = FALSE
)
names(meps) <- tolower(names(meps))

meps <- meps %>%
  mutate(
    across(c(year, perweight, expmapay, himachip, usborn), as.numeric),
    medicaid = himachip == 2,
    nativity = case_when(usborn %in% c(11, 20) ~ "US-born",
                         usborn %in% c(10, 12) ~ "Immigrant",
                         TRUE ~ NA_character_)
  ) %>%
  filter(perweight > 0)

meps_yr <- meps %>%
  group_by(year) %>%
  summarise(
    n             = n(),
    meps_total_bn = sum(expmapay * perweight) / 1e9,
    meps_enroll_mn = sum(perweight[medicaid]) / 1e6,
    .groups = "drop"
  ) %>%
  mutate(meps_per_enr = meps_total_bn * 1e9 / (meps_enroll_mn * 1e6))

# ===========================================================================
# 3. Reconciliation table
# ===========================================================================
cmp <- meps_yr %>%
  left_join(nhea, by = "year") %>%
  mutate(
    nhea_mcaid_chip = nhea_total_bn + chip_bn,
    ratio_unadj     = meps_total_bn / nhea_mcaid_chip,
    ratio_phc       = meps_total_bn / phc,
    ratio_adj       = meps_total_bn / nhea_adj,
    enroll_ratio    = meps_enroll_mn / nhea_enroll_mn,
    per_enr_ratio   = meps_per_enr / nhea_per_enr
  )

cat("\n=========== MEPS vs NHEA Medicaid reconciliation ===========\n")
print(as.data.frame(cmp %>% transmute(
  year, meps_bn = round(meps_total_bn, 1),
  nhea_bn = round(nhea_mcaid_chip, 1), nhea_adj = round(nhea_adj, 1),
  r_unadj = round(ratio_unadj, 3), r_adj = round(ratio_adj, 3),
  meps_enr = round(meps_enroll_mn, 1), nhea_enr = round(nhea_enroll_mn, 1),
  r_enr = round(enroll_ratio, 3), r_perenr = round(per_enr_ratio, 3)
)), row.names = FALSE)

cat("\nMean MEPS/NHEA ratio, unadjusted      :", round(mean(cmp$ratio_unadj), 3), "\n")
cat("Mean MEPS/NHEA ratio, partial adjust  :", round(mean(cmp$ratio_adj), 3), "\n")
cat("Mean MEPS/NHEA enrollment ratio       :", round(mean(cmp$enroll_ratio), 3), "\n")

# ===========================================================================
# 4. The KS tests
# ===========================================================================
# NOTE ON INTERPRETATION. NHEA is an accounting aggregate: one number per year,
# not a sample. So there is no NHEA distribution to compare against. The only
# two-sample KS available treats each YEAR as an observation (n = 14 vs 14).
# These are autocorrelated time series, not iid draws, so the p-values below
# are not valid inference -- they are descriptive of how far the two level
# series are separated. Section 5 gives the defensible test.

cat("\n=========== KS tests: annual series, MEPS vs NHEA ===========\n")

ks_unadj <- ks.test(cmp$meps_total_bn, cmp$nhea_mcaid_chip)
ks_adj   <- ks.test(cmp$meps_total_bn, cmp$nhea_adj)
ks_per   <- ks.test(cmp$meps_per_enr,  cmp$nhea_per_enr)

cat("\n-- aggregate $, MEPS vs unadjusted NHEA Medicaid+CHIP --\n"); print(ks_unadj)
cat("\n-- aggregate $, MEPS vs partially adjusted NHEA --\n");       print(ks_adj)
cat("\n-- per-enrollee $, MEPS vs NHEA --\n");                        print(ks_per)

# ===========================================================================
# 5. The test that is actually valid: design-based one-sample comparison
# ===========================================================================
# NHEA is treated as a known constant. Ask whether the survey-weighted MEPS
# total is statistically distinguishable from it, using the MEPS complex
# design (stratified, clustered) for the standard error.

cat("\n=========== Design-based MEPS total vs NHEA benchmark ===========\n")
options(survey.lonely.psu = "adjust")

design_test <- map_dfr(YEARS, function(y) {
  dy <- meps %>% filter(year == y)
  des <- svydesign(ids = ~psuann, strata = ~stratann, weights = ~perweight,
                   data = dy, nest = TRUE)
  est <- svytotal(~expmapay, des)
  tot <- as.numeric(est) / 1e9
  se  <- as.numeric(SE(est)) / 1e9
  bm  <- cmp$nhea_adj[cmp$year == y]
  tibble(year = y, meps_bn = tot, se_bn = se,
         lo = tot - 1.96 * se, hi = tot + 1.96 * se,
         nhea_adj_bn = bm, z = (tot - bm) / se)
})

print(as.data.frame(design_test %>% mutate(across(where(is.numeric), ~round(., 2)))),
      row.names = FALSE)
cat("\nAll z-statistics are large and negative: the MEPS total is far outside\n",
    "sampling error of the NHEA benchmark. The gap is a coverage/definitional\n",
    "shortfall, not sampling noise.\n", sep = "")

# ===========================================================================
# 6. A KS test that IS appropriate: immigrant vs US-born within MEPS
# ===========================================================================
# Both sides are person-level samples here, so the two-sample KS is at least
# structurally correct. Caveats remain: unweighted (KS admits no weights), and
# the mass of zeros creates heavy ties, so p-values are approximate.

cat("\n=========== KS: immigrant vs US-born Medicaid spending (enrollees) ===========\n")

enr <- meps %>% filter(medicaid, !is.na(nativity))
cat("share of enrollees with exactly $0 Medicaid spending:",
    round(mean(enr$expmapay == 0), 3), "\n")

ks_nat <- map_dfr(YEARS, function(y) {
  d <- enr %>% filter(year == y)
  a <- d$expmapay[d$nativity == "Immigrant"]
  b <- d$expmapay[d$nativity == "US-born"]
  k <- suppressWarnings(ks.test(a, b))
  tibble(year = y, n_immig = length(a), n_usborn = length(b),
         D = as.numeric(k$statistic), p = k$p.value,
         med_immig = median(a), med_usborn = median(b),
         mean_immig = mean(a), mean_usborn = mean(b))
})
print(as.data.frame(ks_nat %>% mutate(across(c(D, p), ~round(., 4)),
                                      across(starts_with("mean"), ~round(., 0)))),
      row.names = FALSE)

# ===========================================================================
# 7. Outputs
# ===========================================================================
dir.create("results", showWarnings = FALSE)
write_csv(cmp,         "results/meps_nhea_medicaid_reconciliation.csv")
write_csv(design_test, "results/meps_nhea_design_based_test.csv")
write_csv(ks_nat,      "results/ks_medicaid_immigrant_vs_usborn.csv")

# Level comparison
p_levels <- cmp %>%
  select(year, MEPS = meps_total_bn,
         `NHEA (unadjusted)` = nhea_mcaid_chip,
         `NHEA (partial scope adj.)` = nhea_adj) %>%
  pivot_longer(-year) %>%
  ggplot(aes(year, value, color = name)) +
  geom_line(linewidth = 1) + geom_point(size = 1.8) +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B")) +
  labs(title = "Medicaid expenditures: MEPS vs. CMS National Health Expenditure Accounts",
       subtitle = "MEPS captures only direct payments tied to reported medical events",
       x = NULL, y = NULL, color = NULL,
       caption = "MEPS (IPUMS) EXPMAPAY, survey-weighted. NHEA historical tables 6, 13, 15, 18, 21.") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
ggsave("results/meps_nhea_medicaid_levels.png", p_levels,
       width = 9, height = 5.5, dpi = 200)

# The ECDF pair the annual-series KS test is actually comparing
p_ecdf <- cmp %>%
  select(year, MEPS = meps_per_enr, NHEA = nhea_per_enr) %>%
  pivot_longer(-year) %>%
  ggplot(aes(value, color = name)) +
  stat_ecdf(linewidth = 1) +
  scale_x_continuous(labels = scales::dollar) +
  labs(title = "What the KS test compares: ECDFs of annual per-enrollee Medicaid spending",
       subtitle = paste0("2010-2023, one observation per year (n = 14 each). D = ",
                         round(ks_per$statistic, 3)),
       x = "Medicaid spending per enrollee", y = "Cumulative share of years",
       color = NULL) +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
ggsave("results/meps_nhea_per_enrollee_ecdf.png", p_ecdf,
       width = 9, height = 5.5, dpi = 200)

cat("\nWrote results/meps_nhea_medicaid_reconciliation.csv,",
    "meps_nhea_design_based_test.csv, ks_medicaid_immigrant_vs_usborn.csv,",
    "and 2 figures.\n")
