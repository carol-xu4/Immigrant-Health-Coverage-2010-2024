if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Immigrant Health Coverage 2010-2024")

acsdata = fread("data/output/acsdata.csv")

fips_to_abb = c(
  "1"="AL","2"="AK","4"="AZ","5"="AR","6"="CA","8"="CO","9"="CT","10"="DE",
  "11"="DC","12"="FL","13"="GA","15"="HI","16"="ID","17"="IL","18"="IN","19"="IA",
  "20"="KS","21"="KY","22"="LA","23"="ME","24"="MD","25"="MA","26"="MI","27"="MN",
  "28"="MS","29"="MO","30"="MT","31"="NE","32"="NV","33"="NH","34"="NJ","35"="NM",
  "36"="NY","37"="NC","38"="ND","39"="OH","40"="OK","41"="OR","42"="PA","44"="RI",
  "45"="SC","46"="SD","47"="TN","48"="TX","49"="UT","50"="VT","51"="VA","53"="WA",
  "54"="WV","55"="WI","56"="WY")

# immig status by state, 2024
immig_counts_state_2024 = acsdata %>%
    filter(year == 2024) %>%
    group_by(statefip, immig_status) %>%
    summarise(pop = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = immig_status, values_from = pop, values_fill = 0) %>%
    mutate(total_pop = rowSums(across(-statefip)))

immig_counts_state_2024 = immig_counts_state_2024 %>%
  mutate(state_abb = fips_to_abb[as.character(statefip)]) %>%
  relocate(state_abb, .after = statefip)

print(immig_counts_state_2024, n = Inf)

write_csv(immig_counts_state_2024, "results/2024_immig_counts_state")

# immig status by state, 2023
immig_counts_state_2023 = acsdata %>%
    filter(year == 2023) %>%
    group_by(statefip, immig_status) %>%
    summarise(pop = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = immig_status, values_from = pop, values_fill = 0) %>%
    mutate(total_pop = rowSums(across(-statefip)))

immig_counts_state_2023 = immig_counts_state_2023 %>%
  mutate(state_abb = fips_to_abb[as.character(statefip)]) %>%
  relocate(state_abb, .after = statefip)

print(immig_counts_state_2023, n = Inf)

write_csv(immig_counts_state_2023, "results/2023_immig_counts_state")

