# video
install.packages(c("gganimate", "gifski"))
library(gganimate)
library(gifski)

# age distribution of medicaid enrollees, native-born vs. all immigrants
age_medicaid_all = acsdata %>%
  filter(hinscaid == 2) %>%
  mutate(group = ifelse(as.character(immig_status) == "Native-born",
                        "Native-born", "Immigrants"))

ACS_age_density_gif = ggplot(age_medicaid_all, aes(x = age, fill = group, color = group, weight = perwt)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c(
    "Native-born" = "#3043B4",
    "Immigrants"  = "#C97703")) +
  scale_color_manual(values = c(
    "Native-born" = "#3043B4",
    "Immigrants"  = "#C97703")) +
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  labs(
    title = "Age Distribution of Medicaid Enrollees — {closest_state}",
    subtitle = "Native-born vs. all immigrants",
    x = "Age",
    y = NULL,
    fill = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 10, color = "gray40"),
    axis.text.y = element_blank(),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +

  transition_states(year, transition_length = 2, state_length = 1) +
  ease_aes("cubic-in-out")

animate(ACS_age_density_gif,
        nframes   = 75,
        fps       = 20,
        width     = 1000,
        height    = 600,
        renderer  = gifski_renderer("results/ACS_age_density.gif"))


# age distribution of medicaid enrollees, by immigration status
age_medicaid_all4 = acsdata %>%
  filter(hinscaid == 2)

ACS_age_density_gif4 = ggplot(age_medicaid_all4, aes(x = age, fill = immig_status, color = immig_status, weight = perwt)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  geom_vline(xintercept = 18, linetype = "dashed", color = "gray70", linewidth = 0.5) +
  geom_vline(xintercept = 65, linetype = "dashed", color = "gray70", linewidth = 0.5) +
  annotate("text", x = 18.5, y = Inf, label = "Age 18", vjust = 1.5,
           hjust = 0, size = 3, color = "gray50") +
  annotate("text", x = 65.5, y = Inf, label = "Age 65", vjust = 1.5,
           hjust = 0, size = 3, color = "gray50") +
  labs(
    title = "Age Distribution of Medicaid Enrollees — {closest_state}",
    subtitle = "ACS",
    x = "Age",
    y = NULL,
    fill = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 10, color = "gray40"),
    axis.text.y = element_blank(),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)) +
  transition_states(year, transition_length = 2, state_length = 1) +
  ease_aes("cubic-in-out")

animate(ACS_age_density_gif4,
        nframes   = 75,
        fps       = 20,
        width     = 1000,
        height    = 600,
        renderer  = gifski_renderer("results/ACS_age_density_4groups.gif"))

# Uninsured trend
ACS_uninsured_gif = ggplot(uninsured_trend, 
                             aes(x = as.numeric(year), y = uninsured_rate, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 0.70, by = 0.05),
    limits = c(0, 0.70),
    expand = c(0.02, 0)) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = 2014.1, y = 0.65, label = "ACA (2014)",
           hjust = 0, size = 3, color = "gray50") +
  labs(
    title = "Uninsured Rate by Immigration Status (2010–2024)",
    subtitle = "ACS",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS PUMS via IPUMS, authors' calculations") +
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
    panel.background = element_rect(fill = "white", color = NA)) +
  transition_reveal(year)

animate(ACS_uninsured_gif,
        nframes   = 150,
        fps       = 15,
        width     = 1000,
        height    = 600,
        res       = 100,
        renderer  = gifski_renderer("results/ACS_uninsured_animated.gif"))

# california age density
ca_medicaid_all = acsdata %>%
  filter(hinscaid == 2, statefip == 6)

CA_age_density_gif = ggplot(ca_medicaid_all, aes(x = age, fill = immig_status, color = immig_status, weight = perwt)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703"
  )) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703"
  )) +
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  geom_vline(xintercept = 18, linetype = "dashed", color = "gray70", linewidth = 0.5) +
  geom_vline(xintercept = 65, linetype = "dashed", color = "gray70", linewidth = 0.5) +
  annotate("text", x = 18.5, y = Inf, label = "Age 18", vjust = 1.5,
           hjust = 0, size = 3, color = "gray50") +
  annotate("text", x = 65.5, y = Inf, label = "Age 65", vjust = 1.5,
           hjust = 0, size = 3, color = "gray50") +
  labs(
    title = "Age Distribution of Medicaid Enrollees — California ({closest_state})",
    subtitle = "ACS",
    x = "Age",
    y = NULL,
    fill = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 10, color = "gray40"),
    axis.text.y = element_blank(),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)) +
  transition_states(year, transition_length = 2, state_length = 1) +
  ease_aes("cubic-in-out")

animate(CA_age_density_gif,
        nframes   = 75,
        fps       = 20,
        width     = 1000,
        height    = 600,
        res       = 100,
        renderer  = gifski_renderer("results/CA_age_density.gif"))

# Coverage rates for all immigration status
coverage_all <- coverage_counts %>%
  group_by(year, immig_status) %>%
  mutate(rate = population / sum(population)) %>%
  ungroup() %>%
  mutate(coverage_type = factor(coverage_type, levels = c(
    "Employer-sponsored",
    "Direct purchase",
    "Medicaid",
    "Medicare",
    "Other public",
    "Uninsured",
    "Unknown")))

ACS_coverage_gif <- ggplot(coverage_all, aes(x = immig_status, y = rate, fill = coverage_type)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.1)) +
  scale_fill_manual(values = c(
    "Employer-sponsored" = "#3043B4",
    "Direct purchase"    = "#7C756D",
    "Medicaid"           = "#C97703",
    "Medicare"           = "#0D0E51",
    "Other public"       = "#6B8E23",
    "Uninsured"          = "#C0392B",
    "Unknown"            = "gray80"
  )) +
  labs(
    title = "Health Insurance Coverage by Immigration Status — {closest_state}",
    subtitle = "ACS",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "Source: ACS PUMS via IPUMS"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  transition_states(year, transition_length = 2, state_length = 1) +
  ease_aes("cubic-in-out")

animate(ACS_coverage_gif,
        nframes   = 75,
        fps       = 10,
        width     = 1000,
        height    = 600,
        res       = 100,
        renderer  = gifski_renderer("results/ACS_coverage_animated.gif"))

# medicaid rate by age, over time
medicaid_age = acsdata %>%
    mutate(medicaid = ifelse(hinscaid == 2, perwt, 0)) %>%
    group_by(year, immig_status, age) %>%
    summarise(
        total_pop = sum(perwt, na.rm = TRUE),
        medicaid = sum(medicaid, na.rm = TRUE),
        .groups = "drop") %>%
    mutate(medicaid_rate = medicaid / total_pop)

write_csv(medicaid_age, "results/ACS_medicaid_age.csv")

ACS_medicaid_age_gif <- ggplot(medicaid_age,
                                aes(x = age, y = medicaid_rate, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703")) +
  scale_x_continuous(breaks = seq(0, 100, by = 10), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.10),
    expand = c(0.02, 0)) +
  geom_vline(xintercept = 18, linetype = "dashed", color = "gray70", linewidth = 0.5) +
  geom_vline(xintercept = 65, linetype = "dashed", color = "gray70", linewidth = 0.5) +
  annotate("text", x = 18.5, y = 0.95, label = "Age 18",
           hjust = 0, size = 3, color = "gray50") +
  annotate("text", x = 65.5, y = 0.95, label = "Age 65",
           hjust = 0, size = 3, color = "gray50") +
  labs(
    title = "Medicaid Rate by Age and Immigration Status — {closest_state}",
    subtitle = "ACS",
    x = "Age",
    y = NULL,
    color = NULL,
    caption = "Source: ACS PUMS via IPUMS, authors' calculations") +
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
    panel.background = element_rect(fill = "white", color = NA)) +
  transition_states(year, transition_length = 2, state_length = 1) +
  ease_aes("cubic-in-out")

animate(ACS_medicaid_age_gif,
        nframes   = 75,
        fps       = 10,
        width     = 1000,
        height    = 600,
        res       = 100,
        renderer  = gifski_renderer("results/ACS_medicaid_age_animated.gif"))

# California medicaid rate by age over time
# california only
ca_medicaid_age <- acsdata %>%
  filter(statefip == 6) %>%
  mutate(medicaid = ifelse(hinscaid == 2, perwt, 0)) %>%
  group_by(year, immig_status, age) %>%
  summarise(
    total_pop     = sum(perwt, na.rm = TRUE),
    medicaid      = sum(medicaid, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(medicaid_rate = medicaid / total_pop)

CA_medicaid_age_gif <- ggplot(ca_medicaid_age,
                               aes(x = age, y = medicaid_rate, color = immig_status)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c(
    "Native-born"         = "#3043B4",
    "Naturalized citizen" = "#0D0E51",
    "Legal immigrant"     = "#7C756D",
    "Undocumented"        = "#C97703"
  )) +
  scale_x_continuous(breaks = seq(0, 85, by = 10), limits = c(0, 85), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.10),
    expand = c(0.02, 0)
  ) +
  geom_vline(xintercept = 18, linetype = "dashed", color = "gray70", linewidth = 0.5) +
  geom_vline(xintercept = 50, linetype = "dashed", color = "gray70", linewidth = 0.5) +
  geom_vline(xintercept = 65, linetype = "dashed", color = "gray70", linewidth = 0.5) +
  annotate("text", x = 18.5, y = 0.95, label = "Age 18",
           hjust = 0, size = 3, color = "gray50") +
  annotate("text", x = 50.5, y = 0.95, label = "Age 50 (Medi-Cal)",
           hjust = 0, size = 3, color = "gray50") +
  annotate("text", x = 65.5, y = 0.95, label = "Age 65",
           hjust = 0, size = 3, color = "gray50") +
  labs(
    title = "Medicaid Rate by Age and Immigration Status — California ({closest_state})",
    subtitle = "ACS",
    x = "Age",
    y = NULL,
    color = NULL,
    caption = "Source: ACS PUMS via IPUMS, authors' calculations"
  ) +
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
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  transition_states(year, transition_length = 2, state_length = 1) +
  ease_aes("cubic-in-out")

animate(CA_medicaid_age_gif,
        nframes   = 75,
        fps       = 10,
        width     = 1000,
        height    = 600,
        res       = 100,
        renderer  = gifski_renderer("results/CA_medicaid_age_animated.gif"))
