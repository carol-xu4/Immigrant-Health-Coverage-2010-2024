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
