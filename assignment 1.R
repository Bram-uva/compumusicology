# ============================================================
# The Weeknd — density plot of full datasets
# Trilogy1 vs Trilogy2
# Output:
#   - weeknd_plot2_feature_densities.png
# ============================================================

library(tidyverse)

# ---- file paths ----
path_trilogy_early <- "Trilogy1.csv"
path_trilogy_late  <- "Trilogy2.csv"

# ---- read data + label corpora ----
early <- read_csv(path_trilogy_early, show_col_types = FALSE) %>%
  mutate(
    corpus = "Trilogy 1",
    era = "Trilogy 1"
  )

late <- read_csv(path_trilogy_late, show_col_types = FALSE) %>%
  mutate(
    corpus = "Trilogy 2",
    era = "Trilogy 2"
  )

df <- bind_rows(early, late)

# ---- select features for density plots ----
features <- c(
  "Acousticness",
  "Danceability",
  "Energy",
  "Liveness",
  "Loudness",
  "Speechiness",
  "Tempo",
  "Valence"
)

# ---- reshape to long format ----
df_long <- df %>%
  select(all_of(features), era) %>%
  pivot_longer(
    cols = all_of(features),
    names_to = "feature",
    values_to = "value"
  ) %>%
  drop_na(value)

# ---- labels for facets ----
feature_labels <- c(
  Acousticness = "Acousticness",
  Danceability = "Danceability",
  Energy = "Energy",
  Liveness = "Liveness",
  Loudness = "Loudness",
  Speechiness = "Speechiness",
  Tempo = "Tempo",
  Valence = "Valence"
)

# ---- color palette inspired by your site ----
burgundy <- "#5C0011"   # donkerrood/navbar vibe
magenta  <- "#0F3A4A"  # fel roze accent
bg_light <- "#F5F2F4"   # zachte lichte achtergrond
ink      <- "#111111"

# ---- plot ----
p2 <- ggplot(df_long, aes(x = value, fill = era, color = era)) +
  geom_density(alpha = 0.35, linewidth = 1) +
  facet_wrap(
    ~ feature,
    scales = "free",
    ncol = 2,
    labeller = as_labeller(feature_labels)
  ) +
  scale_fill_manual(values = c(
    "Trilogy 1" = burgundy,
    "Trilogy 2" = magenta
  )) +
  scale_color_manual(values = c(
    "Trilogy 1" = burgundy,
    "Trilogy 2" = magenta
  )) +
  labs(
    title = "Musical characteristics across The Weeknd datasets",
    subtitle = "Comparing Spotify audio features: Trilogy 1 vs Trilogy 2",
    x = NULL,
    y = "Density",
    fill = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = ink),
    plot.subtitle = element_text(size = 13, color = ink),
    strip.text = element_text(face = "bold", size = 12, color = ink),
    axis.title = element_text(color = ink),
    axis.text = element_text(color = ink),
    legend.position = "top",
    legend.text = element_text(size = 12, color = ink),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#D9D9D9", linewidth = 0.4),
    panel.background = element_rect(fill = bg_light, color = NA),
    plot.background = element_rect(fill = bg_light, color = NA)
  )

# ---- save ----
ggsave(
  "weeknd_plot2_feature_densities.png",
  p2,
  width = 10.5,
  height = 8,
  dpi = 300,
  bg = bg_light
)

# ---- show plot ----
p2