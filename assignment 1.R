# ============================================================
# The Weeknd — Trilogy (early) vs later trilogy-era albums
# Output:
#   - weeknd_plot1_mood_map_single.png
#   - weeknd_plot3_fingerprint_heatmap.png
# ============================================================

library(tidyverse)
library(ggrepel)

# ---- file paths (edit if needed) ----
path_trilogy_early <- "Trilogy1.csv"   # Trilogy (2012 compilation)
path_trilogy_late  <- "Trilogy2.csv"   # After Hours / Dawn FM / Hurry Up Tomorrow (your last 3 albums set)

# ---- read data + label corpora ----
early <- read_csv(path_trilogy_early, show_col_types = FALSE) %>%
  mutate(
    corpus = "Early Trilogy (Trilogy)",
    era = "Early"
  )

late <- read_csv(path_trilogy_late, show_col_types = FALSE) %>%
  mutate(
    corpus = "Late Trilogy (AH → DFM → HUT)",
    era = "Late"
  )

df <- bind_rows(early, late) %>%
  mutate(
    `Track Name` = str_squish(`Track Name`),
    `Album Name` = str_squish(`Album Name`),
    track_lc = str_to_lower(`Track Name`)
  )

# ---- tracks to highlight ----
# We match case-insensitively and allow slight name variants (e.g. Timeless feat.)
highlight_tracks <- c(
  "blinding lights",
  "after hours",
  "wicked games",
  "the zone",
  "d.d",
  "timeless",
  "is there someone else"
)

df <- df %>%
  mutate(
    is_highlight =
      track_lc %in% highlight_tracks |
      str_starts(track_lc, "timeless") |
      str_detect(track_lc, "^d\\.d\\.?$") |
      str_detect(track_lc, "^is there someone else\\??$")
  )

# ---- optional: check which tracks got highlighted ----
df %>%
  filter(is_highlight) %>%
  count(`Track Name`, `Album Name`, era, sort = TRUE) %>%
  print(n = 50)

# ---- clean missing values for used features (avoids warnings) ----
# Plot 1 needs: Valence, Energy, Popularity
# Plot 3 needs: Danceability, Energy, Valence, Acousticness, Tempo, Loudness
df <- df %>%
  drop_na(
    Valence, Energy, Popularity,
    Danceability, Acousticness, Tempo, Loudness
  )

# ============================================================
# PLOT 1 — Single-panel Mood map: Valence vs Energy
# Tufte-friendly: minimal grid, no legend, direct labels on highlighted tracks
# ============================================================

crimson <- "#dc143c"
ink <- "#050505"   # extra donker zwart

p1 <- ggplot(df, aes(x = Valence, y = Energy)) +
  # all tracks
  geom_point(aes(color = era, alpha = Popularity), size = 2) +
  
  # highlight tracks on top
  geom_point(
    data = df %>% filter(is_highlight),
    aes(color = era),
    size = 3.4,
    show.legend = FALSE
  ) +
  
  # labels for highlighted tracks only
  geom_text_repel(
    data = df %>% filter(is_highlight),
    aes(label = `Track Name`, color = era),
    size = 3.3,
    min.segment.length = 0,
    box.padding = 0.35,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  
  geom_vline(xintercept = 0.5, linewidth = 0.3, alpha = 0.35, color = ink) +
  geom_hline(yintercept = 0.5, linewidth = 0.3, alpha = 0.35, color = ink) +
  
  scale_alpha(range = c(0.25, 0.9), guide = "none") +
  
  # ✅ dit is de kleur-fix: geen groen/rood meer, maar ink + crimson
  scale_color_manual(values = c("Early" = ink, "Late" = crimson)) +
  
  labs(
    title = "The Weeknd — Mood shift (Valence vs Energy)",
    subtitle = "Single-panel contrast: Early Trilogy vs later trilogy-era albums. Labels = top-streamed tracks per album.",
    x = "Valence (sad/dark → happy/bright)",
    y = "Energy (low → high)"
  ) +
  
  # ✅ white background, black text
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = ink),
    plot.subtitle = element_text(color = ink),
    axis.title = element_text(color = ink),
    axis.text = element_text(color = ink)
  )

ggsave("weeknd_plot1_mood_map_single.png", p1, width = 11, height = 6.2, dpi = 300, bg = "white")

# ============================================================
# PLOT 3 — Highlighted tracks fingerprint heatmap (z-scores)
# ============================================================

crimson <- "#dc143c"
ink <- "#050505"

p3 <- ggplot(df_hi, aes(x = feature, y = track_display, fill = z)) +
  geom_tile(linewidth = 0.2, alpha = 0.95, color = "white") +
  
  scale_fill_gradient2(
    low = ink,
    mid = "white",
    high = crimson,
    midpoint = 0
  ) +
  
  labs(
    title = "Highlighted tracks — audio feature fingerprint (z-scores)",
    subtitle = "0 = average across your whole corpus; positive = above-average, negative = below-average",
    x = NULL, y = NULL
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 9, color = ink),
    axis.text.x = element_text(color = ink),
    plot.title = element_text(color = ink),
    plot.subtitle = element_text(color = ink),
    legend.title = element_text(color = ink),
    legend.text = element_text(color = ink)
  )

ggsave("weeknd_plot3_fingerprint_heatmap.png", p3, width = 11, height = 6.6, dpi = 300, bg = "white")

# ---- show plots in viewer ----
p1
p3