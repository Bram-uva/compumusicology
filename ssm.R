library(tidyverse)
library(compmus)

# ---- TRACK KIEZEN ----
track_name <- "After Hours"

files <- c(
  "Chromatogram The_Zone.csv",
  "Chromatogram Wicked_Games.csv",
  "Chromatogram Timeless.csv",
  "Chromatogram Blinding_Lights.csv",
  "Chromatogram After_Hours.csv",
  "Chromatogram Is_There_Someone_Else.csv",
  "Chromatogram D.D..csv"
)

names(files) <- c(
  "The Zone",
  "Wicked Games",
  "Timeless",
  "Blinding Lights",
  "After Hours",
  "Is There Someone Else",
  "D.D."
)

# ---- LOAD ----
chroma_raw <- imap_dfr(
  files,
  ~ read_csv(.x, show_col_types = FALSE) |> mutate(track = .y)
)

# ---- PREPARE CHROMA ----
track_data <- chroma_raw |>
  filter(track == track_name) |>
  compmus_wrangle_chroma() |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  filter(row_number() %% 10L == 0L)

# ---- SSM ----
ssm <- compmus_long_distance(
  track_data,
  track_data,
  feature = pitches,
  method = "cosine"
)

# ---- PLOT ----
ssm_plot <- ssm |>
  filter(!is.nan(d)) |>
  ggplot(aes(
    x = xstart + xduration / 2,
    width = xduration,
    y = ystart + yduration / 2,
    height = yduration,
    fill = d
  )) +
  geom_tile() +
  coord_equal() +
  theme_minimal() +
  scale_fill_viridis_c(direction = -1) +
  labs(
    title = paste("Self-Similarity Matrix —", track_name),
    x = "Time (s)",
    y = "Time (s)"
  )

print(ssm_plot)