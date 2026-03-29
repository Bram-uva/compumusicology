# ============================================================
# SSM met timbre features (MFCC)
# ============================================================

library(tidyverse)
library(compmus)

# Kies 3 tracks
tracks <- c("Blinding Lights", "Wicked Games", "After Hours")

mfcc_data <- chroma_raw |>  # als je MFCC CSV hebt, vervang dit
  filter(track %in% tracks)

# Als je MFCC CSVs hebt → beter:
# mfcc_data <- imap_dfr(mfcc_files, ...)

compute_ssm <- function(df) {
  df |>
    compmus_wrangle_mfcc() |>
    mutate(
      mfcc = map(mfcc, compmus_normalise, "euclidean")
    ) |>
    compmus_self_similarity(
      feature = mfcc,
      method = "cosine"
    ) |>
    filter(!is.nan(d)) |>
    ggplot(aes(
      x = xstart + xduration / 2,
      y = ystart + yduration / 2,
      fill = d
    )) +
    geom_tile() +
    coord_equal() +
    theme_minimal() +
    scale_fill_viridis_c() +
    labs(
      title = paste("SSM (Timbre) —", unique(df$track)),
      x = "Time (s)",
      y = "Time (s)",
      fill = "Similarity"
    )
}

# Plotten
mfcc_data |>
  group_split(track) |>
  walk(\(d) print(compute_ssm(d)))