library(tidyverse)
library(compmus)
library(zoo)

# ============================================================
# 1) Helpers
# ============================================================

pitch_names <- c("C", "C#", "D", "D#", "E", "F",
                 "F#", "G", "G#", "A", "A#", "B")

rotate_vec <- function(x, n) {
  n <- n %% length(x)
  c(tail(x, -n), head(x, n))
}

normalise_vec <- function(x) {
  s <- sqrt(sum(x^2))
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  x / s
}

cosine_similarity <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  if (any(is.na(x)) || any(is.na(y))) return(0)
  
  denom <- sqrt(sum(x^2)) * sqrt(sum(y^2))
  if (is.na(denom) || denom == 0) return(0)
  
  sum(x * y) / denom
}

# ============================================================
# 2) Chord templates
# ============================================================

make_chord_templates <- function() {
  major_template <- c(1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0)
  minor_template <- c(1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0)
  
  major_chords <- map_dfr(0:11, function(i) {
    tibble(
      chord = paste0(pitch_names[i + 1], ":maj"),
      template = list(normalise_vec(rotate_vec(major_template, i)))
    )
  })
  
  minor_chords <- map_dfr(0:11, function(i) {
    tibble(
      chord = paste0(pitch_names[i + 1], ":min"),
      template = list(normalise_vec(rotate_vec(minor_template, i)))
    )
  })
  
  bind_rows(major_chords, minor_chords)
}

chord_templates <- make_chord_templates()

# ============================================================
# 3) Volgorde zoals op slides / keygram-achtig
#    Je kunt deze lijst zelf nog aanpassen.
# ============================================================

slide_like_order <- c(
  "D#:min","B:maj","G#:min","E:maj","C#:min","A:maj",
  "F#:min","D:maj","B:min","G:maj","E:min","C:maj",
  "A:min","F:maj","D:min","A#:maj","G:min","D#:maj",
  "C:min","G#:maj","F:min","D#:min","A#:min","G#:maj"
)

# Unieke waarden houden, zodat ggplot geen dubbele levels krijgt
slide_like_order <- unique(slide_like_order)

# Zorg dat alle chords erin zitten
all_chords <- chord_templates$chord
missing_chords <- setdiff(all_chords, slide_like_order)
chord_levels <- c(slide_like_order, missing_chords)

# ============================================================
# 4) Compute smoothed chordogram
# ============================================================

compute_chordogram_smooth <- function(df, norm = "euclidean", bin_size = 1.5, smooth_k = 5) {
  
  chroma_tbl <- df |>
    compmus_wrangle_chroma() |>
    mutate(
      pitches = map(pitches, compmus_normalise, norm),
      pitches = map(pitches, ~ replace_na(as.numeric(.x), 0))
    ) |>
    mutate(frame_id = row_number())
  
  raw_scores <- chroma_tbl |>
    select(track, frame_id, start, duration, pitches) |>
    crossing(chord_templates) |>
    mutate(score = map2_dbl(pitches, template, cosine_similarity)) |>
    select(track, start, duration, chord, score)
  
  # tijd grover maken: bundelen per bin
  binned <- raw_scores |>
    mutate(time_bin = floor(start / bin_size) * bin_size) |>
    group_by(track, chord, time_bin) |>
    summarise(score = mean(score, na.rm = TRUE), .groups = "drop") |>
    arrange(chord, time_bin) |>
    group_by(track, chord) |>
    mutate(score = zoo::rollmean(score, k = smooth_k, fill = "extend")) |>
    ungroup()
  
  binned
}

# ============================================================
# 5) Plot
# ============================================================

plot_chordogram_slide_style <- function(df, norm = "euclidean", bin_size = 1.5, smooth_k = 5) {
  
  chordogram <- compute_chordogram_smooth(
    df,
    norm = norm,
    bin_size = bin_size,
    smooth_k = smooth_k
  )
  
  ggplot(
    chordogram |>
      mutate(chord = factor(chord, levels = rev(chord_levels))),
    aes(x = time_bin, y = chord, fill = score)
  ) +
    geom_tile(width = bin_size, height = 0.9) +
    theme_minimal(base_size = 13) +
    scale_fill_viridis_c() +
    labs(
      title = paste("Chordogram of '", unique(df$track), "'", sep = ""),
      x = "Time (s)",
      y = NULL,
      fill = "Similarity"
    )
}

# ============================================================
# 6) Voorbeeld: 1 song
# ============================================================

after_hours_plot <- chroma_raw |>
  filter(track == "After Hours") |>
  plot_chordogram_slide_style(bin_size = 2, smooth_k = 7)

print(after_hours_plot)

# ============================================================
# 7) Opslaan
# ============================================================

dir.create("figures/chordograms_slide_style", recursive = TRUE, showWarnings = FALSE)

save_slide_style_chordograms <- function(norm = "euclidean", bin_size = 2, smooth_k = 7) {
  chroma_raw |>
    group_split(track) |>
    walk(function(d) {
      p <- plot_chordogram_slide_style(d, norm, bin_size, smooth_k)
      fname <- paste0(
        "figures/chordograms_slide_style/",
        gsub("[^A-Za-z0-9]+", "_", unique(d$track)),
        "_slide_style.png"
      )
      ggsave(fname, p, width = 10, height = 6, dpi = 300)
    })
}

save_slide_style_chordograms()