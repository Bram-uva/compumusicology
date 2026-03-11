# ============================================================
# Week 8 — Chroma Features (Sonic Visualiser CSV -> R plots)
# ============================================================

# 0) Packages (1x installeren als nodig)
# install.packages("tidyverse")
# install.packages("remotes")
# remotes::install_github("jaburgoyne/compmus")

# 1) Laden
library(tidyverse)
library(compmus)

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

# 3) Alles inlezen in één tabel met 'track' label
chroma_raw <- imap_dfr(
  files,
  ~ read_csv(.x, show_col_types = FALSE) |> mutate(track = .y)
)

# ------------------------------------------------------------
# 4) Chromagram plot-functie (zoals in de les)
#    - wrangle -> normalise -> gather -> geom_tile (met timing fix)
# ------------------------------------------------------------
plot_chromagram <- function(df, norm = "euclidean") {
  df |>
    compmus_wrangle_chroma() |>
    mutate(pitches = map(pitches, compmus_normalise, norm)) |>
    compmus_gather_chroma() |>
    ggplot(aes(
      x = start + duration / 2,  # belangrijk: tile-centre fix
      width = duration,
      y = pitch_class,
      fill = value
    )) +
    geom_tile() +
    theme_minimal() +
    scale_fill_viridis_c() +
    labs(
      title = paste("Chromagram —", unique(df$track)),
      subtitle = paste("Normalisation:", norm),
      x = "Time (s)",
      y = NULL,
      fill = "Magnitude"
    )
}

# 5) Chromagrams tonen (run deze blokken om te checken)
# -- Euclidean
chroma_raw |>
  group_split(track) |>
  walk(\(d) print(plot_chromagram(d, "euclidean")))

# -- Manhattan
chroma_raw |>
  group_split(track) |>
  walk(\(d) print(plot_chromagram(d, "manhattan")))

# -- Chebyshev (ook uit de les; handig ter vergelijking)
chroma_raw |>
  group_split(track) |>
  walk(\(d) print(plot_chromagram(d, "chebyshev")))

# ------------------------------------------------------------
# 6) Opslaan als PNG voor inleveren (aanrader)
# ------------------------------------------------------------
dir.create("figures", showWarnings = FALSE)

safe_name <- function(x) str_replace_all(x, "[^A-Za-z0-9]+", "_")

save_all_chromagrams <- function(norm = "euclidean") {
  chroma_raw |>
    group_split(track) |>
    walk(function(d) {
      p <- plot_chromagram(d, norm)
      fname <- paste0("figures/chroma_", norm, "_", safe_name(unique(d$track)), ".png")
      ggsave(fname, p, width = 10, height = 4, dpi = 300)
    })
}

save_all_chromagrams("euclidean")
save_all_chromagrams("manhattan")
save_all_chromagrams("chebyshev")

# ------------------------------------------------------------
# 7) (OPTIONEEL, maar sterk) Distance heatmap tussen 2 songs
#    - Dit is de compmus_long_distance demo uit de les
#    - Je levert dan 1 extra visualisatie in (goed voor punten)
# ------------------------------------------------------------
track_a <- "Darkside — iann dior"
track_b <- "Sunflower — Post Malone"

a <- chroma_raw |> filter(track == track_a)
b <- chroma_raw |> filter(track == track_b)

dist_plot <- compmus_long_distance(
  a |>
    compmus_wrangle_chroma() |>
    mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
    filter(row_number() %% 50L == 0L),   # downsample zoals in de les
  b |>
    compmus_wrangle_chroma() |>
    mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
    filter(row_number() %% 50L == 0L),
  feature = pitches,
  method = "cosine"  # snelle, vaak gebruikte keuze
) |>
  filter(!is.nan(d)) |>
  ggplot(aes(
    x = xstart + xduration / 2,
    width = 50 * xduration,
    y = ystart + yduration / 2,
    height = 50 * yduration,
    fill = d
  )) +
  geom_tile() +
  coord_equal() +
  theme_minimal() +
  scale_fill_viridis_c(guide = NULL) +
  labs(
    title = "Chroma distance (cosine)",
    subtitle = "Darker diagonal = similar chroma content; bends = tempo/structure differences",
    x = track_a,
    y = track_b
  )

print(dist_plot)
ggsave("figures/chroma_distance_darkside_vs_sunflower.png", dist_plot, width = 6, height = 6, dpi = 300)