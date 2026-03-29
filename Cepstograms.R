library(tidyverse)
library(compmus)

graphics.off()

# Map met je CSV-bestanden
input_folder <- "."
output_folder <- "euclidean_ceptograms"

# Maak outputmap aan als die nog niet bestaat
dir.create(output_folder, showWarnings = FALSE)

# Zoek alle CSV-bestanden waarvan de naam met Ceptogram_ begint
files <- list.files(
  path = input_folder,
  pattern = "^Ceptogram_.*\\.csv$",
  full.names = TRUE
)

for (file in files) {
  
  # Bestand inlezen
  data_raw <- read_csv(file, show_col_types = FALSE)
  
  # Timbre wranglen + euclidean normaliseren
  plot_data <- data_raw |>
    compmus_wrangle_timbre() |>
    mutate(timbre = map(timbre, compmus_normalise, "euclidean")) |>
    compmus_gather_timbre()
  
  # Bestandsnaam zonder extensie
  file_name <- tools::file_path_sans_ext(basename(file))
  
  # Plot maken
  p <- ggplot(
    plot_data,
    aes(
      x = start + duration / 2,
      width = duration,
      y = mfcc,
      fill = value
    )
  ) +
    geom_tile() +
    labs(
      title = paste("Cepstrogram - Euclidean -", file_name),
      x = "Time (s)",
      y = "MFCC",
      fill = "Magnitude"
    ) +
    scale_fill_viridis_c() +
    theme_classic()
  
  # Opslaan
  ggsave(
    filename = file.path(output_folder, paste0(file_name, "_euclidean.png")),
    plot = p,
    width = 12,
    height = 7,
    dpi = 150
  )
  
  message("Saved: ", file_name)
}