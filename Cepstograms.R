library(tidyverse)
library(compmus)

graphics.off()

after_hours <- read_csv("Ceptogram_After_Hours.csv")

norms <- c("none", "manhattan", "euclidean", "chebyshev")

for (norm_method in norms) {
  
  data <- after_hours |>
    compmus_wrangle_timbre()
  
  if (norm_method != "none") {
    data <- data |>
      mutate(timbre = map(timbre, ~ compmus_normalise(.x, norm_method)))
  }
  
  plot_data <- data |>
    compmus_gather_timbre()
  
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
      title = paste("Cepstrogram -", norm_method),
      x = "Time (s)",
      y = NULL,
      fill = "Magnitude"
    ) +
    scale_fill_viridis_c() +
    theme_classic()
  
  ggsave(
    paste0("cepstrogram_", norm_method, ".png"),
    plot = p,
    width = 12,
    height = 7,
    dpi = 300
  )
}