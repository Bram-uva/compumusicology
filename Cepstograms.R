library(tidyverse)
library(compmus)

graphics.off()

after_hours <- read_csv("Ceptogram_After_Hours.csv")

# 1) Wrangle maar één keer
base_data <- after_hours |>
  compmus_wrangle_timbre()

# 2) Veilige normalisatiefunctie
safe_normalise <- function(x, method) {
  if (all(is.na(x))) return(x)
  
  if (method == "none") return(x)
  
  # norm berekenen
  norm_value <- switch(
    method,
    manhattan = sum(abs(x), na.rm = TRUE),
    euclidean = sqrt(sum(x^2, na.rm = TRUE)),
    chebyshev = max(abs(x), na.rm = TRUE),
    stop("Unknown method: ", method)
  )
  
  # voorkom delen door 0 of rare waarden
  if (is.na(norm_value) || norm_value == 0) {
    return(rep(NA_real_, length(x)))
  }
  
  x / norm_value
}

norms <- c("none", "manhattan", "euclidean", "chebyshev")

# Optioneel: vaste kleurschaal voor betere vergelijking
all_plot_data <- map_dfr(norms, function(norm_method) {
  base_data |>
    mutate(timbre = map(timbre, ~ safe_normalise(.x, norm_method))) |>
    compmus_gather_timbre() |>
    mutate(norm = norm_method)
})

fill_limits <- range(all_plot_data$value, na.rm = TRUE)

for (norm_method in norms) {
  
  plot_data <- base_data |>
    mutate(timbre = map(timbre, ~ safe_normalise(.x, norm_method))) |>
    compmus_gather_timbre()
  
  message("Plotting: ", norm_method)
  message("  NAs in value: ", sum(is.na(plot_data$value)))
  message("  Range: ", paste(range(plot_data$value, na.rm = TRUE), collapse = " to "))
  
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
      y = "MFCC",
      fill = "Magnitude"
    ) +
    scale_fill_viridis_c(limits = fill_limits, na.value = "grey90") +
    theme_classic()
  
  ggsave(
    filename = paste0("cepstrogram_", norm_method, ".png"),
    plot = p,
    width = 12,
    height = 7,
    dpi = 300
  )
}