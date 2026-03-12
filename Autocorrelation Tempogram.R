library(tidyverse)

tempogram <- read_csv("C:/Users/brsch/OneDrive/Documenten/R Projecten/compumusicology/Autocorrelation_Tempogram_After_Hours.csv")

tempogram |>
  pivot_longer(-TIME, names_to = "tempo", values_to = "value") |>
  mutate(tempo = as.numeric(tempo)) |>
  ggplot(aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_y_continuous(
    transform = c("reciprocal", "reverse"),
    breaks = seq(50, 350, 100)
  ) +
  scale_fill_viridis_c(guide = "none") +
  labs(
    title = "Tempogram After Hours",
    x = "Tijd (s)",
    y = "Tempo (BPM)"
  ) +
  theme_classic()