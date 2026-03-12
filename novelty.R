library(tidyverse)

novelty <- read_csv("C:/Users/brsch/OneDrive/Documenten/R Projecten/compumusicology/Novelty_After_Hours.csv")

ggplot(novelty, aes(x = TIME, y = VALUE)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Novelty Curve After Hours",
    x = "Tijd (s)",
    y = "Novelty"
  )