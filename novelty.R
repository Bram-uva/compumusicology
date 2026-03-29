library(tidyverse)

novelty <- read_csv("C:/Users/brsch/OneDrive/Documenten/R Projecten/compumusicology/Novelty_Wicked_Games.csv")

ggplot(novelty, aes(x = TIME, y = VALUE)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Novelty Curve Wicked Games",
    x = "Tijd (s)",
    y = "Novelty"
  )