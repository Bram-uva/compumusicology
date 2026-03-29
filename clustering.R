# 1. Data inladen
trilogy1 <- read.csv("Trilogy1.csv")
trilogy2 <- read.csv("Trilogy2.csv")

# 2. Samenvoegen
trilogy <- rbind(trilogy1, trilogy2)

# 3. Variabelen selecteren
muziek_num <- trilogy[, c(
  "Instrumentalness",
  "Duration..ms.",
  "Acousticness",
  "Speechiness",
  "Tempo",
  "Danceability",
  "Valence",
  "Loudness",
  "Energy",
  "Liveness"
)]

# 4. Songtitels als rijnamen
rownames(muziek_num) <- trilogy$Track.Name

# 5. Schalen
muziek_scaled <- scale(muziek_num)

# 6. Kortere kolomnamen
colnames(muziek_scaled) <- c(
  "Instrum", "Duration", "Acoustic", "Speech",
  "Tempo", "Dance", "Valence", "Loudness",
  "Energy", "Liveness"
)

# 7. Songs die er sowieso in moeten
verplicht <- c("After Hours", "Wicked Games", "Blinding Lights")

# Check of die songs bestaan in de data
verplicht_bestaand <- verplicht[verplicht %in% rownames(muziek_scaled)]

# Overige songs
overig <- setdiff(rownames(muziek_scaled), verplicht_bestaand)

# 8. Random subset maken:
# totaal 20 songs, waarvan 3 verplicht en 17 random
set.seed(123)
random_songs <- sample(overig, 23)

gekozen_songs <- c(verplicht_bestaand, random_songs)

subset_data <- muziek_scaled[gekozen_songs, ]

# 9. Package laden
library(pheatmap)

# 10. Jouw kleurenschaal
kleuren <- colorRampPalette(c("navy", "white", "firebrick3"))(100)

# 11. Heatmap maken
pheatmap(subset_data,
         clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean",
         clustering_method = "ward.D2",
         border_color = NA,
         fontsize_row = 6,
         fontsize_col = 10,
         angle_col = 45,
         color = kleuren)