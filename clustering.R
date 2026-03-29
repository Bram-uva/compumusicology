# 1. Data inladen
trilogy1 <- read.csv("Trilogy1.csv")
trilogy2 <- read.csv("Trilogy2.csv")

# 2. Samenvoegen
trilogy <- rbind(trilogy1, trilogy2)

# 3. Alleen de numerieke variabelen selecteren
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

# 5. Data schalen
muziek_scaled <- scale(muziek_num)

# 6. Kortere kolomnamen
colnames(muziek_scaled) <- c(
  "Instrum", "Duration", "Acoustic", "Speech",
  "Tempo", "Dance", "Valence", "Loudness",
  "Energy", "Liveness"
)

# 7. Era-label per track maken
era_vector <- ifelse(trilogy$Album.Name == "Trilogy", "Early", "Late")
names(era_vector) <- trilogy$Track.Name

# 8. Vaste songs
verplicht_early <- c("Wicked Games")
verplicht_late  <- c("After Hours", "Blinding Lights")

# Alleen behouden als ze echt bestaan
verplicht_early <- verplicht_early[verplicht_early %in% rownames(muziek_scaled)]
verplicht_late  <- verplicht_late[verplicht_late %in% rownames(muziek_scaled)]

# 9. Tracks per era ophalen
early_tracks <- rownames(muziek_scaled)[era_vector[rownames(muziek_scaled)] == "Early"]
late_tracks  <- rownames(muziek_scaled)[era_vector[rownames(muziek_scaled)] == "Late"]

# 10. Verplichte tracks eruit halen voor random sampling
early_overig <- setdiff(early_tracks, verplicht_early)
late_overig  <- setdiff(late_tracks, verplicht_late)

# 11. Gebalanceerde subset maken: 10 Early en 10 Late
set.seed(123)

random_early <- sample(early_overig, 10 - length(verplicht_early))
random_late  <- sample(late_overig, 10 - length(verplicht_late))

gekozen_songs <- c(verplicht_early, random_early,
                   verplicht_late,  random_late)

subset_data <- muziek_scaled[gekozen_songs, ]

# 12. Annotatie maken voor de subset
annotation_subset <- data.frame(
  Era = era_vector[rownames(subset_data)]
)
rownames(annotation_subset) <- rownames(subset_data)

# 13. Package laden
library(pheatmap)

# 14. Kleurenschaal
kleuren <- colorRampPalette(c("#FFD400", "#5C0011", "#F5F2F4"))(100)
breaks <- seq(-3, 3, length.out = 101)

# 15. Heatmap maken
pheatmap(subset_data,
         color = kleuren,
         breaks = breaks,
         annotation_row = annotation_subset,
         clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean",
         clustering_method = "complete",
         border_color = NA,
         fontsize_row = 6,
         fontsize_col = 10,
         angle_col = 45)