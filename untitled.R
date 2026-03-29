library(tidyverse)

# =========================
# 1. Data inladen
# =========================
trilogy1 <- read_csv("Trilogy1.csv")
trilogy2 <- read_csv("Trilogy2.csv")

# =========================
# 2. Alleen numerieke kolommen
# =========================
num1 <- trilogy1 %>% select(where(is.numeric))
num2 <- trilogy2 %>% select(where(is.numeric))

# missende waarden vervangen
num1[is.na(num1)] <- 0
num2[is.na(num2)] <- 0

# labels
labels1 <- trilogy1$`Track Name`
labels2 <- trilogy2$`Track Name`

# =========================
# 3. Trilogy 1 dendrogram
# =========================
mat1 <- scale(num1)
dist1 <- dist(mat1)
hc1 <- hclust(dist1, method = "ward.D2")

png("trilogy1_dendrogram.png", width = 1600, height = 900, res = 150)
plot(
  hc1,
  labels = labels1,
  main = "Dendrogram - Trilogy 1",
  xlab = "",
  sub = "",
  ylab = "Height",
  cex = 0.8
)
dev.off()

# =========================
# 4. Trilogy 2 dendrogram
# =========================
mat2 <- scale(num2)
dist2 <- dist(mat2)
hc2 <- hclust(dist2, method = "ward.D2")

png("trilogy2_dendrogram.png", width = 1800, height = 1000, res = 150)
plot(
  hc2,
  labels = labels2,
  main = "Dendrogram - Trilogy 2",
  xlab = "",
  sub = "",
  ylab = "Height",
  cex = 0.8
)
dev.off()

# =========================
# 5. Combined dendrogram
# =========================
trilogy1$Source <- "Trilogy1"
trilogy2$Source <- "Trilogy2"

combined <- bind_rows(trilogy1, trilogy2)
combined$Label <- paste(combined$`Track Name`, combined$Source, sep = " | ")

num_combined <- combined %>% select(where(is.numeric))
num_combined[is.na(num_combined)] <- 0

mat_combined <- scale(num_combined)
dist_combined <- dist(mat_combined)
hc_combined <- hclust(dist_combined, method = "ward.D2")

png("combined_dendrogram.png", width = 2600, height = 1200, res = 180)
plot(
  hc_combined,
  labels = combined$Label,
  main = "Dendrogram - Trilogy 1 + Trilogy 2",
  xlab = "",
  sub = "",
  ylab = "Height",
  cex = 0.6
)
dev.off()

pdf("combined_dendrogram.pdf", width = 20, height = 10)
plot(
  hc_combined,
  labels = combined$Label,
  main = "Dendrogram - Trilogy 1 + Trilogy 2",
  xlab = "",
  sub = "",
  ylab = "Height",
  cex = 0.6
)
dev.off()

# =========================
# 6. Subsecties van combined
# =========================
clusters <- cutree(hc_combined, k = 2)

sub1 <- combined[clusters == 1, ]
sub2 <- combined[clusters == 2, ]

# ---- Subsection 1 ----
if (nrow(sub1) >= 2) {
  sub1_num <- sub1 %>% select(where(is.numeric))
  sub1_num[is.na(sub1_num)] <- 0
  
  mat_sub1 <- scale(sub1_num)
  dist_sub1 <- dist(mat_sub1)
  hc_sub1 <- hclust(dist_sub1, method = "ward.D2")
  
  png("combined_subsection_1.png", width = 1800, height = 900, res = 150)
  plot(
    hc_sub1,
    labels = sub1$Label,
    main = "Combined Dendrogram - Subsection 1",
    xlab = "",
    sub = "",
    ylab = "Height",
    cex = 0.8
  )
  dev.off()
} else {
  print("Subsection 1 has fewer than 2 tracks, so no dendrogram was made.")
}

# ---- Subsection 2 ----
if (nrow(sub2) >= 2) {
  sub2_num <- sub2 %>% select(where(is.numeric))
  sub2_num[is.na(sub2_num)] <- 0
  
  mat_sub2 <- scale(sub2_num)
  dist_sub2 <- dist(mat_sub2)
  hc_sub2 <- hclust(dist_sub2, method = "ward.D2")
  
  png("combined_subsection_2.png", width = 1800, height = 900, res = 150)
  plot(
    hc_sub2,
    labels = sub2$Label,
    main = "Combined Dendrogram - Subsection 2",
    xlab = "",
    sub = "",
    ylab = "Height",
    cex = 0.8
  )
  dev.off()
} else {
  print("Subsection 2 has fewer than 2 tracks, so no dendrogram was made.")
}