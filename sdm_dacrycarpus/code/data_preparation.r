.libPaths('/public4/group_wxk/home/wuxk/apps/RLib')
library(rgbif)
library(terra)
library(dplyr)
library(biomod2)

# Set working directory
setwd("~/experiments/sdm_dacrycarpus_huangwy88")

dir.create("./Training")
dir.create("./Evaluation")
dir.create("./Models")
dir.create("./Projection")
dir.create("./RData")
dir.create("./logs")

# Read the occurrence data
occurrences <- occ_download_get("0079646-240229165702484") %>%
  occ_download_import()

# Remove records with missing coordinates
filtered_occurrences <- occurrences[!is.na(occurrences$decimalLongitude) & !is.na(occurrences$decimalLatitude), ]

# Remove specimen records
filtered_occurrences <- filtered_occurrences[!(filtered_occurrences$basisOfRecord %in% c("PRESERVED_SPECIMEN", "FOSSIL_SPECIMEN")), ]

# Keep the required columns
filtered_occurrences <- filtered_occurrences[, c("decimalLongitude", "decimalLatitude")]

# Load a temporary raster to assist
modern_clim <- rast(list.files('~/public_data/climate/worldclim/0ka_5min', pattern = 'asc$', full.names = T))

# Assign resolution to 5 min
points <- vect(filtered_occurrences, geom = c('decimalLongitude', 'decimalLatitude'))
points_data <- extract(modern_clim[[1]], points, xy = T)[, c('x', 'y')]
colnames(points_data)[colnames(points_data) == "x"] <- 'Longitude'
colnames(points_data)[colnames(points_data) == "y"] <- 'Latitude'

# Remove duplicated records
filtered_occurrences <- points_data[!duplicated(points_data[c("Longitude", "Latitude")]), ]

# Add a column named "presence" and fill it with "1" to indicate the presence
filtered_occurrences$presence <- 1

# Print the number of records after filtering
cat("Number of filtered records:", nrow(filtered_occurrences), "\n")

# Save the processed data table
write.csv(filtered_occurrences, file = "./Training/dacrycarpus_filtered.csv", row.names = FALSE)

filtered_occurrences <- vect(filtered_occurrences, geom = c('Longitude', 'Latitude'))

# Generate pseudo-absence data
pseudoabsence_points <- bm_PseudoAbsences(resp.var = filtered_occurrences,
                                          expl.var = modern_clim,
                                          nb.absences = 5000,
                                          nb.rep = 1,
                                          strategy = "sre",
                                          sre.quant = 0.15)
pseudoabsence_data <- as.data.frame(pseudoabsence_points)
rm(pseudoabsence_points)
pseudoabsence_data <- pseudoabsence_data[(nrow(filtered_occurrences) + 1):nrow(pseudoabsence_data),c("xy.x", "xy.y", "sp")]
cat("Number of generated pseudo-absence points: ", nrow(pseudoabsence_data), "\n")

pseudoabsence_points2 <- bm_PseudoAbsences(resp.var = filtered_occurrences,
                                           expl.var = modern_clim,
                                           nb.absences = 5000,
                                           nb.rep = 1,
                                           strategy = "disk",
                                           dist.min = 150000)
pseudoabsence_data2 <- as.data.frame(pseudoabsence_points2)
rm(pseudoabsence_points2)
pseudoabsence_data2 <- pseudoabsence_data2[(nrow(filtered_occurrences) + 1):nrow(pseudoabsence_data2),c("xy.x", "xy.y", "sp")]
cat("Number of generated pseudo-absence points: ", nrow(pseudoabsence_data2), "\n")

pseudoabsence_data <- rbind(pseudoabsence_data, pseudoabsence_data2)

# Rename the column names to "Longitude", "Latitude" and "presence" for better understanding
colnames(pseudoabsence_data)[colnames(pseudoabsence_data) == "xy.x"] <- "Longitude"
colnames(pseudoabsence_data)[colnames(pseudoabsence_data) == "xy.y"] <- "Latitude"
colnames(pseudoabsence_data)[colnames(pseudoabsence_data) == "sp"] <- "presence"

# Fill the column "presence" with "0" to indicate the absence
pseudoabsence_data$presence <- 0

# Save the processed data table
write.csv(pseudoabsence_data, file = "./Training/dacrycarpus_absence.csv", row.names = FALSE)

suit_clim <- extract(modern_clim, filtered_occurrences)

# 计算Pearson相关性矩阵并作图
corr_matrix <- cor(suit_clim[2:20], method = 'pearson', use = "pairwise.complete.obs")
corrplot(corr_matrix)

# 保存相关性矩阵
write.csv(corr_matrix, file = "./Training/corr_matrix.csv", row.names = FALSE)