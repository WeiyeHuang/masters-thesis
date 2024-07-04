.libPaths('~/apps/RLib')
library(rgbif)
library(terra)
library(tidyverse)
library(writexl)

setwd('~/experiments/ca_tengxian_huangwy88')

modern_clim <- rast(list.files('~/public_data/climate/worldclim/0ka_5min', pattern = 'asc$', full.names = T))

data_tidy <- function(occ) {
  tmp_raster <- rast('~/public_data/climate/worldclim/0ka_5min/bio01.asc')
  occ <- occ %>%
    filter(!is.na(decimalLongitude) & !is.na(decimalLatitude)) %>%
    filter(!(basisOfRecord %in% c("PRESERVED_SPECIMEN", "FOSSIL_SPECIMEN"))) %>%
    select(decimalLongitude, decimalLatitude)
  # Assign resolution to 5 min
  points <- vect(occ, geom = c('decimalLongitude', 'decimalLatitude'))
  points_data <- terra::extract(tmp_raster, points, xy = T)[, c('x', 'y')]
  colnames(points_data)[colnames(points_data) == "x"] <- 'Longitude'
  colnames(points_data)[colnames(points_data) == "y"] <- 'Latitude'
  # Remove duplicated records
  occ <- points_data[!duplicated(points_data[c("Longitude", "Latitude")]), ] %>%
    vect(geom = c('Longitude', 'Latitude'))
  rm(points, points_data, tmp_raster)
  return(occ)
}

clim_extract <- function(occ) {
  clim_data <- terra::extract(modern_clim, occ)
  sub_tables <- lapply(clim_data[, -1], function(x) {
    split_points <- quantile(x, probs = c(0.1, 0.9), na.rm = TRUE)
    sub_table <- clim_data[which(x > split_points[1] & x < split_points[2]), ]
    return(sub_table)
  })
  summary_df <- data.frame(matrix(NA, nrow = 1, ncol = 5))
  colnames(summary_df) <- c("Variable", "Max", "Min", "Mean", "SD")
  for (i in 1:19) {
    sorted_sub_table <- sub_tables[[i]] %>% arrange(desc(ID)) # 按ID降序排序
    sub_data <- sorted_sub_table %>% slice(-(1:round(nrow(sorted_sub_table)*0.1)), -(nrow(sorted_sub_table)-round(nrow(sorted_sub_table)*0.1):nrow(sorted_sub_table))) # 删除头尾10%
    summary_df[i, 1] <- names(clim_data)[i+1] # 记录变量名称
    summary_df[i, 2:5] <- c(max(sub_data[[i+1]]), min(sub_data[[i+1]]), mean(sub_data[[i+1]]), sd(sub_data[[i+1]]))
  }
  print(summary_df)
  return(summary_df)
}

keteleeria_clim <- occ_download_get('0011684-230918134249559') %>%
  occ_download_import() %>%
  data_tidy() %>%
  clim_extract()
pinus_clim <- occ_download_get('0093653-230530130749713') %>%
  occ_download_import() %>%
  data_tidy() %>%
  clim_extract()
stewartia_clim <- occ_download_get('0093662-230530130749713') %>%
  occ_download_import() %>%
  data_tidy() %>%
  clim_extract()
syzygium_clim <- occ_download_get('0093666-230530130749713') %>%
  occ_download_import() %>%
  data_tidy() %>%
  clim_extract()
carrierea_clim <- occ_download_get('0067596-240321170329656') %>%
  occ_download_import() %>%
  data_tidy() %>%
  clim_extract()
cryptomeria_clim <- occ_download_get('0067560-240321170329656') %>%
  occ_download_import() %>%
  data_tidy() %>%
  clim_extract()

clim_data_list <- list(keteleeria_clim, pinus_clim, stewartia_clim,
                       syzygium_clim, carrierea_clim, cryptomeria_clim)
genera <- c('Keteleeria', 'Pinus', 'Stewartia', 'Syzygium', 'Carrierea', 'Cryptomeria')

assemble_data <- function(climvar) {
  assembly <- data.frame(matrix(NA, nrow = 1, ncol = 5))
  colnames(assembly) <- c("Variable", "Max", "Min", "Mean", "SD")
  assembly <- assembly[-1, ]
  for (p in 1:6) {
    biodata <- clim_data_list[p] %>%
      as.data.frame() %>%
      filter(Variable == climvar)
    assembly <- rbind(assembly, biodata)
  }
  assembly <- assembly %>%
    select(Max, Min) %>%
    mutate(Genera = genera) %>%
    select(Genera, everything())
  return(assembly)
}

bio_data_list <- list()

for (x in 1:19) {
  temp <- assemble_data(paste0('bio', x))
  bio_data_list[[x]] <- temp
  names(bio_data_list)[x] <- paste0('bio', x)
  rm(temp)
}

write_xlsx(bio_data_list, 'ca_data.xlsx')

save.image('ca_data.RData')
