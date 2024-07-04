.libPaths('~/apps/RLib')
library(rgbif)
library(terra)
library(tidyverse)
library(writexl)

setwd('~/experiments/ca_guiping_huangwy88')

modern_clim <- rast(list.files('~/public_data/climate/worldclim/0ka_5min', pattern = 'asc$', full.names = T))

data_tidy <- function(occ) {
  occ <- occ %>%
    filter(!is.na(decimalLongitude) & !is.na(decimalLatitude)) %>%
    filter(!(basisOfRecord %in% c("PRESERVED_SPECIMEN", "FOSSIL_SPECIMEN"))) %>%
    select(decimalLongitude, decimalLatitude)
  # Assign resolution to 5 min
  points <- vect(occ, geom = c('decimalLongitude', 'decimalLatitude'))
  points_data <- terra::extract(modern_clim[[1]], points, xy = T)[, c('x', 'y')]
  colnames(points_data)[colnames(points_data) == "x"] <- 'Longitude'
  colnames(points_data)[colnames(points_data) == "y"] <- 'Latitude'
  # Remove duplicated records
  occ <- points_data[!duplicated(points_data[c("Longitude", "Latitude")]), ] %>%
    vect(geom = c('Longitude', 'Latitude'))
  rm(points, points_data)
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

cibotium_clim <- occ_download_get('0148023-240321170329656') %>%
  occ_download_import() %>%
  data_tidy() %>%
  clim_extract()
pinus_clim <- occ_download_get('0093653-230530130749713') %>%
  occ_download_import() %>%
  data_tidy() %>%
  clim_extract()
dacrycarpus_clim <- occ_download_get('0079646-240229165702484') %>%
  occ_download_import() %>%
  data_tidy() %>%
  clim_extract()
liquidambar_clim <- occ_download_get('0148045-240321170329656') %>%
  occ_download_import() %>%
  data_tidy() %>%
  clim_extract()
elaeocarpus_clim <- occ_download_get('0199582-240321170329656') %>%
  occ_download_import() %>%
  data_tidy() %>%
  clim_extract()
engelhardia_clim <- occ_download_get('0148034-240321170329656') %>%
  occ_download_import() %>%
  data_tidy() %>%
  clim_extract()
lithocarpus_clim <- occ_download_get('0148048-240321170329656') %>%
  occ_download_import() %>%
  data_tidy() %>%
  clim_extract()
decaspermum_clim <- occ_download_get('0148032-240321170329656') %>%
  occ_download_import() %>%
  data_tidy() %>%
  clim_extract()
camellia_clim <- occ_download_get('0013907-231120084113126') %>%
  occ_download_import() %>%
  data_tidy() %>%
  clim_extract()
viburnum_clim <- occ_download_get('0148052-240321170329656') %>%
  occ_download_import() %>%
  data_tidy() %>%
  clim_extract()

clim_data_list <- list(cibotium_clim, pinus_clim, dacrycarpus_clim,
                       liquidambar_clim, elaeocarpus_clim, engelhardia_clim,
                       lithocarpus_clim, decaspermum_clim, camellia_clim,
                       viburnum_clim)
genera <- c('Cibotium', 'Pinus', 'Dacrycarpus', 'Liquidambar',
            'Elaeocarpus', 'Engelhardia', 'Lithocarpus',
            'Decaspermum', 'Camellia', 'Viburnum')

assemble_data <- function(climvar) {
  assembly <- data.frame(matrix(NA, nrow = 1, ncol = 5))
  colnames(assembly) <- c("Variable", "Max", "Min", "Mean", "SD")
  assembly <- assembly[-1, ]
  for (p in 1:length(genera)) {
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
