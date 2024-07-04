.libPaths('~/apps/RLib')
library(raster)
library(rasterVis)
library(viridisLite)

setwd('~/experiments/sdm_keteleeria_huangwy88/wc')

proj_result <- stack(list.files('Projection', pattern = 'tss.tif$', recursive = T, full.names = T)[1:20])

sysutheme <- rasterTheme(region = c('#f2f2f2', '#c2482e', '#ffeb86', '#00561f'))
pdf('proj_lvlplot.pdf', 12, 9)
levelplot(proj_result, par.settings = sysutheme, contour = F)
dev.off()