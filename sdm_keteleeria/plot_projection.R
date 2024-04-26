library(terra)

setwd("~/experiments/keteleeria_huangwy88")

projection <- rast(list.files('./Projection', pattern = 'tss', full.names = T, recursive = T))
proj_dirs <- list.dirs('./Projection')[2:22]

for (i in 1:nlyr(projection)) {
  svg(paste0(proj_dirs[i], '.svg'), 8, 4.5)
  plot(projection[[i]])
  dev.off()
}

pdf('./Projection/projection.pdf', 8, 4.5)
for (i in 1:nlyr(projection)) {
  plot(projection[[i]])
}
dev.off()
