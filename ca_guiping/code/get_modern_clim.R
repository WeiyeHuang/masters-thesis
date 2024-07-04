.libPaths('~/apps/RLib')
library(terra)

modern_clim <- rast(list.files('~/public_data/climate/worldclim/0ka_30s', pattern = 'asc$', full.names = T))

fossil_site <- vect(data.frame(x = 110.1653, y = 23.3860), geom = c('x', 'y'))

print(extract(modern_clim, fossil_site, method = 'bilinear'))
