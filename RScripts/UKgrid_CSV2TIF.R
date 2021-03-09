setwd("~/OneDrive - Karlsruher Institut für Technologie (KIT)/UK_SPEED/")

library(raster)
library(sp)
dt = read.csv("ukcp18-grid.csv")

# Lon-Lat projection 
proj4.LL <- CRS("+proj=longlat +datum=WGS84")

# Proj4js.defs["EPSG:3035"] etrs89/etrs-laea
# Scope: Single CRS for all Europe. Used for statistical mapping at all scales and other purposes where true area representation is required.
# Reference: http://spatialreference.org/ref/epsg/3035/
# proj4.etrs_laea <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs";



coords = cbind(dt$xWGS, dt$yWGS)
sp = SpatialPoints(coords, proj4string = proj4.LL)

plot(sp)
