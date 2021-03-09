library(raster)
library(rgdal)
library(rgeos)

library(spatstat) # density map

library(dplyr)    # reshaping data frame 

 

 
localfile_path = paste0("rug_results_2020-2030_SSP1.csv")


result.tmp = read.csv2(localfile_path, sep = ",")

Cell_ID_matched = match( result.tmp$CELL_ID, ctry.ids$Cell_ID)
      
result.tmp$lon = ctry.ids$Longitude[Cell_ID_matched]
result.tmp$lat = ctry.ids$Latitude[Cell_ID_matched]
result.tmp = result.tmp[!is.na(result.tmp$lat), ]

# Cell ID and cooridnates 
ctry.ids <- read.csv("Cell_ID_LatLong.csv")

# Lon-Lat projection 
proj4.LL <- CRS("+proj=longlat +datum=WGS84")


# Create a spatial pixels data frame using the lon-lat table (Cell_ID_LatLong.csv) and the input data 
rug_spdf <- SpatialPixelsDataFrame(points = SpatialPoints(cbind(result.tmp$lon, result.tmp$lat), proj4string = proj4.LL), data = data.frame(result.tmp), tolerance = 0.0011)

rs_LL <- stack(spdf.out) # raster stack

plot(rs_LL)

# Proj4js.defs["EPSG:3035"] etrs89/etrs-laea
# Scope: Single CRS for all Europe. Used for statistical mapping at all scales and other purposes where true area representation is required.
# Reference: http://spatialreference.org/ref/epsg/3035/
proj4.etrs_laea <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs";


RES_ETRS = 1E4 # 10 km 
 
rs_ETRS = projectRaster(rs_LL, crs = proj4.etrs_laea, method = "ngb", res = RES_ETRS)

plot(rs_ETRS)


