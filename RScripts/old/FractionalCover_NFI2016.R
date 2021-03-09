library(rgdal)
library(raster)
library(rgeos)
library(doSNOW)

library(fasterize)
# library(stars)



proj4.LL = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4.LL360 = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 +lon_wrap=180"
proj4.BNG = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.1502,0.247,0.8421,-20.4894 +units=m +no_defs"


setwd("~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY UK")

UK_LL = stack("Basegrid/ukcp18_tmax_wgs84.tif")
UK_BNG = stack("Basegrid/ukcp18_tmax.tif")

UK_LL_csv = read.csv("Basegrid/ukcp18-grid.csv")
# proj4string(UK_rs) ="+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"
# plot(UK_rs[[1]])
########## NFI 2016


stop("ends here")

# 
# 
# 
# # too slow 
# # NFI2016_bl_r = rasterize(x = NFI2016_shp_bl[,], y= UK_BNG, field = "OBJECTID", getCover=F)
# library(stars)
# UK_BNG_sf = read_stars("Basegrid/ukcp18_tmax.tif")
# 
# NFI2016_sf_r = st_rasterize(st_as_sf(NFI2016_shp_bl), template = st_as_stars(st_bbox(UK_BNG_sf), values = NA_real_), driver = "GTiff")
# plot(NFI2016_sf_r)