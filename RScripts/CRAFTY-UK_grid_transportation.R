library(rgdal)
library(raster)
library(rgeos)
library(doSNOW)


#  Road density is calcuated based on the Global Roads Inventory Project (GRIP) dataset (https://www.globio.info/download-grip-dataset). It provides consistent global roads dataset for use in global environmental and biodiversity assessment models. The dataset includes the five road types: Type 1 (highways), Type 2 (primary roads), Type 3 (secondary roads), Type 4 (tertiary roads), Type 5 (local roads). In addition to the vector GIS data, future global road infrastructure scenario projections following the Shared Socioeconomic Pathways (SSPs) are described in their paper (Meijer et al., 2018). Using regression modelling, they estimated 18% (SSP1), 19% (SSP2), 15% (SSP3), 14% (SSP4), and 23% (SSP5) increase in road length globally by 2050. The regression model is built at the nations level.
# 
# Future road density ~ Area + population density + GDP + OECD membership + e
# 
# 
# 
# # Vector road data to road density (1 km)
# 
# The road polylines are categorised by road type (1 to 5). On the reference UK grid (1 km), the density of the five road types were calculated using the Arcmap tool box 'line density'. For each 1 km2 cell, total road length (km) in a 1 km radius circle is calculated. The calculation was done for the five road types individually.
# 
# Road density to a capital map
# Weighted averaging of the five density maps is required. A suggestion was to do weighted averaging of them by speed limit ( https://www.gov.uk/speed-limits).
# 
# Built-up areas 30 mph.    (Road Type 4 and 5)
# Single carriageway 60 mph (Road Type 3 secondary roads)
# Dual carriageways 70 mph  (Road Type 2 primary rodas)
# Motorways 70 mph          (Road Type 1 highways)
# 
# 
# It depends what secondary/tertiary and location roads actually represent as they are 60 in most places except built up areas where they are usually 30.
# 
# # Reference:
# https://www.globio.info/download-grip-dataset
# Meijer, J.R., Huijbegts, M.A.J., Schotten, C.G.J. and Schipper, A.M. (2018): Global patterns of current and future road infrastructure. Environmental Research Letters, 13-064006. Data is available at www.globio.info
# https://www.gov.uk/speed-limits
# 
# 




setwd("~/Nextcloud/workspace_newEU/CRAFTY UK input CSV files/")

UK_LL = stack("Basegrid/ukcp18_tmax_wgs84.tif")
UK_BNG = stack("Basegrid/ukcp18_tmax.tif")


if (FALSE) {
  # proj4string(UK_rs) ="+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"
  # plot(UK_rs[[1]])
  # 
  
  
  
  # Clip the transportation data 
  grip4_shp = readOGR("GRIP4_Region4_vector_shp_EU/GRIP4_region4_UK.shp")
  
  
  beginCluster()
  
  GRI4_UK_shp = crop(grip4_shp, UK_LL)
  endCluster()
  writeOGR(GRI4_UK_shp,dsn = "GRIP4_Region4_vector_shp_EU", layer ="GRIP4_region4_UK_cropped", driver = "ESRI Shapefile")
  rm(grip4_shp)
  
  
  # } else { 
  #   GRI4_UK_shp = readOGR("~/Nextcloud/CRAFTY GIS data/Road and railway data//GRIP4_region4_UK_BNG.shp")
  # }
  
  # Type 1 
  GRI4_UK_Typ1 = GRI4_UK_shp[GRI4_UK_shp$GP_RTP == 1,]
  GRI4_UK_Typ2 = GRI4_UK_shp[GRI4_UK_shp$GP_RTP == 2,]
  GRI4_UK_Typ3 = GRI4_UK_shp[GRI4_UK_shp$GP_RTP == 3,]
  GRI4_UK_Typ4 = GRI4_UK_shp[GRI4_UK_shp$GP_RTP == 4,]
  GRI4_UK_Typ5 = GRI4_UK_shp[GRI4_UK_shp$GP_RTP == 5,]
  
  # writeOGR(GRI4_UK_Typ1,dsn = "GRIP4_Region4_vector_shp_EU", layer ="GRIP4_region4_UK_BNG_Typ1", driver = "ESRI Shapefile")
  # writeOGR(GRI4_UK_Typ2,dsn = "GRIP4_Region4_vector_shp_EU", layer ="GRIP4_region4_UK_BNG_Typ2", driver = "ESRI Shapefile")
  # writeOGR(GRI4_UK_Typ3,dsn = "GRIP4_Region4_vector_shp_EU", layer ="GRIP4_region4_UK_BNG_Typ3", driver = "ESRI Shapefile")
  # writeOGR(GRI4_UK_Typ4,dsn = "GRIP4_Region4_vector_shp_EU", layer ="GRIP4_region4_UK_BNG_Typ4", driver = "ESRI Shapefile")
  # writeOGR(GRI4_UK_Typ5,dsn = "GRIP4_Region4_vector_shp_EU", layer ="GRIP4_region4_UK_BNG_Typ5", driver = "ESRI Shapefile")
  
  path_grip = "~/Nextcloud/CRAFTY GIS data/Road and railway data/"
  
  GRIP4_Typ1_density = raster(paste0(path_grip, "GRIP4_Typ1_Density.tif"))
  GRIP4_Typ2_density =  raster(paste0(path_grip, "GRIP4_Typ2_Density.tif"))
  GRIP4_Typ3_density =  raster(paste0(path_grip, "GRIP4_Typ3_Density.tif"))
  GRIP4_Typ4_density =  raster(paste0(path_grip, "GRIP4_Typ4_Density.tif"))
  GRIP4_Typ5_density =  raster(paste0(path_grip, "GRIP4_Typ5_Density.tif"))
  
  
  na_mask = !is.na(UK_BNG)
  
  beginCluster()
  GRIP4_Typ1_density = projectRaster(GRIP4_Typ1_density, UK_BNG) * na_mask
  GRIP4_Typ2_density = projectRaster(GRIP4_Typ2_density, UK_BNG) * na_mask
  GRIP4_Typ3_density = projectRaster(GRIP4_Typ3_density, UK_BNG) * na_mask
  GRIP4_Typ4_density = projectRaster(GRIP4_Typ4_density, UK_BNG) * na_mask
  GRIP4_Typ5_density = projectRaster(GRIP4_Typ5_density, UK_BNG) * na_mask
  
  GRIP4_density_rs = stack(GRIP4_Typ1_density, GRIP4_Typ2_density, GRIP4_Typ3_density, GRIP4_Typ4_density, GRIP4_Typ5_density)
  names(GRIP4_density_rs) = paste0("RoadType", 1:5) 
  
  endCluster()
  
  
  writeRaster(GRIP4_density_rs, filename =  "GRIP4_Region4_vector_shp_EU/GRIP4_region4_UK_density.tif")
}

GRIP4_density_rs = stack(paste0(path_grip, "GRIP4_region4_UK_BNG_Density.tif"))


dens_max = max(getValues(GRIP4_density_rs), na.rm=T)
speed_limit = c(70, 70, 60, 30, 30)  # https://www.gov.uk/speed-limits
dens_weights = speed_limit / sum(speed_limit)

RoadCapital = sum(stack( sapply(1:5, FUN = function(x) GRIP4_density_rs[[x]] * dens_weights[x] )  ), na.rm=T) 
# 
# pdf("RoadDensity_UK.pdf", width = 12, height = 16)
# par(mfrow=c(1,1), mar=c(4,4,4,4), oma=4)
# plot(RoadCapital, main = "Road capital (weighted road density)")
# plot(GRIP4_density_rs, main = paste0("Road type", 1:5, " (km/km2)"))
# dev.off() 

 
### Read the NUTS data 
LAD_shp = readOGR("~/Nextcloud/workspace_newEU/CRAFTY UK input CSV files/Boundaries/Local_Authority_Districts__December_2019__Boundaries_UK_BFE.shp")

beginCluster(16)

# GRIP4_density_rs_BNG = projectRaster(GRIP4_density_rs, crs = proj4string(LAD_shp))
# writeRaster(GRIP4_density_rs_BNG, filename =  "GRIP4_Region4_vector_shp_EU/GRIP4_region4_UK_BNG_density.tif")


RoadCapital_BNG = projectRaster(RoadCapital, crs = proj4string(LAD_shp))

beginCluster(16)
a = extract(RoadCapital_BNG, y = LAD_shp[1:328,],   FUN = function(x) mean(x, na.rm=T))
b = extract(RoadCapital_BNG, y = LAD_shp[329,], FUN = function(x) mean(x, na.rm=T))
c = extract(RoadCapital_BNG, y = LAD_shp[330:351,],  FUN = function(x) mean(x, na.rm=T))

d = extract(RoadCapital_BNG, y = LAD_shp[352:382,],  FUN = function(x) mean(x, na.rm=T))
endCluster()

dens = c(sapply(a, mean),sapply(b, mean), sapply(c, mean, na.rm=T), sapply(d, mean, na.rm=T))
dens[is.na(dens)] = 0 
LAD_shp2 = LAD_shp
LAD_shp2$dens = dens

LAD_shp2$dens = NULL
LAD_shp2$RoadCapital = dens
plot(LAD_shp, col=scales::alpha("red", LAD_shp2$RoadCapital))
library(sf)
# 
pdf("RoadDensity_UK.pdf", width = 12, height = 16)
par(mfrow=c(1,1), mar=c(4,4,4,4))
plot(st_as_sf(LAD_shp2)[,"RoadCapital"], main="Road capital per LAD (km/km2)")
plot(RoadCapital, main = "Weighted road density)")
plot(GRIP4_density_rs, main = paste0("Road type", 1:5, " (km/km2)"))
dev.off()


writeOGR(LAD_shp2,dsn = path_grip, layer ="RoadCapital_LAD2019_USING_GRIP4_UK_BNG", driver = "ESRI Shapefile")


write.csv2(LAD_shp2@data, row.names = F, quote = F, file = paste0(path_grip, "RoadCapital_LAD2019_USING_GRIP4_UK_BNG.csv"))


# # roads <- shapefile("TZA_roads.shp")
# # roads <- spTransform(roads, CRS("+proj=utm +zone=37 +south +datum=WGS84"))
# 
# roads = GRI4_UK_Typ1 
# 
# rs <- raster(extent(roads), crs=projection(roads))
# rs[] <- 1:ncell(rs)
# 
# 
# # Intersect lines with raster "polygons" and add length to new lines segments
# # rsp <- rasterToPolygons(UK_BNG)
# rsp = rasterToPolygons(rasterFromCells(UK_BNG, 380177))
# 
# rp <- intersect(roads, rsp)
# rp$length <- gLength(rp, byid=TRUE) / 1000
# x <- tapply(rp$length, rp$layer, sum)
# r <- raster(rs)
# r[as.integer(names(x))] <- x
# 
# plot(r)
#  
# 
# endCluster()
# 
# 
# cl =  makeCluster(20, type="SOCK")
# doSNOW::registerDoSNOW(cl)
# 
# beginCluster()
# 
# 
# # r1 = rasterize(GRI4_UK_Typ1, UK_LL)
# plot(GRI4_UK_Typ1)
# 
# 
# 
# help(gIntersection)
# 
# raster::UK_LL
# 
# # UK_LL[] = 0 
# 
# 
# GRI4_UK_Typ5
# 
# cell_tmp = rasterFromCells(UK_BNG, 308000:309000)
# polygon_tmp = rasterToPolygons(cell_tmp)
# 
# plot(polygon_tmp, add=F, col="red")
# plot(GRI4_UK_Typ1, add=T)
# plot(polygon_tmp, add=T)
# 
# gIntersection(GRI4_UK_Typ1, polygon_tmp)
# 
# 
# rrst <- raster(extent(roads), crs=proj4string(roads))
# projection(roads) = projection(rrst)
# # Intersect lines with raster "polygons" and add length to new lines segments
# rrst.poly <- rasterToPolygons(rrst)
# rp <- gIntersection(roads, rrst.poly, byid=TRUE)
# rp <- SpatialLinesDataFrame(rp, data.frame(row.names=sapply(slot(rp, "lines"), 
#                                                             function(x) slot(x, "ID")), ID=1:length(rp), 
#                                            length=SpatialLinesLengths(rp)/1000) ) 
# 
# # Rasterize using sum of intersected lines                            
# rd.rst <- rasterize(rp, rrst, field="length", fun="sum")
# 
# # Plot results
# require(RColorBrewer)
# spplot(rd.rst, scales = list(draw=TRUE), xlab="x", ylab="y", 
#        col.regions=colorRampPalette(brewer.pal(9, "YlOrRd")), 
#        sp.layout=list("sp.lines", rp), 
#        par.settings=list(fontsize=list(text=15)), at=seq(0, 1800, 200))
# 
# 




# 
# ## To create raster:
# library(raster)
# library(rgeos)
# r <- raster(ncols=90, nrows=50)
# values(r) <- sample(1:10, ncell(r), replace=TRUE)
# 
# ## Road raster
# r[r[] < 10] <- 0
# r[r[] >= 10] <- 1
# plot(r)
# 
# ## To create spatial lines 
# line1 <- rbind(c(-125,0), c(0,60))
# line2 <- rbind(c(0,60), c(40,5))
# line3 <- rbind(c(40,5), c(15,-45))
# line1_sp <- spLines(line1)
# line2_sp <- spLines(line2)
# line3_sp <- spLines(line3)
# 
# ## To create buffer around lines
# line2_buff <- gBuffer(line2_sp, width=20)
# plot(line2_sp,add=T)
# plot(line2_buff,add=T)
# 
# 
# 
