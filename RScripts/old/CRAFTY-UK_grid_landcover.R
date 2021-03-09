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
# 

if (FALSE) { 
    
    str(UK_LL_csv)
    
    UK_LL_csv$FID = NULL
    UK_LL_csv$xBNG = NULL
    UK_LL_csv$yBNG = NULL
    
    str(UK_LL_csv)
    colnames(UK_LL_csv) = c("ID", "Longitude", "Latitude")
    write.csv(UK_LL_csv, file = "Basegrid/CRAFTY_UK_coordinates.csv", quote = F, row.names = F)
    # ,X,Latitude,Longitude,Crop.productivity,Forest.productivity,Grassland.productivity,Financial.capital,Human.capital,Social.capital,Manufactured.capital,Urban.capital,FR,BT,Region
    
    beginCluster() 
    
    SPA2019_shp = readOGR("Land Cover data/Protected Areas/SPA areas/GB_SPA_OSGB36_20190326.shp")
    
    str(coordinates(UK_LL))
    
    table(coordinates(SPA2019_LL_r) == coordinates(UK_LL))    
    
    
    UK_LL_sp = SpatialPixels(points = SpatialPoints(cbind(UK_LL_csv$Longitude, UK_LL_csv$Latitude)), tolerance = 1E-1)
    proj4string(UK_LL_sp) = proj4.LL
    UK_BNG_sp = spTransform(UK_LL_sp, CRSobj = crs(proj4.BNG))
    
    plot(UK_BNG_sp)
    
    
    
    # Take Presence/Absence of SPA2019 data at the 1 km grid 
    SPA2019_shp$PA = 1
    SPA2019_r = rasterize(SPA2019_shp[,], UK_BNG, field = "PA", fun ="last", background=0)
    SPA2019_r = mask(SPA2019_r, UK_BNG)
    plot(SPA2019_r, add=F)
    plot(is.na(SPA2019_r), add=F)
    
    plot(SPA2019_shp, add=F)
    # plot(UK_BNG, add=T)
    plot(SPA2019_r, add=T)
    plot(SPA2019_shp, add=T)
    
    
    # SPA2019_r2 =  crop(SPA2019_r, SPA2019_shp[9:12,])
    # 
    # 
    # plot(SPA2019_shp[50:100,])
    # plot(SPA2019_r, add=T, legend=F)
    # plot(SPA2019_shp[,], add=T, col = "red")
    # 
    
    SPA2019_extracted = extract(SPA2019_r, UK_BNG_sp, fun = count)
    
    # SPA2019_spdf = SpatialPointsDataFrame(UK_BNG_sp, data.frame(SPA = SPA2019_extracted))
    # 
    # SPA2019_LL_spdf = spTransform(SPA2019_spdf, CRSobj = crs(proj4.LL))
    # plot(SPA2019_spdf, col = c("red", "blue")[SPA2019_spdf$layer+1])
    #  
    # # the orders preserved
    # # plot(coordinates(SPA2019_LL_spdf)[,1], UK_LL_csv$Longitude)
    # # plot(coordinates(SPA2019_LL_spdf)[,2], UK_LL_csv$Latitude)
    # 
    # SPA2019_LL360_spdf = spTransform(SPA2019_spdf, CRSobj = crs(proj4.LL360))
    # 
    # coordinates(SPA2019_LL360_spdf)
    # # a1 = projectRaster(SPA2019_r, crs = crs(proj4.LL))
    #                     
    # # writeRaster(), filename = "SPA2019_PA_360.tif", overwrite=T)
    
    
    SPA2019_csv = UK_LL_csv
    SPA2019_csv$SPA = SPA2019_extracted
    SPA2019_csv$Longitude = SPA2019_csv$Longitude + 180 
    writeRaster(SPA2019_r, filename = "SPA2019_PA.tif", overwrite=T)
    write.csv(SPA2019_csv, file = "CRAFTY_UK_SPA2019_PA.csv", quote = F, row.names = F)
    
    
    # newLongitude = ((oldLongitude+180) % 360) - 180
    
    ### LCM2015
    
    
    LCM2015_rs = stack("Land Cover data/LCM 2015/LCM2015_GB_1km_percent_cover_target_class.tif")
    LCM2015_rs
    # - LCM2015 has 21 layers. Would it mean 21 land cover types? 
    plot(LCM2015_rs)
    proj4string(LCM2015_rs) # BNG
    
    
    # Appendix 3: Recipe for standard LCM2015 colour mapping.
    # LCM2015 class
    LCM2015_classes = c("Broadleaved woodland", 
                        "Coniferous Woodland", 
                        "Arable and Horticulture", 
                        "Neutral Grassland", 
                        "Improved Grassland", 
                        "Calcareous Grassland", 
                        "Acid grassland", 
                        "Fen, Marsh and Swamp", 
                        "Heather", 
                        "Heather grassland",
                        "Bog", 
                        "Inland Rock", 
                        "Saltwater", 
                        "Freshwater", 
                        "Supra-littoral Rock", 
                        "Supra-littoral Sediment", 
                        "Littoral Rock", 
                        "Littoral sediment", 
                        "Saltmarsh", 
                        "Urban", 
                        "Suburban")
    LCM2015_code = 1:21 
    cbind(LCM2015_classes, LCM2015_code)
    names(LCM2015_rs) = LCM2015_classes
    plot(LCM2015_rs, zlim=c(0, 100))
    
    
    LCM2015_dom_rs = which.max(LCM2015_rs)
    plot(LCM2015_dom_rs)
    
    LCM2015_extracted = extract(LCM2015_dom_rs, UK_BNG_sp, method="simple", fun = count)
    
    
    
    LCM2015_csv = UK_LL_csv
    LCM2015_csv$LCM = LCM2015_extracted
    LCM2015_csv$Longitude = LCM2015_csv$Longitude + 180 
    writeRaster(LCM2015_dom_rs, filename = "LCM2015_1km_dominated.tif", overwrite=T)
    write.csv(LCM2015_csv, file = "CRAFTY_UK_LCM2015_LCM2015.csv", quote = F, row.names = F)
    
    
    
    
    ########## NFI 2016
    
    NFI2016_shp = readOGR("Land Cover data/NFI 2016/NATIONAL_FOREST_INVENTORY_GB_2016.shp")
    # length(table(NFI2016_shp$IFT_IOA))
    
    
    # NFI2016_shp_buffer = gBuffer(NFI2016_shp[64,], width = 5E3)
    # NFI2016_shp_clip = crop(NFI2016_shp, NFI2016_shp_buffer)
    # 
    
    # NFI2016_r = rasterize(x = NFI2016_shp_clip[,], y= UK_BNG, field = "IFT_IOA", fun = function(x, na.rm) {str(x); modal(x)})
    # table(getValues(NFI2016_r))
    # 
    # NFI2016_r
    # # NFI2016_r = mask(NFI2016_r, UK_BNG)
    # # factorValues(NFI2016_r)
    # # NFI2016_r = setValues(NFI2016_r, as.vector(factor(getValues(NFI2016_r))))
    # 
    # # rasterize
    # ras <- rasterize(x = dat, y = r.raster, field = "ID")
    # 
    # # ratify raster
    # NFI2016_r = ratify(NFI2016_r)
    # 
    # 
    # plot(NFI2016_shp_buffer)
    # plot(NFI2016_shp_clip, add=T, col = 1 + as.numeric(NFI2016_shp_clip$Category))
    # 
    # plot(NFI2016_shp_buffer)
    # # plot(NFI2016_r, add=T, col = rainbow(3))
    # plot(NFI2016_fs_r, add=T, col = rainbow(27), legend=F)
    # 
    # plot(NFI2016_shp_clip, add=T, col = as.numeric(NFI2016_shp_clip$IFT_IOA))
    # 
    # 
    # NFI2016_shp_bl = NFI2016_shp[NFI2016_shp$IFT_IOA == "Broadleaved",]
    # 
    # NFI2016_shp_clip_bl = NFI2016_shp_clip[NFI2016_shp_clip$IFT_IOA == "Broadleaved",]
    # # plot(NFI2016_shp_clip_bl)
    # # 
    # 
    # NFI2016_shp_bl_simple = NFI2016_shp_bl
    # NFI2016_shp_bl_simple$OBJECTID = NULL
    # NFI2016_shp_bl_simple$Category = NULL
    # NFI2016_shp_bl_simple$IFT_IOA = NULL
    # NFI2016_shp_bl_simple$Shape_Leng = NULL
    # NFI2016_shp_bl_simple$Shape_Area = NULL 
    # NFI2016_shp_bl_simple$Hectares = NULL
    # NFI2016_shp_bl_simple$PA = 1 
    # # plot(NFI2016_shp_bl_simple)
    # 
    # beginCluster()
    # system.time({
    # NFI2016_bl_r = rasterize(x = NFI2016_shp_bl_simple[,], y= UK_BNG, field = "PA", background = 0, getCover=T)
    # })
    # 
    # plot(NFI2016_bl_r)
    # NFI2016_bl_clip_r = crop(NFI2016_bl_r, NFI2016_shp_buffer)
    # 
    # plot(NFI2016_shp_clip_bl)
    # plot(NFI2016_bl_clip_r, add=T)
    # plot(NFI2016_shp_clip_bl, add=T, col = "black")
    # 
    # writeRaster(NFI2016_bl_r, filename = "NFI2016_bl.tif")
    # 
    # beginCluster()
    
    # too slow 
    # NFI2016_bl_r = rasterize(x = NFI2016_shp_bl[,], y= UK_BNG, field = "OBJECTID", getCover=F)
    # library(stars)
    # UK_BNG_sf = read_stars("Basegrid/ukcp18_tmax.tif")
    # 
    # NFI2016_sf_r = st_rasterize(st_as_sf(NFI2016_shp_bl), template = st_as_stars(st_bbox(UK_BNG_sf), values = NA_real_), driver = "GTiff")
    # plot(NFI2016_sf_r)
    # 
    # ratify(NFI2016_shp_clip$IFT_IOA)
    # 
    # NFI2016_bl_cover_r = rasterize(x = NFI2016_shp_clip[,], y= UK_BNG, field = "IFT_IOA", fun = "last")
    # 
    # plot(NFI2016_shp_clip, col = factor(NFI2016_shp_clip$IFT_IOA))
    # plot(NFI2016_bl_cover_r, add=T)
    # plot(NFI2016_shp_clip, col = factor(NFI2016_shp_clip$IFT_IOA), add=T)
    # 
    # 
    # NFI2016_bl_r =  crop(NFI2016_bl_r, NFI2016_shp_clip_bl)
    # plot(NFI2016_bl_r)
    # 
    # 
    # NFI2016_bl_fs_r =  fasterize::fasterize(st_as_sf(NFI2016_shp_clip_bl), fun = "any", raster = UK_BNG, field = "IFT_IOA", background = 0)
    # 
    # NFI2016_sf_r = st_rasterize(st_as_sf(NFI2016_shp_clip_bl)["IFT_IOA"], template = st_as_stars(st_bbox(NFI2016_bl_r), res= 1000, values = NA_real_), driver = "GTiff")
    # 
    # 
    # 
    # plot(NFI2016_shp_clip_bl, add=T, col = as.numeric(NFI2016_shp_clip_bl$IFT_IOA))
    # library(ggplot2)
    # 
    # plot(NFI2016_bl_r, add=T, legend=F)
    # plot(NFI2016_bl_fs_r, add=T, legend=F, col = "red")
    # plot(NFI2016_sf_r) # , add=T, legend=F, col = "green")
    # plot(st_as_sf(NFI2016_shp_clip_bl))
    # # install.packages("stars")
    # library(stars)
    # library(sf)
    # # a = raster(as(NFI2016_shp_clip, "Spatial"), ncols = 6, nrows = 4)
    # # plot(a)
    # 
    # 
    # 
    # 
    # NFI2016_shp_sf = st_as_sf(NFI2016_shp)
    # 
    # UK_BNG_sf = read_stars("Basegrid/ukcp18_tmax.tif")
    # 
    # NFI2016_shp_sf["IFT_IOA"]
    # 
    # sf::st_as_sf(UK_BNG)
    # NFI2016_sf_r = st_rasterize(NFI2016_shp_sf["IFT_IOA"], template = st_as_stars(st_bbox(UK_BNG_sf), values = NA_real_), driver = "GTiff")
    # plot(NFI2016_sf_r)
    # 
    # 
    # NFI2016_fs_r =  fasterize::fasterize(NFI2016_shp_sf, fun = "last", raster = r, field = "IFT_IOA", background = NA)
    # 
    # plot(NFI2016_fs_r)
    # 
    # r = raster(NFI2016_shp_sf, res=1000)
    # 
    # 
    # p1 <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60), c(-180,-20))
    # hole <- rbind(c(-150,-20), c(-100,-10), c(-110,20), c(-150,-20))
    # p1 <- list(p1, hole)
    # p2 <- list(rbind(c(-10,0), c(140,60), c(160,0), c(140,-55), c(-10,0)))
    # p3 <- list(rbind(c(-125,0), c(0,60), c(40,5), c(15,-45), c(-125,0)))
    # pols <- st_sf(value = rep(1,3),
    #               geometry = st_sfc(lapply(list(p1, p2, p3), st_polygon)))
    # r <- raster(pols, res = 1)
    # r <- fasterize(pols, r, field = "value", fun="sum")
    # plot(r)
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # NFI2016_r = fasterize::raster(st_as_sf(NFI2016_sf_r))
    # 
    # plot(NFI2016_r)
    # 
    # st_raster_type(NFI2016_sf_r)
    # stars::st_rasterize(NFI2016_sf_r)
    # plot(as_Spatial(NFI2016_sf_r))
    # 
    # ## stars object with 2 dimensions and 1 attribute
    # ## attribute(s):
    # ##  dens [1/km^2]   
    # ##  Min.   : 0.255  
    # ##  1st Qu.: 1.226  
    # ##  Median : 1.932  
    # ##  Mean   : 3.346  
    # ##  3rd Qu.: 3.826  
    # ##  Max.   :21.248  
    # ##  NA's   :4808    
    # ## dimension(s):
    # ##   from  to offset delta                 refsys point values    
    # ## x    1 162 123829  5000 NAD83 / North Carolina FALSE   NULL [x]
    # ## y    1  61 318260 -5000 NAD83 / North Carolina FALSE   NULL [y]
    # plot(nc.st)
    # 
    
    
    
    
    
    
    doNFI2016 = F 
    
    if (doNFI2016) { 
        
        NFI2016_shp = readOGR("Land Cover data/NFI 2016/NATIONAL_FOREST_INVENTORY_GB_2016.shp")
        # length(table(NFI2016_shp$IFT_IOA))
        
        NFI2016_shp$OBJECTID = NULL
        NFI2016_shp$Category = NULL
        NFI2016_shp$Shape_Leng = NULL
        NFI2016_shp$Shape_Area = NULL 
        NFI2016_shp$Hectares = NULL
        
        
        IOA_names = levels(NFI2016_shp$IFT_IOA) 
        table(NFI2016_shp$IFT_IOA) 
        
        
        
        
        # NFI2016_shp_buffer = gBuffer(NFI2016_shp[64,], width = 5E3)
        # NFI2016_shp_clip = crop(NFI2016_shp, NFI2016_shp_buffer)
        
        # l_idx = 1 
        library(doMC)
        registerDoMC(12)
        
        # for (l_idx in 1:length(IOA_names)) {
        foreach (l_idx = (1:length(IOA_names))) %dopar% { 
            
            IOA_tmp = IOA_names[l_idx]
            print(IOA_tmp)
            NFI2016_shp_tmp = NFI2016_shp[NFI2016_shp$IFT_IOA == IOA_tmp,]
            NFI2016_shp_tmp
            
            NFI2016_shp_tmp$PA = 1 
            NFI2016_shp_tmp$IFT_IOA = NULL
            
            
            # plot(NFI2016_shp_bl_simple)beginCluster()
            system.time({
                NFI2016_IOA_r = rasterize(x = NFI2016_shp_tmp[,], y= UK_BNG, field = "PA", background = 0, getCover=T)
            })
            
            
            plot(NFI2016_IOA_r)
            
            writeRaster(NFI2016_IOA_r, filename = paste0("NFI2016_", IOA_tmp, ".tif"))
            
        }
        IOA_names[5] = "Cloud shadow"
        
        ## read cover fractions 
        
        rs = foreach (l_idx = (1:length(IOA_names))) %do% {
            
            IOA_tmp = IOA_names[l_idx]
            print(IOA_tmp)
            NFI2016_shp_tmp = NFI2016_shp[NFI2016_shp$IFT_IOA == IOA_tmp,]
            
            raster(paste0("Land Cover data/NFI 2016/Fraction/NFI2016_", IOA_tmp, ".tif"))
            
        }
        
        rs = stack(rs)
        
        plot(rs[[5]])
        
        beginCluster()
        nfi_sum = sum(rs)
        plot(nfi_sum)
        plot(NFI2016_shp, add=T)
        
        NFI2016_Top1 = which.max(rs)
        plot(NFI2016_Top1)
        writeRaster(NFI2016_Top1, filename = paste0("Output/NFI2016_dominated_1km.tif"))
        write.csv(data.frame(IFT_IOA=IOA_names), file = "Output/NFI2016_names.csv", row.names = T, quote=F)
            
        
    }
    NFI2016_Top1 = raster("Output/NFI2016_dominated_1km.tif")
    NFI2016_extracted = extract(NFI2016_Top1, UK_BNG_sp, method="simple", fun = modal)
    
    NFI2016_csv = UK_LL_csv
    NFI2016_csv$NFI = NFI2016_extracted
    NFI2016_csv$Longitude = NFI2016_csv$Longitude + 180 
    write.csv(NFI2016_csv, file = "Output/CRAFTY_UK_NFI2016_NFI.csv", quote = F, row.names = F)
    
    
    beginCluster()
    
    GRI4_UK_shp = crop(NFI2016_shp, UK_LL)
    endCluster()
    writeOGR(GRI4_UK_shp,dsn = "GRIP4_Region4_vector_shp_EU", layer ="GRIP4_region4_UK_cropped", driver = "ESRI Shapefile")
    rm(grip4_shp)
    
    
    
    
} else { 
    GRI4_UK_shp = readOGR("GRIP4_Region4_vector_shp_EU/GRIP4_region4_UK_BNG.shp")
}

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
# 

GRIP4_Typ1_density = raster("GRIP4_Region4_vector_shp_EU/GRIP4_Typ1_Density.tif")
GRIP4_Typ2_density = raster("GRIP4_Region4_vector_shp_EU/GRIP4_Typ2_Density.tif")
GRIP4_Typ3_density = raster("GRIP4_Region4_vector_shp_EU/GRIP4_Typ3_Density.tif")
GRIP4_Typ4_density = raster("GRIP4_Region4_vector_shp_EU/GRIP4_Typ4_Density.tif")
GRIP4_Typ5_density = raster("GRIP4_Region4_vector_shp_EU/GRIP4_Typ5_Density.tif")


na_mask = !is.na(UK_BNG)

beginCluster()
GRIP4_Typ1_density = projectRaster(GRIP4_Typ1_density, UK_BNG) * na_mask
GRIP4_Typ2_density = projectRaster(GRIP4_Typ2_density, UK_BNG) * na_mask
GRIP4_Typ3_density = projectRaster(GRIP4_Typ3_density, UK_BNG) * na_mask
GRIP4_Typ4_density = projectRaster(GRIP4_Typ4_density, UK_BNG) * na_mask
GRIP4_Typ5_density = projectRaster(GRIP4_Typ5_density, UK_BNG) * na_mask

GRIP4_density_rs = stack(GRIP4_Typ1_density, GRIP4_Typ2_density, GRIP4_Typ3_density, GRIP4_Typ4_density, GRIP4_Typ5_density)
names(GRIP4_density_rs) = paste0("RoadType", 1:5) 

writeOGR(GRIP4_density_rs,dsn = "GRIP4_Region4_vector_shp_EU", layer ="GRIP4_region4_UK_BNG_Typ1", driver = "ESRI Shapefile")


dens_max = max(getValues(GRIP4_density_rs), na.rm=T)
speed_limit = c(70, 70, 60, 30, 30)  # https://www.gov.uk/speed-limits
dens_weights = speed_limit / sum(speed_limit)

RoadCapital = sum(stack( sapply(1:5, FUN = function(x) GRIP4_density_rs[[x]] * dens_weights[x] )  ), na.rm=T) 

pdf("RoadDensity_UK.pdf", width = 12, height = 16)
par(mfrow=c(1,1), mar=c(4,4,4,4), oma=4)
plot(RoadCapital, main = "Road capital (weighted road density)")
plot(GRIP4_density_rs, main = paste0("Road type", 1:5, " (km/km2)"))
dev.off() 

#  
# 
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








tmax_annual_r_usingraster = mean(UK_rs, na.rm=F)
# 
# plot(UK_rs_mean)

par(mfrow=c(1,2))
plot(tmax_annual_r) 
plot(tmax_annual_r_usingraster,add=T, col="red")



