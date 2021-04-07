

path_wd = "~/Nextcloud/CRAFTY/CRAFTY_UK/"
path_data = "~/Nextcloud/workspace_newEU/CRAFTY UK input CSV files/"
path_output =  "~/Nextcloud/CRAFTY/Output/"

setwd(path_wd)

source("RScripts/CRAFTY-UK_grid_common.R")


proj4.LL = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4.LL360 = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 +lon_wrap=180"
proj4.BNG = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.1502,0.247,0.8421,-20.4894 +units=m +no_defs"



CHESS_BNG_csv = read.csv("Basegrid/CHESS_1k_grid.csv") # BNG perhaps
# proj4string(UK_rs) ="+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"
# plot(UK_rs[[1]])
#  
CHESS_BNG_csv = CHESS_BNG_csv[, c("FID", "POINT_X", "POINT_Y")]
CHESS_BNG_sp = SpatialPixels(points = SpatialPoints(cbind(CHESS_BNG_csv$POINT_X, CHESS_BNG_csv$POINT_Y), proj4string =  crs(proj4.BNG)))

# plot(CHESS_BNG_sp)


CHESS_BNG_r  = raster(SpatialPixelsDataFrame(CHESS_BNG_sp, data =data.frame(CHESS_BNG_csv$FID)))
# plot(UK_BNG_r)

CHESS_mask_r = CHESS_BNG_r
CHESS_mask_r[!is.na(CHESS_mask_r)] = 1 

CHESS_LL_sp = spTransform(CHESS_BNG_sp, CRSobj = crs(proj4.LL))
CHESS_LL_coords = data.frame(coordinates(CHESS_LL_sp))
colnames(CHESS_LL_coords) = c("Longitude", "Latitude")


x = CHESS_BNG_csv$POINT_X
y = CHESS_BNG_csv$POINT_Y




chess_val = getValues(CHESS_BNG_r)
table(is.na(chess_val))

chess_idx = (match(CHESS_BNG_csv$FID, chess_val))
#  
# val = getValues(CHESS_BNG_r)
# val[] = NA
# val[chess_idx] =  basealc_csv_df$FR
# #val[cells] = as.numeric(as.factor(hx$borough))
# r2 = setValues(CHESS_BNG_r, val)
# plot(r2)


rowcol_m_R = raster::rowColFromCell(CHESS_BNG_r, cell = 1:ncell(CHESS_BNG_r))
rowcol_m = rowcol_m_R

rowcol_m[,1] = abs(rowcol_m[,1] - (max(rowcol_m[,1]) + 1 ))
# rowcol_m[,2] = abs(rowcol_m[,2] - (max(rowcol_m[,2]) + 1 ))

cellids = data.frame(Cell_ID= 1:length(CHESS_BNG_csv$FID), FID = CHESS_BNG_csv$FID, X_col = rowcol_m[chess_idx,2], Y_row= rowcol_m[chess_idx,1], Longitude =  CHESS_LL_coords$Longitude, Latitude= CHESS_LL_coords$Latitude, xcoord_bng = CHESS_BNG_csv$POINT_X, ycoord_bng = CHESS_BNG_csv$POINT_Y )
colnames(cellids)


# val[] =  rowcol_m[,2] # col
# #val[cells] = as.numeric(as.factor(hx$borough))
# r2 = setValues(CHESS_BNG_r, val)
# plot(r2)
# 
# val[] = NA
# val[chess_idx] =  rowcol_m[chess_idx,2]

# r2 = setValues(CHESS_BNG_r, val)
# plot(r2, main = "col")
# val[] = NA
# val[chess_idx] =  rowcol_m[chess_idx,1]
# 
# r2 = setValues(CHESS_BNG_r, val)
# 
# plot(r2)
# plot(CHESS_BNG_r)
# 




# Pass the fill.na function to raster::focal and check results. The pad argument creates virtual rows/columns of NA values to keep the vector length constant along the edges of the raster. This is why we can always expect the fifth value of the vector to be the focal value in a 3x3 window thus, the index i=5 in the fill.na function.

fillCoastalPixels <- function(r_in, boundary_r, maskchar=NA, width=3, n_interpol = 1) { 
    
    
    if (!is.na(maskchar)) { 
        r_in[r_in==maskchar] = NA
    }
    
    r_in = projectRaster(r_in, boundary_r)
    
    # na_num = sum(getValues(is.na(boundary_r)))
    # na_num_in = sum(getValues(is.na(r_in)))
    
    
    fill.na <- function(x, i=width+2) {
        if( is.na(x)[i] ) {
            return(mean(x, na.rm=TRUE))
        } else {
            return(x[i])
        }
    }  
    
    
    # while(na_num_in > na_num) 
    for (i in 1:n_interpol) { 
        
        # r2 <- focal(r, w = matrix(1,3,3), fun = fill.na, 
        # pad = TRUE, na.rm = FALSE )
        r_in <- focal(r_in, w = matrix(1,width,width), fun = fill.na, pad = TRUE, na.rm=F)
        na_num_in = sum(getValues(is.na(r_in)))
        print(na_num_in)
    }
    
    return(r_in * boundary_r)
    
}


# plot(BNG_r_tmp)
# plot(Shetland_NUTS_BNG_r, add=T, col="red")
# plot(Orkney_NUTS_BNG_r, add=T, col="blue")


fillShetland <- function(r_in) { 
    
    Orkney_r_tmp = projectRaster(r_in, Orkney_NUTS_BNG_r)
    ork_v = getValues(Orkney_r_tmp)
    
    in_df = cbind(Ork=ork_v, Orkney_elev_df)
    in_df = in_df[!is.na(rowSums(in_df)),]
    rf_tmp = randomForest(Ork ~ ., data = in_df, ntree=1000, nodesize=1)
    shetland_v = predict(rf_tmp, newdata = Shetland_elev_df)
    
    # gbm_tmp = gbm(formula = Ork ~., data = in_df, interaction.depth = 2)
    # shetland_v = predict(gbm_tmp, newdata = Shetland_elev_df)
    
    out_r = setValues(Shetland_elev_r, shetland_v) # * (Shetland_NUTS_BNG_r!=1)
    
    # rng = raster::cellStats(Orkney_r_tmp, stat = range)
    # par(mfrow=c(2,2))
    # 
    # plot(Orkney_elev_r)
    # plot(Shetland_elev_r)
    # 
    # 
    # plot(Orkney_r_tmp)
    # plot(out_r, zlim=rng)
    # 
    # plot(focal(out_r, w = matrix(1,3,3), fun = sum, na.rm=T))
    
    
    out_r_big = mosaic(r_in, out_r, fun = max)
    # plot(r_in)
    # plot(r_out)
    
    # plot(Orkney_r_tmp)
    # plot(out_r, add=F)
    # plot(r_in, add=T)
    
    # plsot(out_r, add=T)
    
    return(out_r_big)
}





doLCM = FALSE 


if (doLCM) { 
    ### LCM2015
    
    LCM2015_england_rs = stack("Land Cover data/LCM 2015/LCM2015_GB_1km_percent_cover_target_class.tif")
    LCM2015_nireland_rs = stack("Land Cover data/LCM 2015/lcm2015_ni_1km_percent_cover_target_class.tif")
    # - LCM2015 has 21 layers. Would it mean 21 land cover types? 
    # plot(LCM2015_england_rs)
    # plot(LCM2015_nireland_rs)
    
    proj4string(LCM2015_england_rs) # BNG
    
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
    names(LCM2015_england_rs) =names(LCM2015_nireland_rs) = LCM2015_classes
    plot(LCM2015_england_rs, zlim=c(0, 100))
    plot(LCM2015_nireland_rs, zlim=c(0, 100))
    
    
    LCM2015_england_sum_rs = sum(LCM2015_england_rs)
    LCM2015_england_mask_r  = LCM2015_england_sum_rs > 0 
    LCM2015_england_mask_r[LCM2015_england_mask_r == 0 ] = NA
    
    plot(LCM2015_england_rs * LCM2015_england_mask_r)
    LCM2015_england_rs = LCM2015_england_rs * LCM2015_england_mask_r
    
    LCM2015_england_dom_rs = which.max(LCM2015_england_rs)
    
    LCM2015_nireland_rs2 = projectRaster(LCM2015_nireland_rs, LCM2015_england_rs, method = "ngb")
    
    LCM2015_nireland_mask_r = sum(LCM2015_nireland_rs2) > 0 
    LCM2015_nireland_mask_r[LCM2015_nireland_mask_r == 0 ] = NA
    
    LCM2015_nireland_rs3 = LCM2015_nireland_rs2 * LCM2015_nireland_mask_r
    
    
    
    
    LCM2015_nireland_dom_rs = which.max(LCM2015_nireland_rs3)
    
    # plot(is.na(LCM2015_nireland_dom_rs))
    # LCM2015_nireland_dom_rs[is.na(LCM2015_nireland_dom_rs)] = 0 
    
    # plot(LCM2015_england_dom_rs)
    # plot(LCM2015_nireland_dom_rs, add=T)
    
    
    LCM2015_dom_rs = mosaic(LCM2015_england_dom_rs, LCM2015_nireland_dom_rs, fun = max, na.rm=T)
    plot(LCM2015_dom_rs)
    
    LCM2015_extracted = extract(LCM2015_dom_rs, CHESS_BNG_sp, method="simple", fun = count)
    
    
    LCM2015_csv_df = data.frame(FID = CHESS_BNG_csv$FID, long = CHESS_LL_coords$Longitude + 180, lat = CHESS_LL_coords$Latitude, X_BNG = CHESS_BNG_csv$POINT_X, Y_BNG = CHESS_BNG_csv$POINT_Y)
    
    LCM2015_csv_df$LCM = LCM2015_extracted
    
    writeRaster(LCM2015_dom_rs, filename = paste0(path_output, "/LCM2015_UK_1km_dominated.tif"), overwrite=T)
    
    write.csv(LCM2015_csv_df, file = paste0(path_output, "/CRAFTY_GB_LCM2015_LCM.csv"), quote = F, row.names = F)
    
    
} else { 
    
    LCM2015_dom_rs = raster(paste0(path_output, "/LCM2015_1km_dominated.tif"))
}

UK_BNG_r = LCM2015_dom_rs[[1]]
UK_LL_r = projectRaster(UK_BNG_r, crs = proj4.LL, method = "ngb")



doShetland =FALSE
if (doShetland) {
    # plot(UK_BNG_r, add=T)
    # plot(CHESS_BNG_sp, add=T)
    Shetland_NUTS_BNG_shp = spTransform(NUTS_shp[NUTS_shp$nuts318nm == "Shetland Islands",], CRSobj = proj4.BNG)
    Orkney_NUTS_BNG_shp = spTransform(NUTS_shp[NUTS_shp$nuts318nm == "Orkney Islands",], CRSobj = proj4.BNG)
    
    beginCluster()
    Shetland_ext  = gBuffer(Shetland_NUTS_BNG_shp[,], width = 1E4)
    Shetland_ext =  extent(Shetland_ext)
    
    Shetland_NUTS_BNG_r = crop(UK_BNG_r, y = Shetland_ext) # , UK_BNG_r, field = "objectid", fun ="mean", na.rm=T, background=0)
    
    Shetland_NUTS_BNG_r[Shetland_NUTS_BNG_r==0] = NA
    
    # Shetland 
    if (FALSE) { 
        plot(UK_BNG_r, add=F)
        plot(Shetland_NUTS_BNG_r, add=T, col="red")
        plot(Orkney_NUTS_BNG_r, add=T, col="blue")
    }
    Orkney_ext  = gBuffer(Orkney_NUTS_BNG_shp[,], width = 1E2)
    Orkney_ext =  extent(Orkney_ext)
    Orkney_NUTS_BNG_r = crop(UK_BNG_r, y = Orkney_ext) # , UK_BNG_r, field = "objectid", fun ="mean", na.rm=T, background=0)
    Orkney_NUTS_BNG_r[Orkney_NUTS_BNG_r==0] = NA
    
    writeOGR(Shetland_NUTS_BNG_shp, dsn = "SpatialUnit", layer = "Shetland_NUTS_BNG", driver = "ESRI Shapefile")
    writeOGR(Orkney_NUTS_BNG_shp, dsn = "SpatialUnit", layer = "Orkney_NUTS_BNG", driver = "ESRI Shapefile")
    
    writeRaster(Orkney_NUTS_BNG_r, paste0(path_output, "/Orkney_NUTS_BNG_r.tif"))
    writeRaster(Shetland_NUTS_BNG_r, paste0(path_output, "/Shetland_NUTS_BNG_r.tif"))
    
} else { 
    
    Orkney_NUTS_BNG_r= raster(paste0(path_output, "/Orkney_NUTS_BNG_r.tif"))
    Shetland_NUTS_BNG_r = raster(paste0(path_output, "/Shetland_NUTS_BNG_r.tif"))
    
}




# plot(Orkney_NUTS_BNG_shp, add=F, col="green")
# plot(Orkney_NUTS_BNG_r, add=T, col="blue")
# plot(UK_BNG_r, add=T)
# plot(Orkney_NUTS_BNG_shp, add=T, col="green")


# plot(Shetland_NUTS_BNG_shp, add=F, col="green")
# plot(Shetland_NUTS_BNG_r, add=F, col="white")
# plot(Shetland_NUTS_BNG_shp, add=T, col="green")
# plot(UK_BNG_r, add=T)
# plot(Shetland_NUTS_BNG_r, add=T)
# plot(Shetland_NUTS_BNG_shp, add=T, col="green")
# 
# 
# plot(Shetland_NUTS_BNG_r, add=F)
# plot(UK_BNG_r, add=T)
# plot(Shetland_NUTS_BNG_r- UK_BNG_r)

doElevation = FALSE 
if (doElevation) { 
    
    
    ########## Elevation 
    
    GB_elev <- getData('alt', country = "GB")
    GB_elev_BNG = projectRaster(GB_elev, CHESS_BNG_r, method = "bilinear")
    UK_terrain_BNG <- projectRaster(terrain(GB_elev_BNG, opt=c('slope', 'aspect'), unit='degrees'), CHESS_BNG_r, method = "bilinear")
    
    # plot(UK_terrain_BNG)
    # plot(GB_elev_BNG, add=T, col="red")
    
    
    
    GB_elev_BNG_filled = fillCoastalPixels(GB_elev_BNG, boundary_r = CHESS_mask_r, width = 3, n_interpol = 5) 
    GB_slope_BNG_filled = fillCoastalPixels(UK_terrain_BNG$slope, boundary_r = CHESS_mask_r, width = 3, n_interpol = 5) 
    GB_aspect_BNG_filled = fillCoastalPixels(UK_terrain_BNG$aspect, boundary_r = CHESS_mask_r, width = 3, n_interpol = 5) 
    
    
    # plot(CHESS_BNG_r)
    # plot(GB_elev_BNG_filled, add=T)
    
    
    
    Orkney_elev_r = projectRaster(GB_elev_BNG_filled, Orkney_NUTS_BNG_r)
    # plot(Orkney_elev_r)
    Shetland_elev_r = projectRaster(GB_elev_BNG_filled, Shetland_NUTS_BNG_r)
    # plot(Shetland_elev_r)
    Orkney_aspect_r = projectRaster(GB_aspect_BNG_filled, Orkney_NUTS_BNG_r)
    Shetland_aspect_r = projectRaster(GB_aspect_BNG_filled, Shetland_NUTS_BNG_r)
    Orkney_slope_r = projectRaster(GB_slope_BNG_filled, Orkney_NUTS_BNG_r)
    Shetland_slope_r = projectRaster(GB_slope_BNG_filled, Shetland_NUTS_BNG_r)
    
    writeRaster(Orkney_elev_r, paste0(path_output, "/Orkney_elev_r.tif"))
    writeRaster(Shetland_elev_r, paste0(path_output, "/Shetland_elev_r.tif"))
    writeRaster(Orkney_aspect_r, paste0(path_output, "/Orkney_aspect_r.tif"))
    writeRaster(Shetland_aspect_r, paste0(path_output, "/Shetland_aspect_r.tif"))
    writeRaster(Orkney_slope_r, paste0(path_output, "/Orkney_slope_r.tif"))
    writeRaster(Shetland_slope_r, paste0(path_output, "/Shetland_slope_r.tif"))
    
} else { 
    Orkney_elev_r = raster(paste0(path_output, "/Orkney_elev_r.tif"))
    Shetland_elev_r = raster(paste0(path_output, "/Shetland_elev_r.tif"))
    Orkney_aspect_r = raster(paste0(path_output, "/Orkney_aspect_r.tif"))
    Shetland_aspect_r = raster(paste0(path_output, "/Shetland_aspect_r.tif"))
    Orkney_slope_r = raster(paste0(path_output, "/Orkney_slope_r.tif"))
    Shetland_slope_r = raster(paste0(path_output, "/Shetland_slope_r.tif"))
    
}

Orkney_elev_df = data.frame(Elev = getValues(Orkney_elev_r), Slope = getValues(Orkney_slope_r), Aspect = getValues(Orkney_aspect_r))
Shetland_elev_df = data.frame(Elev = getValues(Shetland_elev_r), Slope = getValues(Shetland_slope_r), Aspect = getValues(Shetland_aspect_r))




doSuitability = FALSE

if (doSuitability) {
    
    ### Suitability maps
    # Suitability_rs = stack(paste0("CRAFTY UK Capital files/Suitability/ModelledBaseline/uk_1km_suitability_maskout_interact_20yr-mean-annual_199012-201011_class", c("1_Arable", "2_Wetland", "3_Improved_grassland", "5_Semi_natural_grassland"), ".tif")) # Dec 2020
    
    # 
    suitability_years = c(2000, 2010, 2020, 2030, 2040, 2050, 2060, 2070)
    # 
    # for (suit_idx in seq_along(suitability_years)) {  
    
    
    # ignore forest suiltability as we use woodland capital files
    # ignore arable as we have adjusted arable below
    # uk_1km_suitability_binom_maskout_interact_ukcp18-speed_rcp26_bias_corrected_01_1_arable_20yr-mean-annual_203012-205011.tif
    Suitability_RCP45_rs_l = lapply(suitability_years, FUN = function(year) stack(paste0(path_data, "Capital/Suitability/Final modelled scenario files 7Apr2021/uk_1km_suitability_binom_maskout_interact_ukcp18-speed_rcp45_bias_corrected_01_", c("1_arable", "2_wetland", "3_improved_grassland", "5_semi_natural_grassland"), "_20yr-mean-annual_", year - 10, "12-", year + 10, "11.tif")) ) # Dec 2020 RCP85 
    
    
    Suitability_RCP85_rs_l = lapply(suitability_years, FUN = function(year) stack(paste0(path_data, "Capital/Suitability/Final modelled scenario files 7Apr2021/uk_1km_suitability_binom_maskout_interact_ukcp18-speed_rcp85_bias_corrected_01_", c("1_arable", "2_wetland", "3_improved_grassland", "5_semi_natural_grassland"), "_20yr-mean-annual_", year - 10, "12-", year + 10, "11.tif")) ) # Dec 2020 RCP85 
    
    names(Suitability_RCP45_rs_l) = names(Suitability_RCP85_rs_l) = suitability_years
    
    # Scenarios adjusted by dIAP (riam), # arable 
    Suitability_Arable_rs_l = lapply(c(45, 85), FUN = function(rcp) lapply(suitability_years[-c(1:2)], FUN = function(year) {
        # print(year);
        raster(paste0(path_data, "Capital/Suitability/New arable suitability/RCP", rcp ,"_", year, "_YD.tif")); # originally it's rcP(!) not rcp.. 
    }) %>% stack # Mar 2021 
    )
    names(Suitability_Arable_rs_l) = c("RCP4_5", "RCP8_5")
    names(Suitability_Arable_rs_l[[1]]) = suitability_years[-c(1,2)] 
    names(Suitability_Arable_rs_l[[2]]) = suitability_years[-c(1,2)] 
    
    # correct < 0 values
    Suitability_Arable_rs_l[[1]]= raster::clamp(Suitability_Arable_rs_l[[1]], lower=0, useCalue = T)
    Suitability_Arable_rs_l[[2]]= raster::clamp(Suitability_Arable_rs_l[[2]], lower=0, useCalue = T)
    
    # Method
    # 1.       Standardise RiAM yield between 0 and 12 (max for Europe in both scenarios, UK max was 11.4)
    # 2.       Correct where zero at baseline to the value of 2020 (which could also be zero).
    # 3.       Calculate standardised yield anomaly (scenario – baseline standardised yield)
    # 4.       Interpolate to 1km SYAnomaly using IDW and 9 points (self and surrounding cells) to provide values at edges.
    # a.       Note I didn’t restrict the range on these points to make sure the area outside of the points is well covered but obviously the further from data the less reliable.
    # b.       I used IDW as in theory the data for a given cell is “correct”. Alternatives such as kriging allow distant cells more influence.
    # 5.       Calculate Mask from baseline maps where yield = 0 (black areas on map below)
    # 6.       Calculate scenario yield = Baseline yield map + (1kmSYAnomaly * YieldMask)
    # 
    # Stage two fixes most errors and most of the fixes are in Scotland. On stage 2 there are still some locations outside of masked regions that swap from zero to high at later scenarios. But there aren’t many and I’m not sure we have time to massage the data much more. It may be that our mask is too small.
    # 
    # plot(Suitability_RCP85_rs_l[["2000"]][[1]]) # 2000 Arable
    # plot(Suitability_RCP85_rs_l[["2010"]][[1]]) # 2000 Arable
    # 
    # par(mfrow=c(3,3))
    # plot(Suitability_RCP85_rs_l[[1]][[1]], zlim=c(0,max(maxValue(Suitability_Arable_rs_l[[2]])))) # 2000
    # plot(Suitability_RCP85_rs_l[[2]][[1]], zlim= c(0, max(maxValue(Suitability_Arable_rs_l[[2]])))) # 2000
    # plot(Suitability_RCP85_rs_l[[3]][[1]], zlim=c(0, max(maxValue(Suitability_Arable_rs_l[[2]]))))# 2000
    # plot(Suitability_RCP85_rs_l[[4]][[1]], zlim= c(0, max(maxValue(Suitability_Arable_rs_l[[2]])))) # 2000
    # plot(Suitability_Arable_rs_l[[1]][[1]], zlim=c(0, max(maxValue(Suitability_Arable_rs_l[[2]])))) # 2020-2070
    # plot(Suitability_Arable_rs_l[[2]][[1]], zlim=c(0, max(maxValue(Suitability_Arable_rs_l[[2]])))) # 2020-2070
    # plot(Suitability_Arable_rs_l[[1]][[2]], zlim=c(0, max(maxValue(Suitability_Arable_rs_l[[2]])))) # 2020-2070
    # plot(Suitability_Arable_rs_l[[2]][[2]], zlim=c(0, max(maxValue(Suitability_Arable_rs_l[[2]])))) # 2020-2070
    # 
    # plot(Suitability_Arable_rs_l[[1]], zlim=c(0, max(maxValue(Suitability_Arable_rs_l[[1]])))) # 2020-2070
    # plot(Suitability_Arable_rs_l[[2]], zlim=c(0, max(maxValue(Suitability_Arable_rs_l[[2]])))) # 2020-2070
    # 
    # plot(Suitability_Arable_rs_l[[1]][[2]])       # RCP45 2020
    # plot(Suitability_Arable_rs_l[[2]][[2]])       # RCP85 2030      
    # 
    # # plot(Suitability_rs)
    # # plot(sum(Suitability_rs))
    # 
    # 
    # endCluster()
    # beginCluster(12)
    # cl = makeCluster()
    # registerDoSNOW(cl)
    
    
    # Conclusion (29 Mar 2021 by ABS)
    # 2000 for the baseline 
    # take arable suitability from 2030 onward
    
    
    # baseline (the first snapshot of RCP85)
    Suitability_Baseline_rs = Suitability_RCP85_rs_l[["2000"]] # 2000 is our baseline
    
    # RCP45 
    Suitability_RCP45_rs_l_final = Suitability_RCP45_rs_l[-c(2:3)]
    names(Suitability_RCP45_rs_l_final)[1] = "2020" # 2000 is our new 2020 
    
    Suitability_RCP45_rs_l_final[["2030"]][[1]] = Suitability_Arable_rs_l[["RCP4_5"]][["X2030"]]
    Suitability_RCP45_rs_l_final[["2040"]][[1]] = Suitability_Arable_rs_l[["RCP4_5"]][["X2040"]]
    Suitability_RCP45_rs_l_final[["2050"]][[1]] = Suitability_Arable_rs_l[["RCP4_5"]][["X2050"]]
    Suitability_RCP45_rs_l_final[["2060"]][[1]] = Suitability_Arable_rs_l[["RCP4_5"]][["X2060"]]
    Suitability_RCP45_rs_l_final[["2070"]][[1]] = Suitability_Arable_rs_l[["RCP4_5"]][["X2070"]]
    
    
    # RCP85
    Suitability_RCP85_rs_l_final = Suitability_RCP85_rs_l[-c(2:3)]
    names(Suitability_RCP85_rs_l_final)[1] = "2020" # 2000 is our new 2020 
    
    Suitability_RCP85_rs_l_final[["2030"]][[1]] = Suitability_Arable_rs_l[["RCP8_5"]][["X2030"]]
    Suitability_RCP85_rs_l_final[["2040"]][[1]] = Suitability_Arable_rs_l[["RCP8_5"]][["X2040"]]
    Suitability_RCP85_rs_l_final[["2050"]][[1]] = Suitability_Arable_rs_l[["RCP8_5"]][["X2050"]]
    Suitability_RCP85_rs_l_final[["2060"]][[1]] = Suitability_Arable_rs_l[["RCP8_5"]][["X2060"]]
    Suitability_RCP85_rs_l_final[["2070"]][[1]] = Suitability_Arable_rs_l[["RCP8_5"]][["X2070"]]

    
    suitability_names = paste0(c("Arable", "Wetland", "ImprovedGrassland", "SemiNaturalGrassland"), "_Suitability")
    
    
    
    suitability_scenario_names = c("Baseline", "RCP4_5", "RCP8_5")
    suitability_scenario_rs_l = list(list(Suitability_Baseline_rs), Suitability_RCP45_rs_l_final, Suitability_RCP85_rs_l_final)
    
    scen_idx = 1 
    year_idx = 1 
    
    
    for (scen_idx in seq_along(suitability_scenario_names)) { 
        
        
        scenario_name_tmp = suitability_scenario_names[scen_idx]
        suitability_scenario_tmp = suitability_scenario_rs_l[[scen_idx]]
        nyear = length(suitability_scenario_tmp)
        
        years_tmp = suitability_years[-c(1:2)][1:nyear]
        
        for (year_idx in seq_along(years_tmp)) { 
            
            year_tmp = years_tmp[year_idx]
            outname_tmp = ifelse(scenario_name_tmp=="Baseline", yes = scenario_name_tmp, no =  paste0(scenario_name_tmp, "_", year_tmp))
            print(outname_tmp)
            suitability_extracted_m = foreach (s_idx = 1:length(suitability_names), .combine = "cbind") %do% { 
                
                suitability_name = suitability_names[s_idx] 
                
                print(suitability_name)
                
                BNG_r_tmp = projectRaster(suitability_scenario_tmp[[year_idx]][[s_idx]], CHESS_BNG_r, fun ="last", background=0)
                
                
                BNG_r_tmp2 = fillShetland(BNG_r_tmp)
                
                
                # plot(BNG_r_tmp2, add=F)
                # plot(CHESS_mask_r, col="red", add=T) 
                
                BNG_r_tmp3 = fillCoastalPixels(BNG_r_tmp2, boundary_r = CHESS_mask_r, width = 3, n_interpol = 5)
                
                # are there coastal pixels left empy? 
                # plot(CHESS_mask_r, col="red")
                # plot(BNG_r_tmp3, add=T)
                # plot(CHESS_mask_r - !is.na(BNG_r_tmp3), col=c("grey", "red"))
                
                
                if (FALSE) { 
                    par(mfrow=c(1,1))    
                    plot(Orkney_NUTS_BNG_r, add=F, col="white")
                    plot(BNG_r_tmp3, add=T)
                    plot(CHESS_BNG_r, add=T)
                    plot(BNG_r_tmp3, add=T)
                    
                    plot(BNG_r_tmp3 - BNG_r_tmp2, add=T)
                    
                    plot(Orkney_NUTS_BNG_shp, add=T)
                    
                    plot(Shetland_NUTS_BNG_r, add=F)
                    plot(BNG_r_tmp3, add=T)
                    # plot(CHESS_mask_r, add=T)
                }
                
                
                # plot(CHESS_BNG_sp, add=T)
                writeRaster(BNG_r_tmp3, filename =  paste0(path_output, "/Capital/Suitability/CRAFTY_UK_",outname_tmp, "_", suitability_name, ".tif"), overwrite=T)
                
                extract(BNG_r_tmp3, CHESS_BNG_sp, fun = mean)
            }
            
            csv_df = data.frame(FID = CHESS_BNG_csv$FID, long = CHESS_LL_coords$Longitude + 180, lat = CHESS_LL_coords$Latitude, X_BNG = CHESS_BNG_csv$POINT_X, Y_BNG = CHESS_BNG_csv$POINT_Y)
            
            suitability_extracted_m[is.na(suitability_extracted_m)] = 0 
            
            csv_df_out = cbind(csv_df, suitability_extracted_m)
            colnames(csv_df_out)[6:9] = suitability_names
            
            write.csv(csv_df_out, file =paste0(path_output, "/Capital/Suitability/CRAFTY_UK_Suitability_", outname_tmp, ".csv"), quote = F, row.names = F)
        }
    }
    
}


do4Capitals = FALSE 


if (do4Capitals) { 
    
    ############# CAPITAL MAPS
    ### Financial capital (NUTS)
    fc_csv = read.csv(paste0(path_data, "Capital/Capitals/Final version_22Mar2021/F Projections.csv"))
    (colnames(fc_csv))
    
    ### Manufactured capital (LAD)
    mc_csv = read.csv(paste0(path_data, "Capital/Capitals/Final version_22Mar2021/M Projections.csv"))
    (colnames(mc_csv))
    
    ### Human capital (LAD)
    hc_csv = read.csv(paste0(path_data, "Capital/Capitals/Final version_22Mar2021/H Projections.csv"))
    head(colnames(hc_csv))
    
    ### Social capital (NUTS)
    sc_csv = read.csv(paste0(path_data, "Capital/Capitals/Final version_22Mar2021/S Projections.csv"))
    head(colnames(sc_csv))
    
    
    str(colnames(fc_csv))
    colnames(mc_csv)
    colnames(hc_csv)
    colnames(sc_csv)
    
    
    fc_idx = match(NUTS_shp$nuts318cd, fc_csv$nuts318cd)
    sc_idx = match(NUTS_shp$nuts318cd, sc_csv$nuts318cd)
    
    FC_shp = SC_shp = NUTS_shp
    
    FC_shp@data = cbind(  fc_csv[fc_idx, (ncol(fc_csv) -45 + 1):ncol(fc_csv) ])
    SC_shp@data = cbind(  sc_csv[sc_idx, (ncol(sc_csv) -45 + 1):ncol(sc_csv) ])
    
    HC_shp = MC_shp = LAD_shp
    hc_idx = match(LAD_shp$LAD19CD, hc_csv$LAD19CD)
    HC_shp@data = cbind(  hc_csv[hc_idx, (ncol(hc_csv) -45 + 1):ncol(hc_csv) ])
    
    mc_idx = match(LAD_shp$LAD19CD, mc_csv$LAD19CD)
    
    MC_shp@data = cbind(  mc_csv[mc_idx, (ncol(mc_csv) -45 + 1):ncol(mc_csv) ])
    
    
    # spplot(FC_shp, "St.HI_2050_SSP4")
    # spplot(HC_shp, "LE_2050_SSP1")
    
    # library(sf)
    # library(randomcoloR)
    # my.dat_sf <- st_as_sf(MC_shp)
    # plot(my.dat_sf[,1]) # , max.plot=10, breaks=c(seq(from = 100, to = 5000, by = 500),5000),
    # pal = distinctColorPalette(length(seq(from = 100, to = 5000, by = 500))),
    # border=NA, key.pos=4)
    
    
    # beginCluster()
    library(doSNOW)
    cl = makeCluster(23)
    registerDoSNOW(cl)
    
    
    four_capital_names = paste0(c("Social", "Financial", "Human", "Manufactured"), "Capital")
    shp_names = c("SC_shp", "FC_shp", "HC_shp", "MC_shp")
    c_idx = layer_idx = 1
    
    for (c_idx in 1:length(four_capital_names)) { 
        
        capital_name = four_capital_names[c_idx] 
        shp_name = shp_names[c_idx]
        print(capital_name)
        
        BNG_shp = spTransform(get(shp_name), CRSobj = proj4.BNG)
        plot(st_as_sf(BNG_shp)[,1])
        
        layer_names = names(BNG_shp)
        
        foreach (layer_idx = 1:length(layer_names), .packages = c("raster")) %dopar% {
            # foreach (layer_idx = 1, .packages = c("raster")) %dopar% {
            
            layer_name = layer_names[layer_idx]
            print(layer_name)
            
            BNG_r_tmp = rasterize(BNG_shp[,], CHESS_BNG_r, field = layer_name, fun ="last", background=0)
            plot(BNG_r_tmp, add=F)
            
            plot(UK_BNG_r, add=F)
            plot(BNG_r_tmp * CHESS_mask_r, add=T)
            # 
            BNG_r_tmp2 = BNG_r_tmp * CHESS_mask_r 
            # BNG_r_tmp2 = fillShetland(BNG_r_tmp)
            
            if (!(capital_name %in% c("Manufactured", "Human"))) { 
                
                BNG_r_tmp3 = fillCoastalPixels(BNG_r_tmp2,  boundary_r = CHESS_mask_r, maskchar=0, width = 3, n_interpol = 5) 
            } else { 
                BNG_r_tmp3 = BNG_r_tmp2
            }
            
            plot(BNG_r_tmp3)
            plot(BNG_r_tmp3 - BNG_r_tmp2)
            # 
            # plot(UK_BNG_r, add=F)
            # plot(BNG_r_tmp3 * CHESS_mask_r, add=F)
            # 
            # plot(CHESS_mask_r - !is.na(BNG_r_tmp), col=c("grey", "red"))
            # plot(CHESS_mask_r - !is.na(BNG_r_tmp3), col=c("grey", "red"))
            
            
            
            BNG_extracted_tmp = extract(BNG_r_tmp3, CHESS_BNG_sp, fun = mean)
            
            csv_df = data.frame(FID = CHESS_BNG_csv$FID, long = CHESS_LL_coords$Longitude + 180, lat = CHESS_LL_coords$Latitude, X_BNG = CHESS_BNG_csv$POINT_X, Y_BNG = CHESS_BNG_csv$POINT_Y)
            
            csv_df$capital = BNG_extracted_tmp
            colnames(csv_df)[ncol(csv_df)] = capital_name
            
            writeRaster(BNG_r_tmp3, filename =  paste0(path_output, "/Capital/Capitals/",capital_name, "/CRAFTY_UK_", capital_name, "_", layer_name, ".tif"), overwrite=T)
            write.csv(csv_df, file =paste0(path_output, "/Capital/Capitals/",capital_name, "/CRAFTY_UK_", capital_name, "_", layer_name,  ".csv"), quote = F, row.names = F)
        }
    }
    
    stopCluster(cl)
}


doWoodlandCapitals = FALSE 


if (doWoodlandCapitals) { 
    
    
    woodland_aft_tb = read.csv(paste0(path_data, "Capital/Woodland capital/2nd version Mar 2021/ESC_species_to_AFT.csv"))
    
    
    woodland_years = seq(2020, 2070, 10)
    woodland_scenario_years = paste0(woodland_years-10, "_", woodland_years+10)
    
    
    # baseline
    Woodland_baseline_rs = stack(paste0(path_data, "Capital/Woodland capital/2nd version Mar 2021/", "baseline_1991-2011", "/", c("BE", "SBI", "SOK", "SP", "SS", "SY", "WWL"), "_soil_yc_baseline_mdAdj.tif")) # Dec 2020
    proj4string(Woodland_baseline_rs) = proj4.BNG
    
    # RCP45 
    Woodland_RCP45_rs_l = lapply(woodland_scenario_years, FUN = function(year) {
        x = stack(paste0(path_data, "Capital/Woodland capital/2nd version Mar 2021/", "RCP45", "/", c("BE", "SBI", "SOK", "SP", "SS", "SY", "WWL"), "_soil_yc_", year, "_mdAdj.tif"));
        proj4string(x) = proj4.BNG; 
        return(x)}) # Dec 2020
    
    Woodland_RCP85_rs_l = lapply(woodland_scenario_years, FUN = function(year) {
        x = stack(paste0(path_data, "Capital/Woodland capital/2nd version Mar 2021/", "RCP85", "/", c("BE", "SBI", "SOK", "SP", "SS", "SY", "WWL"), "_soil_yc_", year, "_mdAdj.tif"));
                  proj4string(x) = proj4.BNG; 
                  return(x)}) # Dec 2020
    
       
    woodland_rs_l = list(list(Woodland_baseline_rs), Woodland_RCP45_rs_l, Woodland_RCP85_rs_l)
     
    woodland_scenario_names = c("Baseline", "RCP4_5", "RCP8_5")
    

    
    scen_idx = 1 
    year_idx = 1 
    

    
    for (scen_idx in seq_along(woodland_scenario_names)) { 
         
        print(scen_idx)
        woodland_scenario_name_tmp = woodland_scenario_names[scen_idx]
        woodland_rs_l_tmp = woodland_rs_l[[scen_idx]]
         
        nyear = length(woodland_rs_l_tmp)
        
        
        for (year_idx in 1:nyear) { 
            
            year_tmp = woodland_years[year_idx]
            
            woodland_rs_tmp = woodland_rs_l_tmp[[year_idx]]
            
            names(woodland_rs_tmp) = woodland_aft_tb$Species.code[-8]
            
            woodland_rs_tmp$Mixed = mean(woodland_rs_tmp[[c("SS", "SP", "BE", "SOK", "SBI")]])
            
            
            
            # sum of SBI and SOK
            woodland_rs_tmp$SOK = woodland_rs_tmp$SBI + woodland_rs_tmp$SOK
            
            woodland_rs_tmp = woodland_rs_tmp[[-2]]
            
            
             
             
            outname_tmp = ifelse(woodland_scenario_name_tmp=="Baseline", yes = woodland_scenario_name_tmp, no =  paste0(woodland_scenario_name_tmp, "_", year_tmp))
            print(outname_tmp)
            
              
            
            plot(woodland_rs_tmp)
            # plot(sum(Suitability_rs))
            
            
            endCluster()
            # beginCluster(12)
            # cl = makeCluster()
            # registerDoSNOW(cl)
        
            w_idx = 1
            
            woodland_extracted_m = foreach (w_idx = seq_along(woodland_names), .combine = "cbind") %do% { 
                
                woodland_name = woodland_names[w_idx] 
                
                print(woodland_name)
                
                BNG_r_tmp = projectRaster(woodland_rs_tmp[[w_idx]], CHESS_BNG_r, field = woodland_name, fun ="last", background=0)
                
                
                BNG_r_tmp2 = fillShetland(BNG_r_tmp)
                
                
                # plot(BNG_r_tmp2, add=F)
                # plot(CHESS_mask_r, col="red", add=T) 
                
                BNG_r_tmp3 = fillCoastalPixels(BNG_r_tmp2, boundary_r = CHESS_mask_r, width = 3, n_interpol = 5)
                
                # are there coastal pixels left empy? 
                # plot(CHESS_mask_r, col="red")
                # plot(BNG_r_tmp3, add=T)
                # plot(CHESS_mask_r - !is.na(BNG_r_tmp3), col=c("grey", "red"))
                
                
                if (FALSE) { 
                    par(mfrow=c(1,1))    
                    plot(Orkney_NUTS_BNG_r, add=F, col="white")
                    plot(BNG_r_tmp3, add=T)
                    plot(CHESS_BNG_r, add=T)
                    plot(BNG_r_tmp3, add=T)
                    
                    plot(BNG_r_tmp3 - BNG_r_tmp2, add=T)
                    
                    plot(Orkney_NUTS_BNG_shp, add=T)
                    
                    plot(Shetland_NUTS_BNG_r, add=F)
                    plot(BNG_r_tmp3, add=T)
                    # plot(CHESS_mask_r, add=T)
                }
                
                
                # plot(CHESS_BNG_sp, add=T)
                writeRaster(BNG_r_tmp3, filename =  paste0(path_output, "/Capital/Woodland capital/CRAFTY_UK_",woodland_name, "_",  outname_tmp, ".tif"), overwrite=T)
                
                extract(BNG_r_tmp3, CHESS_BNG_sp, fun = mean)
            }
            
            csv_df = data.frame(FID = CHESS_BNG_csv$FID, long = CHESS_LL_coords$Longitude + 180, lat = CHESS_LL_coords$Latitude, X_BNG = CHESS_BNG_csv$POINT_X, Y_BNG = CHESS_BNG_csv$POINT_Y)
            
            woodland_extracted_m[is.na(woodland_extracted_m)] = 0 
            
            csv_df_out = cbind(csv_df, woodland_extracted_m)
            colnames(csv_df_out)[6:12] = woodland_names
            
            write.csv(csv_df_out, file =paste0(path_output, "/Capital/Woodland capital/CRAFTY_UK_Woodland_", outname_tmp, ".csv"), quote = F, row.names = F)
            
        }
    }
}



doUrban = FALSE 

if (doUrban) { 
    # urban scenario (Corine)
    Urban2015_extracted = extract(LCM2015_dom_rs %in% c(20:21), CHESS_BNG_sp, method="simple", fun = count)
    
    Urban2015_csv_df = cbind(basealc_csv_df[, c("X", "Y")], FR_IMMUTABLE = Urban2015_extracted)
    str(Urban2015_csv_df)
    
    write.csv(Urban2015_csv_df, file = paste0(path_output, "/UrbanMask/UrbanMask2020.csv"), quote = F, row.names = F)
    
    
    ### Urbanisation scenarios
    
    
    
    print("do Urban")
    
    
    # beginCluster()
    library(doSNOW)
    cl = makeCluster(12)
    registerDoSNOW(cl)
    
    
    
    SSPs = paste0("SSP", 1:5)
    SSPyears = seq(2020, 2100, 10)
    Urbanisation_path = paste0(path_data, "/UrbanMask/UKSSP Urban Surface_v2/")
    # list.files(urbanisation_path, pattern="tif$")
    
    ssp_idx = year_idx =  1 
    ssp_idx = year_idx =  5
    
    for (ssp_idx in seq_along(SSPs)) { 
        
        SSP_name = SSPs[ssp_idx]
        print(SSP_name)
        
        foreach (year_idx = 1:length(SSPyears), .packages = c("raster")) %dopar% {
            # foreach (layer_idx = 1, .packages = c("raster")) %dopar% {
            
            SSP_year = SSPyears[year_idx]
            print(SSP_year)
            
            BNG_r_tmp = raster(paste0(Urbanisation_path, SSP_name, ".", SSP_year, ".tif"))
            plot(BNG_r_tmp, add=F)
            plot(UK_BNG_r, add=F)
            plot(BNG_r_tmp * CHESS_mask_r, add=T)
            # do not need to fill shetland and coastal pixels 
            BNG_r_tmp3 = BNG_r_tmp * CHESS_mask_r 
            
            
            BNG_extracted_tmp = extract(BNG_r_tmp3, CHESS_BNG_sp, fun = mean)
            
            BNG_extracted_tmp = ifelse(BNG_extracted_tmp==1, yes = 1, no = 0)
            
            csv_df = data.frame( X = cellids$X_col, Y = cellids$Y_row)
            
            csv_df$FR_IMMUTABLE = BNG_extracted_tmp
            colnames(csv_df)[ncol(csv_df)] = "FR_IMMUTABLE"
            
            # writeRaster(BNG_r_tmp3, filename =  paste0(path_output, "/",capital_name, "/CRAFTY_UK_", capital_name, "_", layer_name, ".tif"), overwrite=T)
            write.csv(csv_df, file =paste0(path_output, "/UrbanMask/",SSP_name,"/UrbanMask_", SSP_name, "_", SSP_year,  ".csv"), quote = F, row.names = F)
        }
    }
    stopCluster(cl)
    
}





doFlood = FALSE

if (doFlood) { 
    ### Flood risk maps
    Flood_GB = raster("../CRAFTY New Social Network and Urban Modelling/IH130/UKDTM_$FD1_GB__FD1_0_0_0_700_1250.asc")
    Flood_NI = raster("../CRAFTY New Social Network and Urban Modelling/IH130/UKDTM_$FD2_NI__FD2_-1_0_0_500_500.asc")
    
    proj4string(Flood_NI) = proj4.OSNI1952
    proj4string(Flood_GB) = proj4.BNG
    
    Flood_GB_bng = projectRaster(Flood_GB, to = LCM2015_dom_rs[[1]], method = "ngb")
    Flood_NI_bng = projectRaster(Flood_NI, to = LCM2015_dom_rs[[1]], method = "ngb")
    
    Flood_UK_bng = mosaic(Flood_GB_bng, Flood_NI_bng, fun = max)
    plot(Flood_UK_bng)
    
    Flood_UK_bng = mask(Flood_UK_bng, LCM2015_dom_rs[[1]])
    plot(Flood_UK_bng)
    
    writeRaster(Flood_UK_bng, filename = paste0(path_output, "/FloodRisk_IH130_UK_1km.tif"), overwrite=T)
}



doRAMSAR = FALSE  

if (doRAMSAR) { 
    
    ########### RAMSAR
    
    RAMSAR_shp = readOGR("Land Cover data/Protected Areas/Ramsar sites/UK_RAMSAR_BNG_20190712.shp")
    
    
    # Take Presence/Absence of SPA2019 data at the 1 km grid 
    RAMSAR_shp$PA = 1
    RAMSAR_r = rasterize(RAMSAR_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    # RAMSAR_r = mask(RAMSAR_r, UK_BNG_r)
    plot(RAMSAR_r, add=F)
    plot(is.na(RAMSAR_r), add=F)
    
    # plot(RAMSAR_shp, add=F)
    # plot(UK_BNG_r, add=T)
    # plot(RAMSAR_r, add=T)
    # plot(RAMSAR_shp, add=T)
    
    
    writeRaster(RAMSAR_r, filename = paste0(path_output, "/RAMSAR_UK_PA.tif"), overwrite=T)
    
} else { 
    
    RAMSAR_r = raster(paste0(path_output, "/RAMSAR_UK_PA.tif"))
}


doSAC = FALSE  

if (doSAC) { 
    
    ########### SAC
    
    SAC_england_shp = readOGR("Land Cover data/Protected Areas/SAC areas/GB_SAC_OSGB36_20191031.shp")
    SAC_nireland_shp = readOGR("Land Cover data/Protected Areas/SAC areas/NI_SAC_TM65_20191031.shp")
    
    
    # Take Presence/Absence at the 1 km grid 
    SAC_england_shp$PA = SAC_nireland_shp$PA = 1
    SAC_england_r = rasterize(SAC_england_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    SAC_nireland_shp = rasterize(SAC_nireland_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    
    # SAC_r = mask(SAC_r, UK_BNG_r)
    plot(SAC_england_r, add=F)
    plot(is.na(SAC_nireland_shp), add=F)
    
    # plot(SAC_england_shp, add=F)
    # plot(UK_BNG_r, add=T)
    plot(SAC_england_r, add=F)
    plot(SAC_nireland_shp, add=F)
    
    SAC_r = (SAC_england_r + SAC_nireland_shp) > 0 
    plot(SAC_r, add=F)
    
    
    writeRaster(SAC_r, filename = paste0(path_output, "/SAC_UK_PA.tif"), overwrite=T)
}else { 
    
    SAC_r = raster(paste0(path_output, "/SAC_UK_PA.tif"))
}



doNNR = FALSE  

if (doNNR) { 
    #### NNR
    
    NNR_england_shp = readOGR("Land Cover data/Protected Areas/National Nature Reserves/England/National_Nature_Reserves___Natural_England.shp")
    NNR_scotland_shp = readOGR("Land Cover data/Protected Areas/National Nature Reserves/Scotland/NNR_SCOTLAND.shp")
    NNR_wales_shp = readOGR("Land Cover data/Protected Areas/National Nature Reserves/Wales/NRW_NNRPolygon.shp")
    NNR_nireland_shp = readOGR("Land Cover data/Protected Areas/National Nature Reserves/Northern Ireland/NNR_and_NR.shp")
    
    NNR_england_shp = spTransform(NNR_england_shp, CRSobj = crs(proj4.BNG))
    NNR_scotland_shp = spTransform(NNR_scotland_shp, CRSobj = crs(proj4.BNG))
    NNR_wales_shp = spTransform(NNR_wales_shp, CRSobj = crs(proj4.BNG))
    NNR_nireland_shp = spTransform(NNR_nireland_shp, CRSobj = crs(proj4.BNG))
    
    
    
    NNR_england_shp$PA = NNR_scotland_shp$PA = NNR_wales_shp$PA  = NNR_nireland_shp$PA = 1
    
    NNR_england_r = rasterize(NNR_england_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    NNR_scotland_r = rasterize(NNR_scotland_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    NNR_wales_r = rasterize(NNR_wales_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    NNR_nireland_r = rasterize(NNR_nireland_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    
    NNR_r = (NNR_england_r + NNR_scotland_r + NNR_wales_r + NNR_nireland_r) > 0 
    
    par(mfrow=c(2,2))
    plot(NNR_england_r)
    plot(NNR_scotland_r)
    plot(NNR_wales_r)
    plot(NNR_nireland_r)
    
    plot(NNR_r)
    writeRaster(NNR_r, filename = paste0(path_output, "/NNR_UK_PA.tif"), overwrite=T)
    
    
    ### LNR 
    
    
    LNR_england_shp = readOGR("Land Cover data/Protected Areas/LNRs/Local_Nature_Reserves__England_-shp/Local_Nature_Reserves__England____Natural_England.shp")
    LNR_scotland_shp = readOGR("Land Cover data/Protected Areas/LNRs/LNR_SCOTLAND_ESRI/LNR_SCOTLAND.shp")
    LNR_wales_shp = readOGR("Land Cover data/Protected Areas/LNRs/LocalNatureReservesLNR/NRW_LNRPolygon.shp")
    
    LNR_england_shp = spTransform(LNR_england_shp, CRSobj = crs(proj4.BNG))
    LNR_scotland_shp = spTransform(LNR_scotland_shp, CRSobj = crs(proj4.BNG))
    LNR_wales_shp = spTransform(LNR_wales_shp, CRSobj = crs(proj4.BNG))
    
    
    
    LNR_england_shp$PA = LNR_scotland_shp$PA = LNR_wales_shp$PA  = 1
    
    LNR_england_r = rasterize(LNR_england_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    LNR_scotland_r = rasterize(LNR_scotland_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    LNR_wales_r = rasterize(LNR_wales_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    
    LNR_r = (LNR_england_r + LNR_scotland_r + LNR_wales_r ) > 0 
    
    par(mfrow=c(2,2))
    plot(LNR_england_r)
    plot(LNR_scotland_r)
    plot(LNR_wales_r)
    plot(LNR_r)
    
    
    writeRaster(LNR_r, filename = paste0(path_output, "/LNR_UK_PA.tif"), overwrite=T)
}else { 
    
    NNR_r = raster(paste0(path_output, "/NNR_UK_PA.tif"))
    LNR_r = raster(paste0(path_output, "/LNR_UK_PA.tif"))
    
}



doSSSI = FALSE  

if (doSSSI) { 
    ### SSSI 
    
    SSSI_england_shp = readOGR("Land Cover data/Protected Areas/SSSIs/Sites_of_Special_Scientific_Interest_Units__England_-shp/Sites_of_Special_Scientific_Interest_Condition_Units__SSSI__England___Natural_England.shp")
    SSSI_scotland_shp = readOGR("Land Cover data/Protected Areas/SSSIs/SSSI_SCOTLAND_ESRI/SSSI_SCOTLAND.shp")
    SSSI_wales_shp = readOGR("Land Cover data/Protected Areas/SSSIs/NRW_DS98766_SSSI_2020_06_19/SSSI_June2020.shp")
    SSSI_nireland_shp = readOGR("Land Cover data/Protected Areas/SSSIs/ASSI - Irish National Grid_5/ASSI.shp")
    
    SSSI_england_shp = spTransform(SSSI_england_shp, CRSobj = crs(proj4.BNG))
    SSSI_scotland_shp = spTransform(SSSI_scotland_shp, CRSobj = crs(proj4.BNG))
    SSSI_wales_shp = spTransform(SSSI_wales_shp, CRSobj = crs(proj4.BNG))
    SSSI_nireland_shp = spTransform(SSSI_nireland_shp, CRSobj = crs(proj4.BNG))
    
    
    SSSI_england_shp$PA = SSSI_scotland_shp$PA = SSSI_wales_shp$PA  = SSSI_nireland_shp$PA= 1
    
    
    
    SSSI_england_r <- coverage_fraction(UK_BNG_r, st_combine(st_as_sf(SSSI_england_shp)))[[1]]
    SSSI_england_r1 = SSSI_england_r>0.5
    
    SSSI_england_r = rasterize(SSSI_england_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    
    plot(SSSI_england_r)
    plot(SSSI_england_r1)
    
    SSSI_scotland_r = rasterize(SSSI_scotland_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    SSSI_wales_r = rasterize(SSSI_wales_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    SSSI_nireland_r = rasterize(SSSI_nireland_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    
    SSSI_r = (SSSI_england_r + SSSI_scotland_r + SSSI_wales_r + SSSI_nireland_r ) > 0 
    
    par(mfrow=c(2,2))
    plot(SSSI_england_r)
    plot(SSSI_scotland_r)
    plot(SSSI_wales_r)
    plot(SSSI_nireland_r)
    plot(SSSI_r)
    
    
    writeRaster(SSSI_r, filename = paste0(path_output, "/SSSI_UK_PA.tif"), overwrite=T)
} else { 
    
    SSSI_r = raster(paste0(path_output, "/SSSI_UK_PA.tif"))
    
}


doHC = FALSE  

if (doHC) { 
    ### Heritage Coasts
    
    HC_england_shp = readOGR("Land Cover data/Protected Areas/Heritage Coasts/Heritage_Coasts_(England)/Heritage_Coast___England___Natural_England.shp")
    HC_wales_shp = readOGR("Land Cover data/Protected Areas/Heritage Coasts/HeritageCoasts/NRW_HERITAGE_COASTPolygon.shp")
    
    HC_england_shp = spTransform(HC_england_shp, CRSobj = crs(proj4.BNG))
    HC_wales_shp = spTransform(HC_wales_shp, CRSobj = crs(proj4.BNG))
    
    HC_england_shp$PA = HC_wales_shp$PA= 1
    
    HC_england_r = rasterize(HC_england_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    HC_wales_r = rasterize(HC_wales_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    
    HC_r = (HC_england_r   + HC_wales_r  ) > 0 
    
    par(mfrow=c(2,2))
    plot(HC_england_r)
    plot(HC_wales_r)
    plot(HC_r)
    
    
    writeRaster(HC_r, filename = paste0(path_output, "/HC_UK_PA.tif"), overwrite=T)
} else { 
    
    HC_r = raster(paste0(path_output, "/HC_UK_PA.tif"))
    
}


doBR = FALSE  

if (doBR) { 
    ### Biosphere Reserves (point data)
    
    BR_Global_shp = readOGR("Land Cover data/Protected Areas/Biosphere reserves/mab_biosphere_reserves.shp")
    
    BR_UK_LL_shp = crop(BR_Global_shp, UK_LL_r)
    
    plot(UK_LL_r)
    plot(BR_UK_LL_shp, add=T)
    
    BR_UK_shp = spTransform(BR_UK_LL_shp, CRSobj = crs(proj4.BNG))
    
    BR_UK_shp$PA= 1
    
    BR_r = rasterize(BR_UK_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    
    par(mfrow=c(1,1))
    plot(UK_BNG_r)
    plot(BR_UK_shp, add=T)
    
    
    writeRaster(BR_r, filename = paste0(path_output, "/BR_UK_PA.tif"), overwrite=T)
    
} else { 
    
    BR_r = raster(paste0(path_output, "/BR_UK_PA.tif"))
    
}



### NGO 

doNGO = FALSE 

if (doNGO) {
    
    # RSPB
    RSPB_GB_shp = readOGR("Land Cover data/Protected Areas/NGOs/RSPB/RSPB_Reserves-shp-UK/RSPB_Reserve_Boundaries_20201027.shp")
    
    RSPB_GB_shp = spTransform(RSPB_GB_shp, CRSobj = crs(proj4.BNG))
    
    RSPB_GB_shp$PA= 1
    
    RSPB_r = rasterize(RSPB_GB_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    
    
    par(mfrow=c(1,2))
    plot(UK_BNG_r)
    plot(RSPB_GB_shp, add=T)
    plot(RSPB_r)
    writeRaster(RSPB_r, filename = paste0(path_output, "/RSPB_UK_PA.tif"), overwrite=T)
    
    # JMT (John_Muir_Trust_Boundaries14)
    JMT_scotland_shp = readOGR("Land Cover data/Protected Areas/NGOs/John_Muir_Trust_Boundaries14/John_Muir_Trust_Boundaries14.shp")
    JMT_scotland_shp = spTransform(JMT_scotland_shp, CRSobj = crs(proj4.BNG))
    
    JMT_scotland_shp$PA= 1
    
    JMT_r = rasterize(JMT_scotland_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    writeRaster(JMT_r, filename = paste0(path_output, "/JMT_UK_PA.tif"), overwrite=T)
    
    par(mfrow=c(1,2))
    plot(UK_BNG_r)
    plot(JMT_scotland_shp, add=T)
    plot(JMT_r)
    
    # ScottishWildlifeTrust_reserves_20180522 (2020 data is rather reduced, less polygons)
    SWT_scotland_shp = readOGR("Land Cover data/Protected Areas/NGOs/SWT_reserves/ScottishWildlifeTrust_reserves_20180522/ScottishWildlifeTrust_reserves.shp")
    SWT_scotland_shp = spTransform(SWT_scotland_shp, CRSobj = crs(proj4.BNG))
    
    SWT_scotland_shp$PA= 1
    
    SWT_r = rasterize(SWT_scotland_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    writeRaster(SWT_r, filename = paste0(path_output, "/SWT_UK_PA.tif"), overwrite=T)
    
    par(mfrow=c(1,2))
    plot(UK_BNG_r)
    plot(SWT_scotland_shp, add=T)
    plot(SWT_r)
    
    # National Trust (NTS) England?
    NTS_always_shp = readOGR("Land Cover data/Protected Areas/NGOs/National_Trust/National_Trust_Open_Data3A_Land__Always_Open/National_Trust_Open_Data_Land__Always_Open.shp")
    NTS_limited_shp = readOGR("Land Cover data/Protected Areas/NGOs/National_Trust/National_Trust_Open_Data3A_Land__Limited_Access/National_Trust_Open_Data_Land__Limited_Access.shp")
    
    NTS_always_shp = spTransform(NTS_always_shp, CRSobj = crs(proj4.BNG))
    NTS_limited_shp = spTransform(NTS_limited_shp, CRSobj = crs(proj4.BNG))
    
    NTS_always_shp$PA = NTS_limited_shp$PA= 1
    
    NTS_always_r = rasterize(NTS_always_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    NTS_limited_r = rasterize(NTS_limited_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    
    NTS_r = (NTS_always_r   + NTS_limited_r  ) > 0 
    writeRaster(NTS_r, filename = paste0(path_output, "/NTS_UK_PA.tif"), overwrite=T)
    
    par(mfrow=c(1,2))
    plot(UK_BNG_r)
    plot(NTS_always_shp, add=T)
    plot(NTS_limited_shp, add=T)
    
    plot(NTS_r)
    
    # Woodland Trusties
    WT_GB_shp = readOGR("Land Cover data/Protected Areas/NGOs/woodlandtrustsites_SHP/woodlandtrustsites.shp")
    WT_GB_shp = spTransform(WT_GB_shp, CRSobj = crs(proj4.BNG))
    
    WT_GB_shp$PA= 1
    
    WT_r = rasterize(WT_GB_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    writeRaster(WT_r, filename = paste0(path_output, "/WT_UK_PA.tif"), overwrite=T)
    
    par(mfrow=c(1,2))
    plot(UK_BNG_r)
    plot(WT_GB_shp, add=T)
    plot(WT_r)
    
    # Trees for Life (TFL)
    TFL_dnd_shp = readOGR("Land Cover data/Protected Areas/NGOs/TFL/DundregganBoundary.shp")
    TFL_ga_shp = readOGR("Land Cover data/Protected Areas/NGOs/TFL/Glen Affric fencelines.shp") # polyline
    
    proj4string(TFL_ga_shp) = proj4string(TFL_dnd_shp)
    
    TFL_dnd_shp = spTransform(TFL_dnd_shp, CRSobj = crs(proj4.BNG))
    TFL_ga_shp = spTransform(TFL_ga_shp, CRSobj = crs(proj4.BNG))
    
    
    TFL_ga_sf <- st_as_sf(TFL_ga_shp) 
    TFL_ga_sf_polygons <- st_polygonize(TFL_ga_sf)
    TFL_ga_shp_polygons <- as(TFL_ga_sf_polygons, "Spatial") # If you want sp
    class(TFL_ga_shp_polygons)
    # plot(TFL_ga_shp_polygons, col = "red")
    
    
    TFL_dnd_shp$PA = TFL_ga_shp$PA= 1
    
    TFL_dnd_r = rasterize(TFL_dnd_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    TFL_ga_r = rasterize(TFL_ga_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    
    TFL_r = (TFL_dnd_r   + TFL_ga_r  ) > 0 
    
    par(mfrow=c(1,2))
    plot(UK_BNG_r)
    plot(TFL_dnd_shp, add=T)
    plot(TFL_ga_shp, add=T)
    plot(TFL_r)
    
    writeRaster(TFL_r, filename = paste0(path_output, "/TFL_UK_PA.tif"), overwrite=T)
    
    
    ### NGO 
    NGO_r = (RSPB_r + JMT_r + SWT_r + NTS_r + WT_r + TFL_r) > 0 
    plot(NGO_r)    
    writeRaster(NGO_r, filename = paste0(path_output, "/NGO_UK_PA.tif"), overwrite=T)
    
} else { 
    NGO_r = raster(paste0(path_output, "/NGO_UK_PA.tif"))
    
}




doAONB = FALSE 

if (doAONB) {
    
    #### AONB
    
    AONB_england_shp = readOGR("Land Cover data/Protected Areas/AreaOfOutstandingNaturalBeauty/AONB - England/Areas_of_Outstanding_Natural_Beauty__England____Natural_England.shp")
    NSA_scotland_shp = readOGR("Land Cover data/Protected Areas/AreaOfOutstandingNaturalBeauty/NationalScenicAreas_1998 - Scotland/SG_NationalScenicAreas_1998.shp")
    AONB_wales_shp = readOGR("Land Cover data/Protected Areas/AreaOfOutstandingNaturalBeauty/AONB - Wales/NRW_AONBPolygon.shp")
    AONB_nireland_shp = readOGR("Land Cover data/Protected Areas/AreaOfOutstandingNaturalBeauty/AONB - Irish National Grid/AONB.shp")
    
    AONB_england_shp = spTransform(AONB_england_shp, CRSobj = crs(proj4.BNG))
    NSA_scotland_shp = spTransform(NSA_scotland_shp, CRSobj = crs(proj4.BNG))
    AONB_wales_shp = spTransform(AONB_wales_shp, CRSobj = crs(proj4.BNG))
    AONB_nireland_shp = spTransform(AONB_nireland_shp, CRSobj = crs(proj4.BNG))
    
    AONB_england_shp$PA = NSA_scotland_shp$PA = AONB_wales_shp$PA = AONB_nireland_shp$PA = 1
    
    AONB_england_r = rasterize(AONB_england_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    NSA_scotland_r = rasterize(NSA_scotland_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    AONB_wales_r = rasterize(AONB_wales_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    AONB_nireland_r = rasterize(AONB_nireland_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    
    AONB_r = (AONB_england_r + NSA_scotland_r + AONB_wales_r + AONB_nireland_r) > 0 
    
    
    
    par(mfrow=c(2,2))
    plot(AONB_england_r)
    plot(NSA_scotland_r)
    plot(AONB_wales_r)
    plot(AONB_nireland_r)
    plot(AONB_r)
    
    
    
    
    writeRaster(AONB_r, filename = paste0(path_output, "/AONB_UK_PA.tif"), overwrite=T)
    
} else { 
    AONB_r = raster(paste0(path_output, "/AONB_UK_PA.tif"))
}


doSPA = FALSE 

if (doSPA) { 
    ############ SPA2019
    
    SPA2019_gb_shp = readOGR("Land Cover data/Protected Areas/SPA areas/GB_SPA_OSGB36_20190326.shp")
    SPA2019_ni_shp = readOGR("Land Cover data/Protected Areas/SPA areas/NI_SPA_TM65_20171114.shp")
    
    # Take Presence/Absence of SPA2019 data at the 1 km grid 
    SPA2019_gb_shp$PA = SPA2019_ni_shp$PA=  1
    SPA2019_gb_r = rasterize(SPA2019_gb_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    SPA2019_ni_r = rasterize(SPA2019_ni_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    
    # SPA2019_r = mask(SPA2019_r, UK_BNG_r)
    # plot(SPA2019_r, add=F)
    # plot(is.na(SPA2019_r), add=F)
    
    # plot(SPA2019_shp, add=F)
    # plot(UK_BNG_r, add=T)
    plot(SPA2019_gb_r, add=F)
    plot(SPA2019_ni_r, add=F)
    
    SPA2019_r = (SPA2019_gb_r + SPA2019_ni_r) > 0 
    
    
    SPA2019_extracted = extract(SPA2019_r, CHESS_BNG_sp, fun = count)
    
    
    SPA2019_csv_df = data.frame(FID = CHESS_BNG_csv$FID, long = CHESS_LL_coords$Longitude + 180, lat = CHESS_LL_coords$Latitude, X_BNG = CHESS_BNG_csv$POINT_X, Y_BNG = CHESS_BNG_csv$POINT_Y)
    
    SPA2019_csv_df$SPA = SPA2019_extracted
    writeRaster(SPA2019_r, filename = paste0(path_output, "/SPA2019_PA.tif"), overwrite=T)
    write.csv(SPA2019_csv_df, file = paste0(path_output, "/CRAFTY_UK_SPA2019_PA.csv"), quote = F, row.names = F)
} else { 
    SPA2019_r = raster(paste0(path_output, "/SPA2019_PA.tif"))
}



doNationalParks = FALSE 

if (doNationalParks) { 
    ### National Parks
    
    NationalParks_shp = readOGR("Land Cover data/Protected Areas/National Parks/National_Parks__December_2018__Boundaries_GB_BGC.shp")
    
    
    # Take Presence/Absence at the 1 km grid 
    NationalParks_shp$PA = 1
    NationalParks_r = rasterize(NationalParks_shp[,], UK_BNG_r, field = "PA", fun ="last", background=0)
    # NationalParks_r = mask(NationalParks_r, UK_BNG_r)
    plot(NationalParks_r, add=F)
    plot(is.na(NationalParks_r), add=F)
    
    plot(NationalParks_shp, add=F)
    # plot(UK_BNG_r, add=T)
    plot(NationalParks_r, add=T)
    plot(NationalParks_shp, add=T)
    
    writeRaster(NationalParks_r, filename = paste0(path_output, "/NationalParks_UK_PA.tif"), overwrite=T)
    
} else { 
    NationalParks_r = raster(paste0(path_output, "/NationalParks_UK_PA.tif"))
}


doProtectedAreas = FALSE
if (doProtectedAreas) { 
    ### Two-level system
    
    ProtectedAreas_L1_r = (NGO_r + AONB_r + HC_r + NationalParks_r) > 0 
    ProtectedAreas_L2_r = (NNR_r + LNR_r + SAC_r + SSSI_r + BR_r + SPA2019_r + RAMSAR_r ) > 0 
    
    par(mfrow=c(1,2))
    plot(ProtectedAreas_L1_r)
    plot(ProtectedAreas_L2_r)
    
    writeRaster(ProtectedAreas_L1_r, filename = paste0(path_output, "/ProtectedAreas_L1_UK_PA.tif"), overwrite=T)
    writeRaster(ProtectedAreas_L2_r, filename = paste0(path_output, "/ProtectedAreas_L2_UK_PA.tif"), overwrite=T)
    
    ProtectedAreas_L1_extracted = extract(ProtectedAreas_L1_r, CHESS_BNG_sp, fun = count)
    ProtectedAreas_L2_extracted = extract(ProtectedAreas_L2_r, CHESS_BNG_sp, fun = count)
    
    ProtectedAreas_csv_df = data.frame(FID = CHESS_BNG_csv$FID, long = CHESS_LL_coords$Longitude + 180, lat = CHESS_LL_coords$Latitude, X_BNG = CHESS_BNG_csv$POINT_X, Y_BNG = CHESS_BNG_csv$POINT_Y)
    
    ProtectedAreas_csv_df$Protected_L1 = ProtectedAreas_L1_extracted
    ProtectedAreas_csv_df$Protected_L2 = ProtectedAreas_L2_extracted
    
    write.csv(ProtectedAreas_csv_df, file = paste0(path_output, "/CRAFTY_UK_ProtectedAreas_PA.csv"), quote = F, row.names = F)
    write.csv(ProtectedAreas_csv_df[,-7], file = paste0(path_output, "/CRAFTY_UK_ProtectedAreas_L1.csv"), quote = F, row.names = F)
    write.csv(ProtectedAreas_csv_df[,-6], file = paste0(path_output, "/CRAFTY_UK_ProtectedAreas_L2.csv"), quote = F, row.names = F)
}





doNFI = FALSE 


if (doNFI) { 
    ########## NFI 2016
    
    
    # takes a lot of time 
    
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
    
    registerDoMC(16)
    
    
    # l_idx= 4
    # beginCluster()
    
    foreach (l_idx = (1:length(IOA_names))) %dopar% { 
        
        IOA_tmp = IOA_names[l_idx]
        print(IOA_tmp)
        NFI2016_shp_tmp = NFI2016_shp[NFI2016_shp$IFT_IOA == IOA_tmp,]
        NFI2016_shp_tmp
        
        NFI2016_shp_tmp$PA = 1 
        NFI2016_shp_tmp$IFT_IOA = NULL
        
        
        # plot(NFI2016_shp_bl_simple)beginCluster()
        # system.time({
        #     NFI2016_IOA_r = rasterize(x = NFI2016_shp_tmp[,], y= UK_BNG_r, field = "PA", background = 0, getCover=T, )
        # })
        
        system.time({
            NFI2016_IOA_r <- coverage_fraction(CHESS_BNG_r, st_combine(st_as_sf(NFI2016_shp_tmp)))[[1]]
        })
        
        
        # plot(NFI2016_IOA_r)
        
        writeRaster(NFI2016_IOA_r, filename = paste0(path_output, "/NFI2016_", IOA_tmp, ".tif"), overwrite=T)
        
    }
    
    file.rename( paste0(path_output, "/NFI2016_Cloud \\\ shadow.tif"),  paste0(path_output, "/NFI2016_Cloud shadow.tif"))
    IOA_names[5] = "Cloud shadow" 
    
    ## read cover fractions 
    
    rs = foreach (l_idx = (1:length(IOA_names))) %do% {
        
        IOA_tmp = IOA_names[l_idx]
        print(IOA_tmp)
        NFI2016_shp_tmp = NFI2016_shp[NFI2016_shp$IFT_IOA == IOA_tmp,]
        
        raster(paste0("Land Cover data/NFI 2016/Fraction/NFI2016_", IOA_tmp, ".tif"))
        
    }
    
    rs = stack(rs)
    
    plot(rs[[1]])
    plot(rs[[4]])
    
    beginCluster()
    nfi_sum = sum(rs)
    plot(nfi_sum)
    # plot(NFI2016_shp, add=T)
    
    NFI2016_Top1 = which.max(rs)
    
    plot(CHESS_BNG_r)
    plot(NFI2016_Top1, add=T)
    
    # plot(NFI2016_shp, add=T)
    
    writeRaster(NFI2016_Top1, filename = paste0(path_output, "/NFI2016_dominated_1km.tif"), overwrite=T)
    write.csv(data.frame(IFT_IOA=IOA_names), file = paste0(path_output, "/NFI2016_names.csv"), row.names = T, quote=F)
    
    
    NFI2016_Top1 = raster(paste0(path_output, "/NFI2016_1km_dominated.tif"))
    NFI2016_extracted = extract(NFI2016_Top1, CHESS_BNG_sp, method="simple", fun = modal)
    plot(nfi_sum)
    plot(NFI2016_Top1, add=T)
    
    
    NFI2016_csv_df = data.frame(FID = CHESS_BNG_csv$FID, long = CHESS_LL_coords$Longitude + 180, lat = CHESS_LL_coords$Latitude, X_BNG = CHESS_BNG_csv$POINT_X, Y_BNG = CHESS_BNG_csv$POINT_Y)
    
    NFI2016_csv_df$NFI = NFI2016_extracted
    
    write.csv(NFI2016_csv_df, file = paste0(path_output, "/CRAFTY_UK_NFI2016_NFI.csv"), quote = F, row.names = F)
    
    
}







doProtectedArea = FALSE 

if (doProtectedArea) { 
    
    
    
    
    
    
    
    
    ProtectedAreaL1_df = cbind(basealc_csv_df[, c("X", "Y")], PROTECTED_L1 = ProtectedAreas_csv_df$Protected_L1)
    ProtectedAreaL2_df = cbind(basealc_csv_df[, c("X", "Y")], PROTECTED_L2 = ProtectedAreas_csv_df$Protected_L2)
    
    write.csv(ProtectedAreaL1_df, file = paste0(path_output, "/CRAFTY_GB_ProtectedAreas_L1.csv"), quote = F, row.names = F)
    write.csv(ProtectedAreaL2_df, file = paste0(path_output, "/CRAFTY_GB_ProtectedAreas_L2.csv"), quote = F, row.names = F)
    
    
    
    # Mask_final = Urban2015_csv_df$Urban + ProtectedAreaL1_df$Protected.MaskL1 + ProtectedAreaL2_df$Protected.MaskL2
    # Mask_final[Mask_final>1] = 1 
    # 
    # FinalMask_csv_df = cbind(basealc_csv_df[, c("X", "Y")], Mask = Mask_final )
    # 
    # 
    # write.csv(FinalMask_csv_df, file = paste0(path_output, "/CRAFTY_GB_MaskAggregated.csv"), quote = F, row.names = F)
    
    
    ### FR role 
    frrole = data.frame(matrix(data = 0, nrow = nrow(aftnames)-1, ncol = nrow(aftnames)-1))
    rownames(frrole) = colnames(frrole) = aftnames$AFT[-16]
    frrole$UNMANAGED = 0 
    frrole["UNMANAGED",] = rep(0, ncol(frrole))
    
    # intensive types 
    intensive_types = c("IAfood", "IAfodder", "SusAr", "Bioenergy", "IP")
    non_intensive_types= setdiff(rownames(frrole), intensive_types)
    intensitve_forest_types = c("PNB", "PNC", "PNNB", "PNNC")
    
    # prohibit non-intensive to intensive changes 
    frrole[non_intensive_types, intensive_types]  = 1 
    frrole["UNMANAGED", ]  = 0
    
    frrole_write = cbind(rownames(frrole), frrole)
    colnames(frrole_write)[1] = "FR"
    write.table(frrole_write, file = paste0(path_out, "/csv/AllocationTypeRestrictions_UK.csv"), sep=",", row.names=F, quote=F)
    
    
} 

