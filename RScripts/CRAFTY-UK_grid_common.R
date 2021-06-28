library(exactextractr)
library(sf)
library(rgdal)
library(raster)
library(rgeos)
# library(doSNOW)

# library(fasterize)
# library(stars)

library(doMC)
library(randomForest)
library(gbm)


library(dplyr)



proj4.LL = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4.LL360 = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 +lon_wrap=180"
proj4.BNG = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.1502,0.247,0.8421,-20.4894 +units=m +no_defs"



### https://gis.stackexchange.com/questions/34276/whats-the-difference-between-epsg4326-and-epsg900913
proj4.SphericalMercator = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs +type=crs"  #EPSG:900913
# EPSG:3857
proj4.OSGB1936 ="+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"  # proj4string(LAD2019_shp) # EPSG:27700
proj4.OSNI1952 = "+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=1 +x_0=200000 +y_0=250000 +ellps=airy +towgs84=482.5,-130.6,564.6,-1.042,-0.214,-0.631,8.15 +units=m +no_defs" # EPSG:29901

 
path_data = "~/Nextcloud/workspace_newEU/CRAFTY UK input CSV files/"
 

### Read the NUTS data 
# LAD_shp = readOGR(paste0(path_data, "Boundaries/Local_Authority_Districts__April_2019__Boundaries_UK_BFE.shp"))
LAD_shp = readOGR(paste0(path_data, "Boundaries/Local_Authority_Districts__December_2019__Boundaries_UK_BFE.shp"))

NUTS_shp = readOGR(paste0(path_data, "Boundaries/NUTS_Level_3__January_2018__Boundaries.shp"))

 
# LAD_shp_osgb = spTransform(LAD_shp, proj4.OSGB1936)
# 
# sum(LAD_shp$Shape__Are)
# sum(LAD_shp_osgb$Shape__Are)
# sum(LAD2019_shp$Shape__Are)
# 
# rgeos::gArea(LAD_shp)
# rgeos::gArea(LAD_shp_osgb)
# 
# rgeos::gArea(LAD2019_shp)


# 
# lad_area = rgeos::gArea(LAD_shp_osgb, byid = T)
# lad2019_area = rgeos::gArea(LAD2019_shp, byid = T)
# 
# plot(lad_area, lad2019_area)
# cor(lad_area, lad2019_area)

## there are capital files beyond nuts boundary (e.g. Human Capital)
# NUTS_r = rasterize(spTransform(NUTS_shp[,], proj4.BNG), CHESS_BNG_r, field = "objectid", fun ="last", background=NA)
# NUTS_dummy_r = NUTS_r>0 


CHESS_BNG_csv = read.csv(paste0(path_data, "Basegrid/CHESS_1k_grid.csv")) # BNG perhaps
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


## 

# r_in = BNG_r_tmp2
# boundary_r = CHESS_mask_r
# width = 10 
# maskchar = 0 

smoothCapitals <- function(r_in, boundary_r, width_m=5000, maskchar=null) { 
    
    r_in = projectRaster(r_in, boundary_r)
    
    r_out = r_in
    
    if (!is.null(maskchar)) { 
        r_out[r_in==maskchar] = NA
    }
     
    
    win <- focalWeight(r_in, d = width_m, type = "circle") 
    
    mean_withNA = function(x) { 
        
        sum(x, na.rm=T) / length(x[!is.na(x)])  * length(x)
    }
    
    r_out <- focal(r_out, w = win, fun = mean_withNA, pad = T, na.rm=F)
    
    if (!is.null(maskchar)) { 
        r_out[r_in==maskchar] = NA
    }
    # plot(r_out)

    
    r_out[is.na(r_in)] = NA
    
    # plot(r_out * boundary_r)
    
    return(r_out * boundary_r)
    
}



## there are capital files beyond nuts boundary (e.g. Human Capital)
# NUTS_r = rasterize(spTransform(NUTS_shp[,], proj4.BNG), CHESS_BNG_r, field = "objectid", fun ="last", background=NA)
# NUTS_dummy_r = NUTS_r>0 



LCM2015_dom_rs = raster(paste0(path_output, "/LCM2015_1km_dominated.tif"))


UK_BNG_r = LCM2015_dom_rs[[1]]
UK_LL_r = projectRaster(UK_BNG_r, crs = proj4.LL, method = "ngb")

 


preprocessing = FALSE 

if (preprocessing) { 
    
    # x = CHESS_BNG_csv$POINT_X
    # y = CHESS_BNG_csv$POINT_Y
    # 
    
    
    
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
 
    
    write.csv(cellids, paste0(path_output, "/Basegrid/Cell_ID_XY_UK.csv"), quote = F, row.names = F)
    
    
    
    aftnames = data.frame(rbind(c("AF", "Agroforestry", "Extensive", "Agroforestry"),
                                c("Bioenergy", "Bioenergy", "Intensive", "Bioenergy"),
                                c("EA", "EA", "Extensive", "Extensive Agriculture"),
                                c("EP", "Ext_Pastoral", "Extensive", "Extensive Pastoral"),
                                c("IAfodder", "IAfodder", "Intensive", "Intensive Agriculture Fodder"),
                                c("IAfood", "IAfood", "Intensive", "Intensive Agriculture Food"),
                                c("IP", "Int_Pastoral", "Intensive", "Intensive Pastoral"),
                                c("MW", "Mixed woodland", "Extensive", "Mixed woodland"),
                                c("NWCons", "NWCons", "Extensive", "Natural woodland Conservation"),
                                c("PNB", "PNB", "Extensive",  "Productive N Broadleaf"),
                                c("PNC", "PNC", "Extensive", "Productive N Conifer"),
                                c("PNNB", "PNNB", "Extensive", "Productive NN Broadleaf"),
                                c("PNNC", "PNNC", "Extensive", "Productive NN Conifer"),
                                c("SusAr", "SusAr", "Extensive", "Sustainable Agriculture"),
                                c("VEP", "V_Ext_Pastoral", "Extensive", "Very Extensive Pastoral"),
                                c("Urban", "Urban", "Urban", "Urban"),
                                c("NOT_ASSIGNED", "Unmanaged","Extensive", "Unmanaged")
    ))
    
    colnames(aftnames) = c("AFT", "AFT_cb", "Type", "Description")
    
    
    write.csv(aftnames, file = paste0(path_output, "/AFT/AFT_Names_UK.csv"))
    
    
    capital_names = data.frame(Capital = c("Human", 
                                           "Social", 
                                           "Manufactured", 
                                           "Financial", 
                                           "Arable.suit", 
                                           # "Wetland.suit",
                                           "Igrass.suit", 
                                           "SNGrass.suit", 
                                           "Bioenergy.suit", 
                                           "AgroForestry.suit", 
                                           "NNConifer.suit", 
                                           "NConifer.suit", 
                                           "NNBroadleaf.suit", 
                                           "NBroadleaf.suit", 
                                           "Tree.suit"
    ))
    
    
    # capital_names_16 = c(
    #     "Human.capital",
    #     "Social.capital",
    #     "Manufactured.capital",
    #     "Financial.capital",
    #     "Arable_Suitability",
    #     "Wetland_Suitability",
    #     "ImprovedGrassland_Suitability",
    #     "SemiNaturalGrassland_Suitability",
    #     "bioenergy.YC",
    #     "agroforestry.YC",
    #     "non.native.conifer.YC",
    #     "native.conifer.YC",
    #     "non.native.broadleaf.YC",
    #     "native.broadleaf.YC",
    #     "native.woodland.YC",
    #     "mixed.YC"
    # )
    
    write.csv(capital_names, file = paste0(path_output, "/Capital/Capital_names_UK.csv"))
    
    
    
    service_names = c(
        "Food.crops",
        "Fodder.crops",
        "GF.redMeat",
        "Fuel",
        "Softwood",
        "Hardwood",
        "Biodiversity",
        "Carbon",
        "Recreation",
        "Flood.reg",
        "Employment",
        "Ldiversity",
        "GF.milk",
        "Sus.Prod")
    
    
    
    
    write.csv(service_names, file = paste0(path_output, "/Service/Service_names_UK.csv"), quote = F, row.names = F)
    
} else { 
    aftnames = read.csv(paste0(path_data, "/AFT/AFT_Names_UK.csv"))
    
    capital_names  = read.csv(paste0(path_data, "/Capital/Capital_names_UK.csv"))$Capital
    
    service_names  = as.vector(read.csv(paste0(path_data, "/Service/Service_names_UK.csv")))
    
    cellids = read.csv(paste0(path_data, "/Basegrid/Cell_ID_XY_UK.csv"))
}




woodland_names = c("NNBroadleaf.suit", # Beech (BE)
                   "NBroadleaf.suit",  # Sessile oak (SOK)
                   "NConifer.suit",    # Scots pine (SP)
                   "NNConifer.suit",   # Sitka spruce (SS)
                   "AgroForestry.suit", # Sycamore (SY)
                   "Bioenergy.suit",    # Willow (WWL)
                   "Tree.suit")         # Mixed (SS, SP, BE, SOK, SBI)







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
