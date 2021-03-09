
library(RNetCDF)
library(raster)

# setwd("~/CRAFTY/CRAFTY-UK/CRAFTY-UKr")
setwd("~/Downloads/CRAFTY_Scotland/")

# note tasmax units are kelvin - need to convert later

# read ncdf file
if (FALSE ) { 
    ukcp18_file <-  "data-processed/ukcp18-speed_rcp85_01_tasmax_uk_1km_20yr-mean-monthly_198012-200011.nc"
    
    nc.f <- RNetCDF::open.nc(ukcp18_file)
    RNetCDF::print.nc(nc.f)
    lon <- RNetCDF::var.get.nc(nc.f, "x") # easting british national grid
    lat <- RNetCDF::var.get.nc(nc.f, "y") # northing british national grid
    time <- RNetCDF::var.get.nc(nc.f, "time") # time in days since 1970
    
    fillValue <- RNetCDF::att.get.nc(nc.f, variable = "tasmax", "_FillValue")
    tmax_array <- RNetCDF::var.get.nc(nc.f, "tasmax") # store the data in a 3-dimensional array
    dim(tmax_array) 
    tmax_array[tmax_array == fillValue] <- NA
    
    
    
     
    tmax_array_reshaped <- aperm(tmax_array, perm = c(1,2,3)) # actually do not do anything here but flip the raster in the below 
    tmax_daily_rs <- flip(brick(tmax_array_reshaped[,,], xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs "), transpose=T), direction = 2)
    
    tmax_annual_r <- mean(tmax_daily_rs, na.rm=F) # 

    
    # Use the raster package  
    UK_rs = stack(ukcp18_file)
    proj4string(UK_rs) ="+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"
    plot(UK_rs[[1]])
    # 
    tmax_annual_r_usingraster = mean(UK_rs, na.rm=F)
    # 
    # plot(UK_rs_mean)
    
    par(mfrow=c(1,2))
    plot(tmax_annual_r) 
    plot(tmax_annual_r_usingraster,add=T, col="red")
    
    
    
    
    writeRaster(tmax_annual_r, filename = "data-processed/ukcp18_tmax.tif", driver = "GeoTIFF")
} else { 
    tmax_annual_r  <- raster ("data-processed/ukcp18_tmax.tiff")
}
