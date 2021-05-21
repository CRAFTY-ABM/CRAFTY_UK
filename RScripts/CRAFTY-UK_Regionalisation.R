# Divide the world into multiple regions and interpolate annual data

library(data.table)
library(abind)
library(sf)


path_wd = "~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY_UK/"
path_data = "~/Nextcloud/workspace_newEU/CRAFTY UK input CSV files/"
path_output =  "~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY_UK_Output/"

setwd(path_wd)

source("RScripts/CRAFTY-UK_grid_common.R")


pixelSelection = FALSE 
if (pixelSelection) { # 
  
  
  
  ## Boundary
  ukregions = readOGR(paste0(path_data, "Boundaries"), layer = "NUTS_Level_1_(January_2018)_Boundaries", verbose = T)
  
  ukregions$nuts118nm
  
  
  ukregions_sf = st_as_sf(ukregions)
  
  # plot(ukregions_sf)
  
  # ukregions_sf$nuts118nm
  uk_nuts1 = as.character(ukregions_sf$nuts118nm)
  
  scot_idx = which(uk_nuts1 %in% "Scotland")
  wales_idx = which(uk_nuts1 %in% "Wales")
  nireland_idx = which(uk_nuts1 %in% "Northern Ireland")
  england_idx = setdiff(seq_along(uk_nuts1), c(scot_idx, wales_idx, nireland_idx))
  
  scotland_shp = ukregions[scot_idx,]
  wales_shp = ukregions[wales_idx,]
  england_shp = gUnaryUnion(ukregions[england_idx,])
  
  
  uk_nuts1[england_idx]
  
  
  
  CHESS_BNG_csv = read.csv("Basegrid/CHESS_1k_grid.csv") # BNG perhaps
  # proj4string(UK_rs) ="+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"
  # plot(UK_rs[[1]])
  #  
  CHESS_BNG_csv = CHESS_BNG_csv[, c("FID", "POINT_X", "POINT_Y")]
  
  CHESS_BNG_spdf = SpatialPointsDataFrame(cbind(CHESS_BNG_csv$POINT_X, CHESS_BNG_csv$POINT_Y), proj4string =  crs(proj4.BNG), data = CHESS_BNG_csv)
  # 
  CHESS_BNG_sp = SpatialPixels(CHESS_BNG_spdf)
  
  # writeOGR(CHESS_BNG_spdf, dsn = paste0(path_output, "Basegrid"), layer = "CHESS_BNG_sp", driver = "ESRI Shapefile")
  
  # plot(CHESS_BNG_sp)
  CHESS_BNG_sf = st_as_sf(CHESS_BNG_sp)
  
  
  england_sf = st_as_sf(england_shp)
  sf::st_crs(ukregions_sf) = proj4.BNG
  
  
  
  
  # nearest feature join (using sf) # now doing in QGIS using nnjoin
  nnjoin_sf_w_pts = st_join(CHESS_BNG_sf[ ,], ukregions_sf, 
                            join = st_nearest_feature)
  
  nnjoin_sf_w_pts
  # writeOGR(nnjoin_sf_w_pts, dsn = paste0(path_output, "Basegrid"), layer = "nnjoin_uk", driver = "ESRI Shapefile")
  
  nuts118nm = as.character(nnjoin_sf_w_pts$nuts118nm)
  
  
  nuts118nm[nuts118nm %in% uk_nuts1[england_idx]] = "England"
  table(nuts118nm)
  
  CHESS_BNG_spdf$NUTS118NM = nnjoin_sf_w_pts$nuts118nm
  CHESS_BNG_spdf$FourRegions = nuts118nm
  
  
  
  # writeOGR(CHESS_BNG_spdf, dsn = paste0(path_output, "Basegrid"), layer = "CHESS_BNG_sp_NUTS1", driver = "ESRI Shapefile", overwrite=T)
  
  
  
  
  # hangs.. 
  # scotland_shp2 = gUnaryUnion(scotland_shp)
  # scotland_shp3 = gBuffer(wales_shp2, width = 2000)
  # 
  # wales_shp2 = gUnaryUnion(wales_shp)
  # wales_shp3 = gBuffer(wales_shp2, width = 2000)
  # # 
  # # 
  # 
  # 
  # tb_contains = gIntersects( ukregions, CHESS_BNG_spoints[,], byid = T)
  # sum(apply(tb_contains, MARGIN = 2, FUN = function(x) length(x[x])))
  # 
  # nrow(CHESS_BNG_spoints)
  
  
  
  # # slow 
  # # res = sf::st_overlaps(CHESS_BNG_sf[11E4:11.1E4,], england_sf)
  # res_eng = over(CHESS_BNG_sp[,], england_shp)
  # table(res_eng)
  # 
  # 
  # scotland_shp2 = gUnaryUnion(scotland_shp)
  # scotland_shp3 = gBuffer(wales_shp2, width = 2000)
  # 
  # res_scot = over(CHESS_BNG_sp[,], gUnaryUnion(scotland_shp))
  # table(res_scot$objectid)
  # 
  # 
  # wales_shp2 = gUnaryUnion(wales_shp)
  # wales_shp3 = gBuffer(wales_shp2, width = 2000)
  # 
  # res_wal = over(CHESS_BNG_sp[,], wales_shp3)
  # # table(res_wal)
  # 
  # eng_idx2 = which(res_eng==1)
  # scot_idx2 = which(!is.na(res_scot$objectid))
  # wal_idx2 = which(res_wal==1)
  # 
  # plot(CHESS_BNG_sp[eng_idx2,])
  # plot(CHESS_BNG_sp[scot_idx2,])
  # plot(CHESS_BNG_sp[wal_idx2,])
  # 
  # 
  # gCoveredBy(CHESS_BNG_sp[1:10,], england_shp)
  #  
  # length(eng_idx2) + length(scot_idx2) + length(wal_idx2)
  # 
  # rest_idx2 = setdiff(1:nrow(CHESS_BNG_csv), c(eng_idx2, scot_idx2, wal_idx2))
  # plot(CHESS_BNG_sp[rest_idx2,])
  
} else { 
  CHESS_BNG_spdf = readOGR(paste0(path_output, "Basegrid/CHESS_BNG_sp_NUTS1.shp"))
  CHESS_regions = as.character(CHESS_BNG_spdf$ForRgns)
}



climate_scenario_names = c("Baseline", "RCP4_5", "RCP8_5")
ssp_names = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")



scenario_names_df = rbind(
  c("Baseline", "Baseline")
  , expand.grid(climate_scenario_names, ssp_names, stringsAsFactors = F)
)
colnames(scenario_names_df) = c("Climate", "SSP")


# currently five scenarios including baseline
scenario_names_df = scenario_names_df[c(1,2,5,8,11,14, 6,12, 7,16),]
scenario_names_df

n_scenario = nrow(scenario_names_df)

# timeslices
scene_years_l = c(replicate("", n=1, F), replicate( seq(2020, 2070, 10), n=9, F))

year_intv = 10 


scene_idx =4
year_idx = 1

registerDoMC(16)


# DON'T reginalise capitals and just use UK data (with row and col id) 

# for (scene_idx in 4) { # baseline SSP3
#   
#   scene_name_tmp = scenario_names_df[scene_idx,] 
#   
#   scene_years_tmp = scene_years_l[[scene_idx]]
#   
#   
#   for (year_idx in seq_along(scene_years_tmp)) { 
#     
#     scnene_year_tmp = scene_years_tmp[year_idx]
#     
#     
#     both_suffix_tmp = paste0(scene_name_tmp$Climate, ifelse(scene_name_tmp$SSP=="Baseline", yes = "", no = paste0("-",scene_name_tmp$SSP)), ifelse(scnene_year_tmp=="", yes = "", no = "_"), scnene_year_tmp)
#     both_suffix_next_tmp = paste0(scene_name_tmp$Climate, ifelse(scene_name_tmp$SSP=="Baseline", yes = "", no = paste0("-",scene_name_tmp$SSP)), ifelse(scnene_year_tmp=="", yes = "", no = "_"), scnene_year_tmp + 10)
#     
#     print(both_suffix_tmp)
#     
#     
#     ## baseline capital with x and y (rnk)
#     capital_decadal = read.csv( file = paste0(path_output, "Capital/UK_capitals-", both_suffix_tmp, ".csv"))
#     
#     capital_next_name = paste0(path_output, "Capital/UK_capitals-", both_suffix_next_tmp, ".csv")
#     
#     
#     if (file.exists(capital_next_name)) { 
#       capital_next_decadal = read.csv( capital_next_name)
#     } else { 
#       capital_next_decadal = capital_decadal
#     }
#     
#     
#     
#     # col_idx = 7
#     res_col_l = foreach (col_idx = 3:16) %dopar% { 
#       d1 = capital_decadal[,col_idx]
#       d2 = capital_next_decadal[, col_idx]
#       
#       summary(d1)
#       summary(d2)
#       
#       
#       d_intp = d1 + sapply(1:(year_intv-1), FUN = function(x) x * (d2 - d1) / year_intv ) # linear interpolation 
#       return(d_intp)
#     }
#     str(res_col_l)
#     
#     res_col_arr = aperm(abind(res_col_l, along=0.5), perm = c(3,2,1))
#     str(res_col_arr)
#     
#     # year_annual_idx = 1 
#     
#     
#     foreach(year_annual_idx = 1:9) %dopar% { 
#       ann_df_tmp  = cbind(capital_decadal[,1:2], res_col_arr[year_annual_idx,,])
#       colnames(ann_df_tmp) = colnames(capital_decadal)
#       
#       
#       # regionalisation 
#       ann_england = ann_df_tmp[CHESS_regions=="England",]
#       ann_scotland = ann_df_tmp[CHESS_regions=="Scotland",]
#       ann_wales = ann_df_tmp[CHESS_regions=="Wales",]
#       
#       
#       ann_name_england_tmp =  file = paste0(path_output, "Capital/Annual_Interpolated/England_capitals-", paste0(scene_name_tmp$Climate, ifelse(scene_name_tmp$SSP=="", yes = "", no = "-"), scene_name_tmp$SSP, ifelse(scnene_year_tmp=="", yes = "", no = "_"), scnene_year_tmp + year_annual_idx), ".csv")
#       
#       ann_name_scotland_tmp =  file = paste0(path_output, "Capital/Annual_Interpolated/Scotland_capitals-", paste0(scene_name_tmp$Climate, ifelse(scene_name_tmp$SSP=="", yes = "", no = "-"), scene_name_tmp$SSP, ifelse(scnene_year_tmp=="", yes = "", no = "_"), scnene_year_tmp + year_annual_idx), ".csv")
#       
#       ann_name_wales_tmp =  file = paste0(path_output, "Capital/Annual_Interpolated/Wales_capitals-", paste0(scene_name_tmp$Climate, ifelse(scene_name_tmp$SSP=="", yes = "", no = "-"), scene_name_tmp$SSP, ifelse(scnene_year_tmp=="", yes = "", no = "_"), scnene_year_tmp + year_annual_idx), ".csv")
#       
#       write.csv(ann_england, file =ann_name_england_tmp, quote = F, row.names = F)
#       write.csv(ann_scotland, file =ann_name_scotland_tmp, quote = F, row.names = F)
#       write.csv(ann_wales, file =ann_name_wales_tmp, quote = F, row.names = F)
#       
#     }
#     
#     
#     # regionalisation 
#     capital_england = capital_decadal[CHESS_regions=="England",]
#     capital_scotland = capital_decadal[CHESS_regions=="Scotland",]
#     capital_wales = capital_decadal[CHESS_regions=="Wales",]
#     
#     
#     write.csv(capital_england, file =paste0(path_output, "Capital/Regionalisation/England_capitals-", paste0(scene_name_tmp$Climate, ifelse(scene_name_tmp$SSP=="", yes = "", no = "-"), scene_name_tmp$SSP, ifelse(scnene_year_tmp=="", yes = "", no = "_"), scnene_year_tmp ), ".csv"), quote = F, row.names = F)
#     write.csv(capital_scotland, file =paste0(path_output, "Capital/Regionalisation/Scotland_capitals-", paste0(scene_name_tmp$Climate, ifelse(scene_name_tmp$SSP=="", yes = "", no = "-"), scene_name_tmp$SSP, ifelse(scnene_year_tmp=="", yes = "", no = "_"), scnene_year_tmp ), ".csv"), quote = F, row.names = F)
#     write.csv(capital_wales, file =paste0(path_output, "Capital/Regionalisation/Wales_capitals-", paste0(scene_name_tmp$Climate, ifelse(scene_name_tmp$SSP=="", yes = "", no = "-"), scene_name_tmp$SSP, ifelse(scnene_year_tmp=="", yes = "", no = "_"), scnene_year_tmp ), ".csv"), quote = F, row.names = F)
#     
#     
#   }
#   
# }


### baseline maps

path_inputdata = "~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY_UK/data_UK/"


baseline_name_tmp =  file = paste0(path_output, "Basegrid/Baseline_map_UK.csv")

baseline_map   = read.csv(baseline_name_tmp)


# regionalisation 
baseline_england = baseline_map[CHESS_regions=="England",]
# nrow(capital_scotland)
baseline_scotland = baseline_map[CHESS_regions=="Scotland",]
baseline_wales = baseline_map[CHESS_regions=="Wales",]




write.csv(baseline_england, file =paste0(path_output, "Basegrid/Baseline_map_England.csv"), quote = F, row.names = F)
write.csv(baseline_scotland, file =paste0(path_output, "Basegrid/Baseline_map_Scotland.csv"), quote = F, row.names = F)
write.csv(baseline_wales, file =paste0(path_output, "Basegrid/Baseline_map_Wales.csv"), quote = F, row.names = F)












