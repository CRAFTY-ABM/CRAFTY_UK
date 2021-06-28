library(data.table)

path_wd = "~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY_UK/"
path_data = "~/Nextcloud/workspace_newEU/CRAFTY UK input CSV files/"
path_output =  "~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY_UK_Output/"

setwd(path_wd)

source("RScripts/CRAFTY-UK_grid_common.R")



climate_scenario_names = c("RCP4_5", "RCP8_5")
ssp_names = c("SSP2", "SSP3", "SSP4", "SSP5")




# basealc_cb_old = read.csv("~/Nextcloud/workspac
## demand file 

demand_years = seq(2020, 2100, 1)


### Demand file 
demand_df = data.frame(matrix(data = 0, nrow = length(demand_years), ncol = nrow(service_names) + 1))
colnames(demand_df) = c("Year", as.character(unlist(service_names)))
demand_df$Year = demand_years

# initial supply 
# (Dec 2020) Food.crops:35530.64245838188 Fodder.crops:35530.64245838188 GF.redMeat:19767.429285510883 Fuel:83893.15156101855 Softwood:38935.077142864604 Hardwood:30781.33200836578 Biodiversity:138489.38363181156 Carbon:176808.51146998556 Recreation:130596.0225824626 Flood.reg:117899.7173348387 Employment:206914.30307151988 Ldiversity:228043.30457211105 GF.milk:24268.733895594087
# (Jan 2021) Food.crops 7202.861290649656 Fodder.crops:6156.738559102148 GF.redMeat:13673.502137754882 Fuel:295.419645201573 Softwood:2175.345380988489 Hardwood:3326.121798678294 Biodiversity:26817.06011420219 Carbon:27946.732044533124 Recreation:25323.054640807914 Flood.reg:23886.4673829672 Employment:26935.447800923226 Ldiversity:28067.07404973141 GF.milk:8035.124725509452 }
# Jan 2021 V2
# Food.crops:7012.7360840051815 Fodder.crops:6030.557632755382 GF.redMeat:13130.659539533488 Fuel:324.9797815343801 Softwood:2210.970103250336 Hardwood:3356.278594882515 Biodiversity:28853.53729288205 Carbon:29611.48238920369 Recreation:26635.42097792667 Flood.reg:25242.539558505996 Employment:25421.6387181713 Ldiversity:27162.816680170537 GF.milk:7674.1159755553 }

# Feb 2021 V3 
# Food.crops:5450.980740380088 Fodder.crops:3449.886538479136 GF.redMeat:8974.441727952737 Fuel:869.8540937358832 Softwood:14100.464215901735 Hardwood:7320.2107197922505 Biodiversity:70280.18918608055 Carbon:76378.93166629746 Recreation:58229.06241735095 Flood.reg:59580.76451225908 Employment:55158.980548180836 Ldiversity:54145.202567015134 GF.milk:4608.74321500223 }
# 5472.517041297 Fodder.crops:3449.62114255335 GF.redMeat:8975.98984795043 Fuel:161.8690121276494 Softwood:695.7940502291776 Hardwood:955.9753897675859 Biodiversity:23669.846210096915 Carbon:23854.381516733112 Recreation:22690.020252839106 Flood.reg:21233.964108874767 Employment:15363.913828575349 Ldiversity:23592.16779867519 GF.milk:4607.907802453262 }

# Feb 2021 v4 (normalised capitals)
# Food.crops:8378.547566220961 Fodder.crops:5322.216765406225 GF.redMeat:14005.200587797144 Fuel:287.6222050347335 Softwood:1079.6727248494208 Hardwood:1277.5827018438838 Biodiversity:27421.04993405298 Carbon:27749.622798286346 Recreation:26157.4837258607 Flood.reg:23783.079948011233 Employment:22634.701520100836 Ldiversity:25608.141982522062 GF.milk:8123.276727664139 }

# Mar 2021 v5 (gap-filled and new input files)
# FOod.crops: 8126.127483719713 Fodder.crops:5159.215662720631 GF.redMeat:14132.341340761652 Fuel:289.60138136502195 Softwood:1257.6074197171397 Hardwood:1282.369238930499 Biodiversity:28309.846374701745 Carbon:28686.54821324183 Recreation:26989.762914758696 Flood.reg:24699.906842365253 Employment:22695.964383369457 Ldiversity:26396.84839747337 GF.milk:8008.22563578971 }

# Mar 2021 v6 (corrected capital files + woodland birch used)
# Food.crops:8186.423703690629 Fodder.crops:5195.649612371075 GF.redMeat:14264.19945262999 Fuel:294.1256370164427 Softwood:1276.0841605779758 Hardwood:1258.1920655673368 Biodiversity:28367.838824467522 Carbon:28748.378631788495 Recreation:27051.50444290339 Flood.reg:24740.48409909865 Employment:22819.729784500967 Ldiversity:26418.315484676514 GF.milk:8079.994507255595 }

# Apr 2021 v7 (corrected normalisation)
# ServiceSupply:Food.crops,ServiceSupply:Fodder.crops,ServiceSupply:GF.redMeat,ServiceSupply:Fuel,ServiceSupply:Softwood,ServiceSupply:Hardwood,ServiceSupply:Biodiversity,ServiceSupply:Carbon,ServiceSupply:Recreation,ServiceSupply:Flood.reg,ServiceSupply:Employment,ServiceSupply:Ldiversity,ServiceSupply:GF.milk,
# Food.crops:8186.423703690629 Fodder.crops:5195.649612371075 GF.redMeat:14264.197836760897 Fuel:294.1256370164427 Softwood:1276.0841605779758 Hardwood:1258.1920655673368 Biodiversity:28367.80359062123 Carbon:28748.343397942204 Recreation:27051.4692090571 Flood.reg:24740.448865911374 Employment:22819.7253144086 Ldiversity:26418.280292188985 GF.milk:8079.997422721622 }


# demand_april_df = demand_df
# 
# demand_april_df$Food.crops = 8186.423703690629 
# demand_april_df$Fodder.crops = 5195.649612371075
# demand_april_df$GF.redMeat= 14264.197836760897
# demand_april_df$Fuel = 294.1256370164427
# demand_april_df$Softwood = 1276.0841605779758
# demand_april_df$Hardwood = 1258.1920655673368
# demand_april_df$Biodiversity = 28367.80359062123
# demand_april_df$Carbon = 28748.343397942204
# demand_april_df$Recreation = 27051.4692090571
# demand_april_df$Flood.reg = 24740.448865911374
# demand_april_df$Employment = 22819.7253144086
# demand_april_df$Ldiversity = 26418.280292188985
# demand_april_df$GF.milk = 8079.997422721622

# UK 8 May (corrected bioenergy)
# 8171.249216469596 Fodder.crops:5235.621103324947 GF.redMeat:14220.749577566034 Fuel:322.5037499359316 Softwood:1274.9893223601905 Hardwood:1251.9150568453626 Biodiversity:28355.115907279094 Carbon:28747.033521170248 Recreation:27042.102412451037 Flood.reg:24733.52527212331 Employment:22783.41159388893 Ldiversity:26434.392613948257 GF.milk:8036.563073919001 }

# UK 15 May 
# 21766.97290234391 Fodder.crops:14112.409852241073 GF.redMeat:40108.11886736392 Fuel:1016.8883724829846 Softwood:5689.9844400473485 Hardwood:2058.4974824261094 Biodiversity:44946.91422698692 Carbon:46121.93703216233 Recreation:42349.64906339934 Flood.reg:36148.20719302831 Employment:55500.99449591252 Ldiversity:34846.58579084016 GF.milk:25826.372107517484 }

# UK 22 May  
# 21019.838,13621.988,38650.303,950.690,5565.719,2018.705,44034.783,45159.180,41491.447,35506.786,53563.917,34435.081,25062.023,9311.735,

# # UK 25 May
# ServiceSupply:Food.crops,ServiceSupply:Fodder.crops,ServiceSupply:GF.redMeat,ServiceSupply:Fuel,ServiceSupply:Softwood,ServiceSupply:Hardwood,ServiceSupply:Biodiversity,ServiceSupply:Carbon,ServiceSupply:Recreation,ServiceSupply:Flood.reg,ServiceSupply:Employment,ServiceSupply:Ldiversity,ServiceSupply:GF.milk,ServiceSupply:Sus.Prod,Demand:Food.crops,Demand:Fodder.crops,Demand:GF.redMeat,Demand:Fuel,Demand:Softwood,Demand:Hardwood,Demand:Biodiversity,Demand:Carbon,Demand:Recreation,Demand:Flood.reg,Demand:Employment,Demand:Ldiversity,Demand:GF.milk,Demand:Sus.Prod,Region,Tick
# 21862.666,14189.133,40447.693,1023.830,5753.034,2081.253,45026.054,46211.902,42408.602,36212.947,55867.298,34875.172,26079.491,9716.594,21019.838,13621.988,38650.303,950.690,5565.719,2018.705,44034.783,45159.180,41491.447,35506.786,53563.917,34435.081,25062.023,9311.735,UK,2020


demand_df$Food.crops = 21862.666
demand_df$Fodder.crops = 14189.133
demand_df$GF.redMeat= 40447.693
demand_df$Fuel = 1023.830
demand_df$Softwood = 5753.034
demand_df$Hardwood = 2081.253
demand_df$Biodiversity = 45026.054
demand_df$Carbon = 46211.902
demand_df$Recreation = 42408.602
demand_df$Flood.reg = 36212.947
demand_df$Employment = 55867.298
demand_df$Ldiversity = 34875.172
demand_df$GF.milk = 26079.491
demand_df$Sus.Prod = 9716.594

demand_df_org = demand_df
# 
write.csv(demand_df_org, file = paste0(path_output, "/Demand/Baseline_demands_UK.csv"), quote = F, row.names = F)

# library(openxlsx)
# initial_supply_df = read.xlsx(paste0(path_data, "/Scenarios/Latest/InitialSupply_15May2021.xlsx"), 1)


# demand_df_org = demand_df
# 
# write.csv(demand_df, file = paste0(path_output, "/Demand/Baseline_demands_UK.csv"), quote = F, row.names = F)

### SSP demands (scaled based on Apirl demand by Calum)


SSP_names = paste0("SSP", 1:5)

# 
# initial_supply_df_ssp = initial_supply_df[c(2, 6, 7, 9),]
# initial_supply_df_ssp$Scenario = SSP_names[-3]

ssp_idx = 2 

demand_names = as.character(unlist(service_names))


for (ssp_idx in c(1,2,4,5)) {
    
    ssp_name = SSP_names[ssp_idx]
    
    demand_cb  = read.csv(paste0("~/Nextcloud/workspace_newEU/CRAFTY UK input CSV files/Scenarios/170521 versions/demand/SSP", ssp_idx, " demands_scaled_popn.csv"))
    str(demand_cb)
    
    
    ## discount by the initial supply
    # init_sup = initial_supply_df_ssp[initial_supply_df_ssp$Scenario == ssp_name, paste0("ServiceSupply:", demand_names)] 
    init_sup = demand_df_org[1,-1]
    names(init_sup) = demand_names
    
    demand_cols = demand_names
    
    demand_ssp = demand_df_org
    demand_ssp[, demand_cols] = sapply(demand_cols, FUN = function(x) demand_cb[,x] * as.numeric( init_sup[x]/demand_cb[1,x])) # 2020-2100
    
    
    
    
    write.csv(demand_ssp, file = paste0(path_output, "Demand/Baseline-", SSP_names[ssp_idx], "_demands_UK.csv"), quote = F, row.names = F)
    
}


ssp_idx = 3 


# England 
# ServiceSupply:Food.crops,
# ServiceSupply:Fodder.crops,
# ServiceSupply:GF.redMeat,
# ServiceSupply:Fuel,
# ServiceSupply:Softwood,
# ServiceSupply:Hardwood,
# ServiceSupply:Biodiversity,
# ServiceSupply:Carbon,
# ServiceSupply:Recreation,
# ServiceSupply:Flood.reg,
# ServiceSupply:Employment,
# ServiceSupply:Ldiversity,
# ServiceSupply:GF.milk,
# Demand:Food.crops,Demand:Fodder.crops,Demand:GF.redMeat,Demand:Fuel,Demand:Softwood,Demand:Hardwood,Demand:Biodiversity,Demand:Carbon,Demand:Recreation,Demand:Flood.reg,Demand:Employment,Demand:Ldiversity,Demand:GF.milk,Region,Tick
# 7610.754,4893.897,7233.887,284.135,424.737,1074.832,7596.932,7724.278,6888.520,4776.289,15994.747,5802.092,6281.543,8186.424,5195.650,14264.198,294.126,1276.084,1258.192,28367.804,28748.343,27051.469,24740.449,22819.725,26418.280,8079.997,England,2020

# Scotland 
# ServiceSupply:Food.crops,ServiceSupply:Fodder.crops,ServiceSupply:GF.redMeat,ServiceSupply:Fuel,ServiceSupply:Softwood,ServiceSupply:Hardwood,ServiceSupply:Biodiversity,ServiceSupply:Carbon,ServiceSupply:Recreation,ServiceSupply:Flood.reg,ServiceSupply:Employment,ServiceSupply:Ldiversity,ServiceSupply:GF.milk,Demand:Food.crops,Demand:Fodder.crops,Demand:GF.redMeat,Demand:Fuel,Demand:Softwood,Demand:Hardwood,Demand:Biodiversity,Demand:Carbon,Demand:Recreation,Demand:Flood.reg,Demand:Employment,Demand:Ldiversity,Demand:GF.milk,Region,Tick
# 491.706,279.207,4968.312,9.522,689.487,114.545,19353.990,19541.037,18846.684,18741.427,5617.570,19311.690,507.847,8186.424,5195.650,14264.198,294.126,1276.084,1258.192,28367.804,28748.343,27051.469,24740.449,22819.725,26418.280,8079.997,Scotland,2020


# Wales
# ServiceSupply:Food.crops,ServiceSupply:Fodder.crops,ServiceSupply:GF.redMeat,ServiceSupply:Fuel,ServiceSupply:Softwood,ServiceSupply:Hardwood,ServiceSupply:Biodiversity,ServiceSupply:Carbon,ServiceSupply:Recreation,ServiceSupply:Flood.reg,ServiceSupply:Employment,ServiceSupply:Ldiversity,ServiceSupply:GF.milk,Demand:Food.crops,Demand:Fodder.crops,Demand:GF.redMeat,Demand:Fuel,Demand:Softwood,Demand:Hardwood,Demand:Biodiversity,Demand:Carbon,Demand:Recreation,Demand:Flood.reg,Demand:Employment,Demand:Ldiversity,Demand:GF.milk,Region,Tick
# 82.624,22.584,2084.507,0.025,166.404,75.283,1435.015,1502.984,1329.516,1239.574,1228.717,1315.761,1301.909,8186.424,5195.650,14264.198,294.126,1276.084,1258.192,28367.804,28748.343,27051.469,24740.449,22819.725,26418.280,8079.997,Wales,2020


# 
# demand_df_england_org = demand_df_scotland_org = demand_df_wales_org = demand_df[,]
# demand_df_england_april = demand_df_scotland_april = demand_df_wales_april = demand_df[,]

# demand_df_england_april[,-1] = t(sapply(1:nrow(demand_df), FUN = function(x) c(7610.754,4893.897,7233.887,284.135,424.737,1074.832,7596.932,7724.278,6888.520,4776.289,15994.747,5802.092,6281.543)))
#                                     
# demand_df_scotland_april[,-1] = t(sapply(1:nrow(demand_df), FUN = function(x) c(491.706,279.207,4968.312,9.522,689.487,114.545,19353.990,19541.037,18846.684,18741.427,5617.570,19311.690,507.847)))
# 
# demand_df_wales_april[,-1] =t(sapply(1:nrow(demand_df), FUN = function(x)  c(82.624,22.584,2084.507,0.025,166.404,75.283,1435.015,1502.984,1329.516,1239.574,1228.717,1315.761,1301.909)))



# Supply of region England: { Food.crops:7551.71409403912 Fodder.crops:4928.24311531373 GF.redMeat:7107.23448725221 Fuel:322.4028598281733 Softwood:423.6144611443088 Hardwood:1068.526811570599 Biodiversity:3050.638024789987 Carbon:6236.890004581034 Recreation:5518.025461491209 Flood.reg:4248.571378281984 Employment:15911.459268054521 Ldiversity:5471.92154695221 GF.milk:6238.081549551833 }
# Supply of region Scotland: { Food.crops:474.0402530478728 Fodder.crops:286.8787177474124 GF.redMeat:4695.469973560545 Fuel:0.0327729648682961 Softwood:689.4692468873932 Hardwood:114.52764458973753 Biodiversity:9428.4309937715 Carbon:18395.07657320967 Recreation:17704.531121454776 Flood.reg:17665.72998956932 Employment:5472.4567141067255 Ldiversity:18259.340332445223 GF.milk:507.829222347463 }
# Supply of region Wales: { Food.crops:74.43612854595882 Fodder.crops:20.719557602314577 GF.redMeat:2029.330174376222 Fuel:0.01833383737195789 Softwood:166.39727070460034 Hardwood:75.2767618765977 Biodiversity:762.3291419704672 Carbon:1419.151580986647 Recreation:1245.6901956526572 Flood.reg:1193.2488211796108 Employment:1219.4901834603138 Ldiversity:1272.097315039078 GF.milk:1301.902690061388 }


# dem_tmp = initial_supply_df[3:5,] 
# colnames(dem_tmp)[-c(1:2)] = demand_names
# 
# demand_df_england_org[,-1] = t(sapply(1:nrow(demand_df), FUN = function(x) as.numeric(dem_tmp[1,-c(1:2)])))
# # 
# demand_df_scotland_org[,-1] = t(sapply(1:nrow(demand_df), FUN = function(x) as.numeric(dem_tmp[2,-c(1:2)])))
# # 
# demand_df_wales_org[,-1] = t(sapply(1:nrow(demand_df), FUN = function(x) as.numeric(dem_tmp[,-c(1:2)])))

# write.csv(demand_df_england_org, file = paste0(path_output, "Demand/Baseline_demands_England.csv"), quote = F, row.names = F)
# write.csv(demand_df_scotland_org, file = paste0(path_output, "Demand/Baseline_demands_Scotland.csv"), quote = F, row.names = F)
# write.csv(demand_df_wales_org, file = paste0(path_output, "Demand/Baseline_demands_Wales.csv"), quote = F, row.names = F)




ssp_idx = 3 

demand_england_cb  = read.csv(paste0("~/Nextcloud/workspace_newEU/CRAFTY UK input CSV files/Scenarios/170521 versions/demand/SSP3 demands_scaled_popn_England.csv"))
demand_scotland_cb  = read.csv(paste0("~/Nextcloud/workspace_newEU/CRAFTY UK input CSV files/Scenarios/170521 versions/demand/SSP3 demands_scaled_popn_Scotland.csv"))
demand_wales_cb  = read.csv(paste0("~/Nextcloud/workspace_newEU/CRAFTY UK input CSV files/Scenarios/170521 versions/demand/SSP3 demands_scaled_popn_Wales.csv"))



demand_cols = demand_names

# England 
demand_england_ssp = demand_england_cb 
demand_england_ssp[, demand_cols] = sapply(demand_names, FUN = function(x) demand_england_cb[,x] *  as.numeric(init_sup[x]/demand_cb[1,x]))


write.csv(demand_england_ssp, file = paste0(path_output, "Demand/RCP6_0-", SSP_names[ssp_idx], "_demands_England.csv"), quote = F, row.names = F)


# Scotland 
demand_scotland_ssp = demand_scotland_cb
demand_scotland_ssp[, demand_cols] = sapply(demand_names, FUN = function(x) demand_scotland_cb[,x] * as.numeric(init_sup[x]/demand_cb[1,x]))

write.csv(demand_scotland_ssp, file = paste0(path_output, "Demand/RCP6_0-", SSP_names[ssp_idx], "_demands_Scotland.csv"), quote = F, row.names = F)

# Wales 
demand_wales_ssp = demand_wales_cb
demand_wales_ssp[, demand_cols] = sapply(demand_names, FUN = function(x) demand_wales_cb[,x] * as.numeric(init_sup[x]/demand_cb[1,x]))
write.csv(demand_wales_ssp, file = paste0(path_output, "Demand/RCP6_0-", SSP_names[ssp_idx], "_demands_Wales.csv"), quote = F, row.names = F)





stop("ends here")







# 
# 
# 
# 
# 
# 
# # PLUM 
# 
# plum_demand = read.csv(paste0(path_data, "/Demand/PLUM_demands1_16Feb2021_fromCalum.csv"))
# 
# 
# SSP_names = paste0("SSP", 1:5)
# 
# s_idx = 1 
# SSP_name = SSP_names[s_idx]
# 
# pd_tmp = lapply(SSP_names, FUN = function(SSP_name) plum_demand[plum_demand$Scenario == SSP_name,] )
# 
# 
# food_names = colnames(plum_demand)[4:8]
# f_idx = 1 
# 
# 
# par(mfrow=c(2,3), mar=c(4,5,4,4))
# for (f_idx in 1:5) { 
#     food_name = food_names[f_idx]
#     
#     plot(pd_tmp[[1]]$Yr, pd_tmp[[1]][, food_name] * 100 , lty=1, type="l", main=food_name,  ylim=c(-100, 380), col=1, xlab="Year", ylab="Relative change (%)", cex.main=2, cex.lab=2)
#     foreach(x = 2:5) %do% {lines(pd_tmp[[x]]$Yr, pd_tmp[[x]][, food_name] * 100 , col=x)}
# }
# 
# plot.new()
# legend("topleft", legend = SSP_names, col=1:5, lty=1, cex=2)
# 
# ### Plum 
# food_names_full = c("Food.crops", "Fodder.crops", "GF.milk", "GF.redMeat")
# 
# for (ssp_idx in 1:5) { 
#     
#     demand_ssp = demand_df_org
#     demand_ssp[,food_names_full] = demand_df_org[,food_names_full] * pd_tmp[[ssp_idx]][1:71, food_names[-3]]
#     write.csv(demand_ssp, file = paste0(path_output, "Demand/Baseline_demands_", SSP_names[ssp_idx], "_UK.csv"), quote = F, row.names = F)
#     
# }
# stop("ends here")
# 
# 
# ## Food demand increase decrease
# 
# food_inc_res = read.csv("~/Nextcloud/workspace_newEU/CRAFTY UK output files/Food demand increased 50percent/Baseline-0-99-UK-AggregateAFTComposition.csv", sep=","
# )
# 
# food_dec_res = read.csv("~/Nextcloud/workspace_newEU/CRAFTY UK output files/Food demand decreased 50percent/Baseline-0-99-UK-AggregateAFTComposition.csv", sep=","
# )
# 
# food_inc_res[,3:18] = sapply(food_inc_res[, 3:18], FUN = function(x) as.numeric(as.character(x)))
# 
# food_dec_res[,3:18] = sapply(food_dec_res[, 3:18], FUN = function(x) as.numeric(as.character(x)))
# 
# par(mfrow=c(2,1))
# barplot(t(as.matrix(food_inc_res[,3:18])), horiz=F)
# barplot(t(as.matrix(food_dec_res[,3:18])), horiz=F)
# 
# 
