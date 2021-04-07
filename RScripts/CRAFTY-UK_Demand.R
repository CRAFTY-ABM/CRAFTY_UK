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

demand_df$Food.crops = 8186.423703690629 
demand_df$Fodder.crops = 5195.649612371075
demand_df$GF.redMeat= 14264.19945262999
demand_df$Fuel = 294.1256370164427
demand_df$Softwood = 1276.0841605779758
demand_df$Hardwood = 1258.1920655673368
demand_df$Biodiversity = 28367.838824467522
demand_df$Carbon = 28748.378631788495
demand_df$Recreation = 27051.50444290339
demand_df$Flood.reg = 24740.48409909865
demand_df$Employment = 22819.729784500967
demand_df$Ldiversity = 26418.315484676514
demand_df$GF.milk = 8079.994507255595

demand_df_org = demand_df

write.csv(demand_df, file = paste0(path_output, "/Demand/Baseline_demands_UK.csv"), quote = F, row.names = F)


# 50% increased food demand
demand_df$Food.crops = demand_df_org$Food.crops * 1.5
demand_df$Fodder.crops = demand_df_org$Fodder.crops * 1.5
demand_df$GF.redMeat= demand_df_org$GF.redMeat * 1.5
demand_df$GF.milk = demand_df_org$GF.milk * 1.5 



write.csv(demand_df, file = paste0(path_output, "/Demand/Baseline_demands_IncFoodDemand_UK.csv"), quote = F, row.names = F)


# 50% decreased food demand
demand_df$Food.crops = demand_df_org$Food.crops * 0.5
demand_df$Fodder.crops = demand_df_org$Fodder.crops * 0.5
demand_df$GF.redMeat= demand_df_org$GF.redMeat * 0.5
demand_df$GF.milk = demand_df_org$GF.milk * 0.5 


write.csv(demand_df, file = paste0(path_output, "/Demand/Baseline_demands_DecFoodDemand_UK.csv"), quote = F, row.names = F)


### SSP2 



SSP_names = paste0("SSP", 1:5)
ssp_idx = 2 

for (ssp_idx in c(2,4,5)) {
    
    
    
    
    demand_multiplier  = read.csv(paste0("~/Nextcloud/workspace_newEU/CRAFTY UK input CSV files/Scenarios/SSP", ssp_idx, "/SSP", ssp_idx, " demands.csv"))
    str(demand_multiplier)
    
    demand_cols = colnames(demand_multiplier)[-c(1:2)]
    
      
    demand_ssp = demand_df_org
    demand_ssp[, demand_cols] = demand_df_org[,demand_cols] * demand_multiplier[, demand_cols] # 2020-2100
    write.csv(demand_ssp, file = paste0(path_output, "Demand/Baseline_demands_", SSP_names[ssp_idx], "_UK.csv"), quote = F, row.names = F)
    
}
stop("ends here")













# PLUM 

plum_demand = read.csv(paste0(path_data, "/Demand/PLUM_demands1_16Feb2021_fromCalum.csv"))


SSP_names = paste0("SSP", 1:5)

s_idx = 1 
SSP_name = SSP_names[s_idx]

pd_tmp = lapply(SSP_names, FUN = function(SSP_name) plum_demand[plum_demand$Scenario == SSP_name,] )


food_names = colnames(plum_demand)[4:8]
f_idx = 1 


par(mfrow=c(2,3), mar=c(4,5,4,4))
for (f_idx in 1:5) { 
    food_name = food_names[f_idx]
    
    plot(pd_tmp[[1]]$Yr, pd_tmp[[1]][, food_name] * 100 , lty=1, type="l", main=food_name,  ylim=c(-100, 380), col=1, xlab="Year", ylab="Relative change (%)", cex.main=2, cex.lab=2)
    foreach(x = 2:5) %do% {lines(pd_tmp[[x]]$Yr, pd_tmp[[x]][, food_name] * 100 , col=x)}
}

plot.new()
legend("topleft", legend = SSP_names, col=1:5, lty=1, cex=2)

### Plum 
food_names_full = c("Food.crops", "Fodder.crops", "GF.milk", "GF.redMeat")

for (ssp_idx in 1:5) { 
    
    demand_ssp = demand_df_org
    demand_ssp[,food_names_full] = demand_df_org[,food_names_full] * pd_tmp[[ssp_idx]][1:71, food_names[-3]]
    write.csv(demand_ssp, file = paste0(path_output, "Demand/Baseline_demands_", SSP_names[ssp_idx], "_UK.csv"), quote = F, row.names = F)
    
}
stop("ends here")


## Food demand increase decrease

food_inc_res = read.csv("~/Nextcloud/workspace_newEU/CRAFTY UK output files/Food demand increased 50percent/Baseline-0-99-UK-AggregateAFTComposition.csv", sep=","
)

food_dec_res = read.csv("~/Nextcloud/workspace_newEU/CRAFTY UK output files/Food demand decreased 50percent/Baseline-0-99-UK-AggregateAFTComposition.csv", sep=","
)

food_inc_res[,3:18] = sapply(food_inc_res[, 3:18], FUN = function(x) as.numeric(as.character(x)))

food_dec_res[,3:18] = sapply(food_dec_res[, 3:18], FUN = function(x) as.numeric(as.character(x)))

par(mfrow=c(2,1))
barplot(t(as.matrix(food_inc_res[,3:18])), horiz=F)
barplot(t(as.matrix(food_dec_res[,3:18])), horiz=F)




# "NWCons", Color.magenta.brighter());
# "Bioenergy", Color.cyan.darker());
# "EA", Color.orange.darker());
# "EP", new Color(251,154,153));
# "IAfodder", Color.red.brighter());
# "IAfood", Color.blue.darker());
# "PNNB", Color.yellow.brighter() );
# "MW", Color.green.brighter());		
# "AF", new Color(31,120,180)); // #1f78b4
# "PNB", Color.gray.darker());	
# "PNC", Color.green.darker());	
# "PNNC", Color.pink.darker());
# "IP",new Color(178,223,138) );// #b2df8a
# "SusAr", Color.cyan.brighter());
# "VEP", rgb(51,160,44, maxColorValue = 255) 	#33a02c
# "Urban", rgb(255,127,0, maxColorValue = 255)  #6a3d9a	
# "Lazy FR", Color.white.brighter());	