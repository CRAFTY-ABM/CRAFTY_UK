# linearly interpolate annual capital files

library(abind)

path_wd = "~/Nextcloud/CRAFTY/CRAFTY_UK/"
path_data = "~/Nextcloud/workspace_newEU/CRAFTY UK input CSV files/"
path_output =  "~/Nextcloud/CRAFTY/Output/"

setwd(path_wd)

source("RScripts/CRAFTY-UK_grid_common.R")


## demand file 

demand_years = seq(2020, 2100, 10)

climate_scenario_names = c("RCP4_5", "RCP8_5")
ssp_names = c("SSP2","SSP4", "SSP5")


scenario_names_df = rbind(
  c("Baseline", ""),
  expand.grid(climate_scenario_names, ssp_names, stringsAsFactors = F)
)
colnames(scenario_names_df) = c("Climate", "SSP")

# currently five scenarios including baseline
scenario_names_df = scenario_names_df[c(1,2,3,6)+1,]
n_scenario = nrow(scenario_names_df)



scene_idx = 1
year_idx = 1

# Changes to make to the scenarios:
# SSP2 Benefit functions for all services are set as 1x + 0, except the food services, which are 2x + 0
# Social networks take density of self-type agents within 20km radius and add up to 0.02 onto production

# SSP4
# Benefit functions for all services are set as 0.5x + 0, except the food services, which are 1x + 0
# PAs removed in 2050
# [x] When thresholds are on:
#	Intensive thresholds: -0.01GU, +0.01GI
#	All other thresholds: +0.01GU, -0.01GI
# Social networks take density of self-type agents within 20km radius and add up to 0.05 onto production
# 
# SSP5
# Benefit functions for all services are set as 1x + 0, except the food services, which are 3x + 0
# No PAs at all
# [x] When thresholds are on:
# Higher GU (+0.01), lower GI (-0.01) for all agents
# Social networks take density of self-type agents within 20km radius and add up to 0.05 onto production


### Agent CSV file 

# read.csv("


# threshold parameters
b_idx = 2 
aft_names = as.character(aftnames$AFT)
aft_names = setdiff(aft_names, "NOT_ASSIGNED")
intensive_names = as.character(aftnames$AFT[aftnames$Type=="Intensive"])

Baseline_aft_params_l = lapply(aft_names, FUN = function(x) read.csv(paste0(path_data, "Behavioural parameters/Default/BaselineBehaviour/AftParams_", x, ".csv"), stringsAsFactors = F ))
Threshold_aft_params_l = lapply(aft_names, FUN = function(x) read.csv(paste0(path_data, "Behavioural parameters/Default/Thresholds/AftParams_", x, ".csv"), stringsAsFactors = F ))

# applied to intensive types (Gu, Gi, Prob)
ssp_intensive_param_adjuster = list(SSP1 = c(0,0,0), SSP2 = c(0,0,0), SSP3= c(0,0,0), SSP4 = c(-0.01, 0.01, 0), SSP5 = c(0.01, -0.01, 0))
ssp_extensive_param_adjuster = list(SSP1 = c(0,0,0), SSP2 = c(0,0,0), SSP3= c(0,0,0), SSP4 = c(+0.01, -0.01, 0), SSP5 = c(0.01, -0.01, 0))


SSP_names = paste0("SSP", 1:5)
ssp_idx = 2 
aft_idx = 1 

for (ssp_idx in c(2,4,5)) {
  
  SSP_name_tmp = SSP_names[ssp_idx]
  intensive_param_adjuster_tmp = ssp_intensive_param_adjuster[[ssp_idx]]
  extensive_param_adjuster_tmp = ssp_extensive_param_adjuster[[ssp_idx]]
   
  for (aft_idx in 1:length(aft_names)) { 
    aft_name_tmp = aft_names[aft_idx]
    threshold_param_tmp = Threshold_aft_params_l[[aft_idx]]
     
    scene_param_tmp = threshold_param_tmp
    
    if(aft_name_tmp %in% intensive_names) { 
      scene_param_tmp$givingUpDistributionMean = threshold_param_tmp$givingUpDistributionMean + intensive_param_adjuster_tmp[1]
      scene_param_tmp$givingInDistributionMean = threshold_param_tmp$givingInDistributionMean + intensive_param_adjuster_tmp[2]
      
    } else { 
      scene_param_tmp$givingUpDistributionMean = threshold_param_tmp$givingUpDistributionMean + extensive_param_adjuster_tmp[1]
      scene_param_tmp$givingInDistributionMean = threshold_param_tmp$givingInDistributionMean + extensive_param_adjuster_tmp[2]
      # do nothing
    }
    
    path_tmp = paste0(path_output, "Behavioural parameters/Thresholds/", SSP_name_tmp)
    if (!dir.exists(path_tmp)) { 
      dir.create(path_tmp, recursive = T)
    }
    write.csv(scene_param_tmp, file = paste0(path_tmp, "/AftParams_", aft_name_tmp, ".csv"), quote = F, row.names = F)
  }
}



stop("ends here")





