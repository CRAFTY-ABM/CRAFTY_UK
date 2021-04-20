# linearly interpolate annual capital files

library(abind)

path_wd = "~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY_UK/"
path_data = "~/Nextcloud/workspace_newEU/CRAFTY UK input CSV files/"
path_output =  "~/Nextcloud/CRAFTY/Output/"

setwd(path_wd)

source("RScripts/CRAFTY-UK_grid_common.R")


## demand file 

 
climate_scenario_names = c("RCP4_5", "RCP8_5")
ssp_names = c("SSP2", "SSP3", "SSP4", "SSP5")


# scenario_names_df = rbind(
#   c("Baseline", ""),
#   expand.grid(climate_scenario_names, ssp_names, stringsAsFactors = F)
# )
# colnames(scenario_names_df) = c("Climate", "SSP")
# 
# # currently five scenarios including baseline
# scenario_names_df = scenario_names_df[c(1,2,3,6)+1,]
# n_scenario = nrow(scenario_names_df)
# 
# 


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
ssp_intensive_param_adjuster = list(SSP1 = c(0.05, 0.05, 0.07),
                                    SSP2 = c(0.05, 0.05, 0.07),
                                    SSP3 = c(0.05, 0.05, 0.07),  # individual level random noise (service level?) still to be implemented
                                    SSP4 = c(0.04, 0.06, 0.07), 
                                    SSP5 = c(0.06, 0.04, 0.07))
# extensive types
ssp_extensive_param_adjuster = list(SSP1 = c(0.04, 0.06, 0.07), 
                                    SSP2 = c(0.05, 0.05, 0.07), 
                                    SSP3 = c(0.05, 0.05, 0.07), 
                                    SSP4 = c(0.06, 0.04, 0.07), 
                                    SSP5 = c(0.06, 0.04, 0.07))


var_names = c("givingUpDistributionMean", "givingInDistributionMean", "givingUpProb")

SSP_names = paste0("SSP", 1:5)
ssp_idx = 1 
aft_idx = 1 

for (ssp_idx in c(1,2,3,4,5)) {
  
  SSP_name_tmp = SSP_names[ssp_idx]
  intensive_param_adjuster_tmp = ssp_intensive_param_adjuster[[ssp_idx]]
  extensive_param_adjuster_tmp = ssp_extensive_param_adjuster[[ssp_idx]]
   
  for (aft_idx in 1:length(aft_names)) { 
    aft_name_tmp = aft_names[aft_idx]
    threshold_param_tmp = Baseline_aft_params_l[[aft_idx]]
     
    scene_param_tmp = threshold_param_tmp
    
    if(aft_name_tmp %in% intensive_names) { 
      scene_param_tmp[var_names] = threshold_param_tmp[var_names] + intensive_param_adjuster_tmp
      
    } else { 
      scene_param_tmp[var_names] = threshold_param_tmp[var_names] + extensive_param_adjuster_tmp
 
      # do nothing
    }
    
    path_tmp = paste0(path_output, "Behavioural parameters/Thresholds/", SSP_name_tmp)
    if (!dir.exists(path_tmp)) { 
      dir.create(path_tmp, recursive = T)
    }
    write.csv(scene_param_tmp, file = paste0(path_tmp, "/AftParams_", aft_name_tmp, ".csv"), quote = F, row.names = F)
  }
}


 