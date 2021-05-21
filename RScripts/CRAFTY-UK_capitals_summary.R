
library(data.table)

library(abind)

path_wd = "~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY_UK/"
path_data = "~/Nextcloud/workspace_newEU/CRAFTY UK input CSV files/"
path_output =  "~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY_UK_Output/"

setwd(path_wd)

source("RScripts/CRAFTY-UK_grid_common.R")

 


# scenario_names_df = expand.grid(climate_scenario_names, ssp_names)
climate_scenario_names = c("Baseline", "RCP2_6", "RCP4_5", "RCP6_0", "RCP8_5")
ssp_names = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")


# currently seven scenarios including baseline

scenario_names_df = data.frame(rbind(
    c("Baseline", "Baseline")
    , c(climate_scenario_names[2], ssp_names[1]) # R2S1
    , c(climate_scenario_names[3], ssp_names[2]) # R4S2
    , c(climate_scenario_names[3], ssp_names[4]) # R4S4
    , c(climate_scenario_names[4], ssp_names[3]) # R6S3
    , c(climate_scenario_names[5], ssp_names[2]) # R8S2
    , c(climate_scenario_names[5], ssp_names[5]) # R8S5
))

colnames(scenario_names_df) = c("Climate", "SSP")


n_scenario = nrow(scenario_names_df)

# timeslices
scene_years_l = c(replicate("", n=1, F), replicate( seq(2020, 2070, 10), n=6, F))


year_intv = 10 


scene_idx =4
year_idx = 1

registerDoMC(16)

path_inputdata = "~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY_UK/data_UK/"

### summary capitals

registerDoMC()

scene_idxs = 2:n_scenario
 
for (scene_idx in scene_idxs) { 
    
    
    scene_name_tmp = scenario_names_df[scene_idx,] 
    
    print(scene_name_tmp)
    
    scene_years_tmp = 2020:2079
    
    both_suffix_tmp = paste0(scene_name_tmp$Climate, ifelse(scene_name_tmp$SSP=="", yes = "", no = "-"), scene_name_tmp$SSP) 
    
    res_scene = foreach (year_annual_idx = seq_along(scene_years_tmp), .combine = rbind) %dopar% { 
        
        scnene_year_tmp = scene_years_tmp[year_annual_idx]
        print(scnene_year_tmp)
        
        
        ann_name_tmp =  file = paste0(path_inputdata, "worlds/UK/capitals/", scene_name_tmp$Climate, "-", scene_name_tmp$SSP, "/UK_capitals-", paste0(scene_name_tmp$Climate, ifelse(scene_name_tmp$SSP=="", yes = "", no = "-"), scene_name_tmp$SSP, ifelse(scnene_year_tmp=="", yes = "", no = "_"), scnene_year_tmp), ".csv")
        
        ann_dt = read.csv(ann_name_tmp)
        return(sapply(ann_dt[, as.character(capital_names)], mean, na.rm=T))
        
    }
    
    res_scene = cbind(Tick= scene_years_tmp, res_scene)
    
    write.csv(res_scene, file =paste0(path_output, "Capital/Summary/", both_suffix_tmp, "-0-99-UK-AggregateCapital.csv"), quote = F, row.names = F)
    
}

 
baseline_name_tmp =  file = paste0(path_inputdata, "worlds/UK/capitals/Baseline/UK_capitals-Baseline.csv")

baseline_dt = read.csv(baseline_name_tmp)
baseline_summary = sapply(baseline_dt[, as.character(capital_names)], mean, na.rm=T)
baseline_max_summary = sapply(baseline_dt[, as.character(capital_names)], max, na.rm=T)

stopifnot(all(baseline_max_summary==1)) # must be 1 


base_out = cbind(Tick=2020, t(data.frame(baseline_summary)))
write.csv(base_out, file =paste0(path_output, "Capital/Summary/Baseline-0-99-UK-AggregateCapital.csv"), quote = F, row.names = F)




