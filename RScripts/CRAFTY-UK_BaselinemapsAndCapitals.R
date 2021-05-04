library(data.table)

path_wd = "~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY_UK/"
path_data = "~/Nextcloud/workspace_newEU/CRAFTY UK input CSV files/"
path_output =  "~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY_UK_Output/"

setwd(path_wd)

source("RScripts/CRAFTY-UK_grid_common.R")





# basealc_cb_old = read.csv("~/Nextcloud/workspace_newEU/CRAFTY UK input CSV files/AFT/Basline allocation 2_from Calum 6 Jan 2021_all properly processed.csv")
basealc_cb = read.csv(paste0(path_data, "/AFT/Basline allocation 3_from Calum 5 Feb 2021_food fodder fixed_ready for mask files.csv"))




table(basealc_cb$Agent) %>% print %>% sum
table(aftnames[ match(basealc_cb$Agent, aftnames$AFT_cb), "AFT"] ) %>% print %>% sum

basealc_cb$AFT15 = aftnames[ match(basealc_cb$Agent, aftnames$AFT_cb), "AFT"]
table(basealc_cb$AFT15) %>% print %>% sum

basealc_csv_df = data.frame( X = cellids$X_col, Y = cellids$Y_row)

stopifnot(table(basealc_cb$FID==basealc_csv_df$FID)==nrow(basealc_csv_df))




# ,X,Latitude,Longitude,Crop.productivity,Forest.productivity,Grassland.productivity,Financial.capital,Human.capital,Social.capital,Manufactured.capital,Urban.capital,FR,BT,Region


str(basealc_csv_df)

# baseline HC 

# hc_base = read.csv("~/Nextcloud/workspace_newEU/CRAFTY UK input CSV files/Capital/HumanCapital/CRAFTY_UK_HumanCapital_H_2020_SSP1.csv")
# hc_base = read.csv("Output/HumanCapital/CRAFTY_UK_HumanCapital_H_2020_SSP1.csv")

# basealc_csv_df$Human = hc_base$HumanCapital
# 
# sqrt(mean((hc_base$HumanCapital - basealc_cb$Human)^2, na.rm=T))
# plot(basealc_cb$Human, hc_base$HumanCapital)

# capital_csv = read.csv("~/Nextcloud/workspace_newEU/CRAFTY UK input CSV files/Capital/All_capitals_16Dec2020_CB.csv")
# capital_csv

# str(capital_csv)
# colnames(capital_csv)[5:18] 
colnames(basealc_cb)[7:20]

colnames(basealc_cb)[12] = "NConifer.suit"
colnames(basealc_cb)[15] = "Bioenergy.suit"
colnames(basealc_cb)[5] = "NBroadleaf.suit"


match( colnames(basealc_cb)[7:20], capital_names)






doProtectedArea = FALSE 

if (doProtectedArea) { 
    ProtectedAreas_csv_df = read.csv(paste0(path_output, "/CRAFTY_UK_ProtectedAreas_PA.csv")  )
    
    
    ProtectedAreaL1_df = cbind(basealc_csv_df[, c("X", "Y")], PROTECTED_L1 = ProtectedAreas_csv_df$Protected_L1)
    ProtectedAreaL2_df = cbind(basealc_csv_df[, c("X", "Y")], PROTECTED_L2 = ProtectedAreas_csv_df$Protected_L2)
    
    ProtectedAreaL1_Removed_df =  cbind(basealc_csv_df[, c("X", "Y")], PROTECTED_L1 = 0)
    ProtectedAreaL2_Removed_df =  cbind(basealc_csv_df[, c("X", "Y")], PROTECTED_L2 = 0)
    
    write.csv(ProtectedAreaL1_df, file = paste0(path_output, "/CRAFTY_UK_ProtectedAreas_L1.csv"), quote = F, row.names = F)
    write.csv(ProtectedAreaL2_df, file = paste0(path_output, "/CRAFTY_UK_ProtectedAreas_L2.csv"), quote = F, row.names = F)
    write.csv(ProtectedAreaL1_Removed_df, file = paste0(path_output, "/CRAFTY_UK_ProtectedAreas_L1_Removed.csv"), quote = F, row.names = F)
    write.csv(ProtectedAreaL2_Removed_df, file = paste0(path_output, "/CRAFTY_UK_ProtectedAreas_L2_Removed.csv"), quote = F, row.names = F)
    
    
    }


# Mask_final = Urban2015_csv_df$Urban + ProtectedAreaL1_df$Protected.MaskL1 + ProtectedAreaL2_df$Protected.MaskL2
# Mask_final[Mask_final>1] = 1 
# 
# FinalMask_csv_df = cbind(basealc_csv_df[, c("X", "Y")], Mask = Mask_final )
# 
# 
# write.csv(FinalMask_csv_df, file = paste0(path_output, "/CRAFTY_GB_MaskAggregated.csv"), quote = F, row.names = F)


# scenario_names_df = expand.grid(climate_scenario_names, ssp_names)

climate_scenario_names = c("Baseline", "RCP4_5", "RCP8_5")
ssp_names = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")



scenario_names_df = rbind(
    c("Baseline", "Baseline")
    , expand.grid(climate_scenario_names, ssp_names, stringsAsFactors = F)
)
colnames(scenario_names_df) = c("Climate", "SSP")


# currently five scenarios including baseline
scenario_names_df = scenario_names_df[c(1,2,5,8,11,14, 6,12, 7,16),]
 


n_scenario = nrow(scenario_names_df)

# timeslices
scene_years_l = c(replicate("", n=1, F), replicate( seq(2020, 2070, 10), n=9, F))


# adjust capitals by SSP  
capital_multiplier_SSP1 = read.csv(paste0(path_data, "Scenarios/Updates 180421/SSP1/Suitability_multipliers.csv")) # 18 Apr
capital_multiplier_SSP2 = read.csv(paste0(path_data, "Scenarios/SSP2New/Suitability_multipliers.csv")) # 4 May
capital_multiplier_SSP3 = read.csv(paste0(path_data, "Scenarios/Updates 180421/SSP3/Suitability_multipliers.csv")) # 18 Apr
capital_multiplier_SSP4 = read.csv(paste0(path_data, "Scenarios/SSP4New/Suitability_multipliers.csv")) # 4 May 
capital_multiplier_SSP5 = read.csv(paste0(path_data, "Scenarios/SSP5New/Suitability_multipliers2.csv")) # 4 May


# capital_multiplier_SSP2

adjust_multiplier_l = list(NULL, capital_multiplier_SSP1, capital_multiplier_SSP2,capital_multiplier_SSP3, capital_multiplier_SSP4,capital_multiplier_SSP5, capital_multiplier_SSP2, capital_multiplier_SSP4, capital_multiplier_SSP2, capital_multiplier_SSP5) 

# multiply to SSP2 at each decade 


# has to be there in prior..
woodland_baseline_df = read.csv(paste0(path_output, "Capital/Woodland capital/CRAFTY_UK_Woodland_Baseline.csv"), sep = ",")
# Suitabilities 
suitability_baseline_df = read.csv(paste0(path_output, "Capital/Suitability/CRAFTY_UK_Suitability_Baseline.csv"), sep = ",")
# Capitals
sc_baseline_df = read.csv(paste0(path_output, "Capital/Capitals/SocialCapital/CRAFTY_UK_SocialCapital_S_2020_SSP1.csv"), sep=",")
fc_baseline_df = read.csv(paste0(path_output, "Capital/Capitals/FinancialCapital/CRAFTY_UK_FinancialCapital_F_2020_SSP1.csv"), sep=",")
mc_baseline_df = read.csv(paste0(path_output, "Capital/Capitals/ManufacturedCapital/CRAFTY_UK_ManufacturedCapital_M_2020_SSP1.csv"), sep=",")
hc_baseline_df = read.csv(paste0(path_output, "Capital/Capitals/HumanCapital/CRAFTY_UK_HumanCapital_H_2020_SSP1.csv"), sep=",")

 


colnames(suitability_baseline_df)
summary(suitability_baseline_df)
summary(woodland_baseline_df)


scene_idx = 3
year_idx = 5


scene_idxs = c(3, 5:10) # ssp 2,4, and 5


for (scene_idx in scene_idxs) { 

    scene_name_tmp = scenario_names_df[scene_idx,] 
    scene_years_tmp = scene_years_l[[scene_idx]]
    
    
    adjust_multiplier_df = adjust_multiplier_l[[scene_idx]]
    adjust_cols_tmp = colnames(adjust_multiplier_df)[-1]
    
    for (year_idx in seq_along(scene_years_tmp)) { 
        
        scnene_year_tmp = scene_years_tmp[year_idx]
        
        clim_suffix_tmp = paste0(scene_name_tmp$Climate,   ifelse(scene_name_tmp$Climate=="Baseline", yes = "", no = paste0("_", scnene_year_tmp)) )  
         
        ssp_suffix_tmp = ifelse(scnene_year_tmp=="", yes = "_2020_SSP1", no = paste0("_", scnene_year_tmp, "_", scene_name_tmp$SSP)) # 2020 SSP1 for baseline
        
        both_suffix_tmp = paste0(scene_name_tmp$Climate, ifelse(scene_name_tmp$SSP=="Baseline", yes = "", no = paste0("-",scene_name_tmp$SSP)),  ifelse(scnene_year_tmp=="", yes = "", no = "_"), scnene_year_tmp) 
        
        
        ### dummy 
        capital_csv = basealc_cb[,7:20]
        
        
        ### Collect new capital files
        
        
        # Suitabilities 
        suitability_df = read.csv(paste0(path_output, "Capital/Suitability/CRAFTY_UK_Suitability_", clim_suffix_tmp, ".csv"), sep = ",")
        colnames(suitability_df)

        summary(suitability_df)
        
        # normalise them
        # Normalised relative to the baseline, so the value of '1' is the maximum in the baseline and all the scenarios change around that.
        capital_csv[, "Arable.suit"] = suitability_df$Arable_Suitability / max(suitability_baseline_df$Arable_Suitability, na.rm=T)
        capital_csv[, "Igrass.suit"] = suitability_df$ImprovedGrassland_Suitability / max( suitability_baseline_df$ImprovedGrassland_Suitability , na.rm=T)
        capital_csv[, "SNGrass.suit"] = suitability_df$SemiNaturalGrassland_Suitability / max( suitability_baseline_df$SemiNaturalGrassland_Suitability, na.rm=T)
        
        
        summary(capital_csv)
        
        # Woodland capitals
 
        woodland_df = read.csv(paste0(path_output, "Capital/Woodland capital/CRAFTY_UK_Woodland_", clim_suffix_tmp, ".csv"), sep=",")
        colnames(woodland_df) # [6:12] = capital_names[c(8,9,10,11,)]
        
 
        (woodland_names %in% as.character(capital_names[8:14]))
        
         
        # plot(woodland_df_tmp$NNBroadleaf.suit, woodland_df$NNBroadleaf.suit)
        # plot(woodland_df_tmp$Nbroadleaf.suit, woodland_df$Nbroadleaf.suit)
        
         
        
        # normalise them
        woodland_max = sapply(woodland_baseline_df[, woodland_names], max, na.rm=T)
        
        woodland_df_norm = sapply(1:length(woodland_max), FUN = function(x) woodland_df[, woodland_names[x]] / woodland_max[x]) 
        
        summary(woodland_df_norm)
        
        capital_csv[, woodland_names] = woodland_df_norm
        
        
        
        
        # Capitals
        sc_df = read.csv(paste0(path_output, "Capital/Capitals/SocialCapital/CRAFTY_UK_SocialCapital_S", ssp_suffix_tmp, ".csv"), sep=",")
        fc_df = read.csv(paste0(path_output, "Capital/Capitals/FinancialCapital/CRAFTY_UK_FinancialCapital_F", ssp_suffix_tmp, ".csv"), sep=",")
        mc_df = read.csv(paste0(path_output, "Capital/Capitals/ManufacturedCapital/CRAFTY_UK_ManufacturedCapital_M", ssp_suffix_tmp, ".csv"), sep=",")
        hc_df = read.csv(paste0(path_output, "Capital/Capitals/HumanCapital/CRAFTY_UK_HumanCapital_H", ssp_suffix_tmp, ".csv"), sep=",")
        
        
        # 
        capital_csv[, "Financial"] = fc_df$FinancialCapital %>% as.character %>% as.numeric
        capital_csv[, "Human"] = hc_df$HumanCapital %>% as.character %>% as.numeric
        capital_csv[, "Manufactured"] = mc_df$ManufacturedCapital %>% as.character %>% as.numeric
        capital_csv[, "Social"] = sc_df$SocialCapital %>% as.character %>% as.numeric
        
        
        capital_csv[, "Financial"] = capital_csv[, "Financial"]/ max(fc_baseline_df$FinancialCapital, na.rm=T)
        capital_csv[, "Human"] = capital_csv[, "Human"]/ max(hc_baseline_df$HumanCapital, na.rm=T)
        capital_csv[, "Manufactured"] = capital_csv[, "Manufactured"]/ max(mc_baseline_df$ManufacturedCapital, na.rm=T)
        capital_csv[, "Social"] = capital_csv[, "Social"]/ max(sc_baseline_df$SocialCapital, na.rm=T)
        
        
        # capital_reord_df = capital_csv[, match( colnames(capital_csv)[5:20], capital_names_16)]
        # colnames(capital_reord_df) = capital_names$Capital[match( colnames(capital_csv)[5:20], capital_names_16)]
        
        capital_reord_df = capital_csv[, as.character(capital_names)]
        capital_reord_df[is.na(capital_reord_df)] = 0 
        
        capital_csv_df = cbind(basealc_csv_df[, c("X", "Y")], capital_reord_df[,])
        
        
        
        ## Adjust capitals (6 Apr 2021)
        if (!is.null(adjust_multiplier_df)) { 
            print("adjust capitals")
             
            adjust_multiplier_tmp = adjust_multiplier_df[adjust_multiplier_df$Yr == scnene_year_tmp, ]
            print(adjust_multiplier_tmp)
            capital_csv_df[,adjust_cols_tmp] =  sapply(adjust_cols_tmp, FUN = function(x) capital_csv_df[, x] * adjust_multiplier_tmp[,x])
            
        }
        
        
        
        ## baseline capital with x and y (rnk)
        write.csv(capital_csv_df, file = paste0(path_output, "Capital/UK_capitals-", both_suffix_tmp, ".csv"), quote = F, row.names = F)
        
        
 
        
        # write basic allocation file if baseline 
        if (scene_name_tmp$SSP == "Baseline") { 
            
            basealc_csv_df = cbind(basealc_csv_df, capital_reord_df)
            
            # AFT 
            
            basealc_csv_df$FR = as.character(basealc_cb$AFT15)
            table(basealc_csv_df$FR)
            
            ## Replace current allocation by the LCM 2015 urban mask 
            Urban2015_csv_df = read.csv( paste0(path_output, "UrbanMask/UrbanMask2020.csv"))
            str(Urban2015_csv_df)
            
            
            table(Urban2015_csv_df$FR_IMMUTABLE==1)
            
            
            basealc_csv_df$FR[Urban2015_csv_df$FR_IMMUTABLE==1] = "Urban" 
            
            
            # Behavioural Type (BT)
            basealc_csv_df$BT = 0 
            
            # Region (@TODO give region names)
            
            
            # str(basealc_csv_df)
            
            # Baseline map
            write.csv(basealc_csv_df, file = paste0(path_output, "Basegrid/Baseline_map_UK.csv"), quote = F, row.names = F)
            
            
            ## Empty map
            basealc_csv_df$FR = "NOT_ASSIGNED"
            
            write.csv(basealc_csv_df, file = paste0(path_output, "Basegrid/Baseline_map_UK_empty.csv"), quote = F, row.names = F)
        }
    }
    
}
