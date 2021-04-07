library(data.table)

path_wd = "~/Nextcloud/CRAFTY/CRAFTY_UK/"
path_data = "~/Nextcloud/workspace_newEU/CRAFTY UK input CSV files/"
path_output =  "~/Nextcloud/CRAFTY/Output/"

setwd(path_wd)

source("RScripts/CRAFTY-UK_grid_common.R")



climate_scenario_names = c("RCP4_5", "RCP8_5")
ssp_names = c("SSP2", "SSP4", "SSP5")




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



# scenario_names_df = expand.grid(climate_scenario_names, ssp_names)


scenario_names_df = rbind(
    c("Baseline", "")
    , expand.grid(climate_scenario_names, ssp_names, stringsAsFactors = F)
)
colnames(scenario_names_df) = c("Climate", "SSP")

n_scenario = nrow(scenario_names_df)

# currently five scenarios including baseline
scenario_names_df = scenario_names_df[c(1,2,3,4,7),]

# timeslices
scene_years_l = list("", seq(2020, 2070, 10), seq(2020, 2070, 10))

 
# adjust capitals by SSP (now only for SSP2)
capital_multiplier_SSP2 = read.csv(paste0(path_data, "Scenarios/SSP2/Suitability_multipliers.csv"))
capital_multiplier_SSP4 = read.csv(paste0(path_data, "Scenarios/SSP4/Suitability_multipliers.csv"))
capital_multiplier_SSP5 = read.csv(paste0(path_data, "Scenarios/SSP5/Suitability_multipliers.csv"))

# capital_multiplier_SSP2

adjust_multiplier_l = list(NULL, capital_multiplier_SSP2, capital_multiplier_SSP2, capital_multiplier_SSP4, capital_multiplier_SSP5) 

# multiply to SSP2 at each decade 


scene_idx = 2
year_idx = 1 

for (scene_idx in 1:n_scenario) { 
     
    scen_name_tmp = scenario_names_df[scene_idx,] 
    scene_years_tmp = scene_years_l[[scene_idx]]
    
    
    adjust_multiplier_df = adjust_multiplier_l[[scene_idx]]
    adjust_cols_tmp = colnames(adjust_multiplier_df)[-1]
    
    for (year_idx in seq_along(scene_years_tmp)) { 
        
        scnene_year_tmp = scene_years_tmp[year_idx]
        
        clim_suffix_tmp = paste0(scen_name_tmp$Climate,   ifelse(scnene_year_tmp=="", yes = "", no = "_"), scnene_year_tmp) 
        ssp_suffix_tmp = ifelse(scnene_year_tmp=="", yes = "_2020_SSP1", no = paste0("_", scnene_year_tmp, "_", scen_name_tmp$SSP)) # 2020 SSP1 for baseline
        
        both_suffix_tmp = paste0(scen_name_tmp$Climate, ifelse(scen_name_tmp$SSP=="", yes = "", no = "-"), scen_name_tmp$SSP, ifelse(scnene_year_tmp=="", yes = "", no = "_"), scnene_year_tmp) 
        
        
        ### dummy 
        capital_csv = basealc_cb[,7:20]
        
        
        ### Collect new capital files
        
        
        # Suitabilities 
        suitability_df = read.csv(paste0(path_output, "Capital/Suitability/CRAFTY_UK_Suitability_", clim_suffix_tmp, ".csv"), sep = ",")
        colnames(suitability_df)
        
        # normalise them
        capital_csv[, "Arable.suit"] = suitability_df$Arable_Suitability / max(suitability_df$Arable_Suitability, na.rm=T)
        capital_csv[, "Igrass.suit"] = suitability_df$ImprovedGrassland_Suitability / max( suitability_df$ImprovedGrassland_Suitability , na.rm=T)
        capital_csv[, "SNGrass.suit"] = suitability_df$SemiNaturalGrassland_Suitability / max( suitability_df$SemiNaturalGrassland_Suitability, na.rm=T)
        
        
        # summary(capital_csv)
        
        # Woodland capitals
        
        # woodland_df = read.csv2("CRAFTY UK Capital files/Woodland capital/data-processed/woodland_capitals.csv", sep=",")
        # 
        # plot(basealc_cb$NNBroadleaf.suit, woodland_df$non.native.broadleaf_YC)
        # plot(basealc_cb$Bioenergy.suit, woodland_df$bioenergy_YC)
        # 
        # capital_csv$NNBroadleaf.suit = woodland_df$non.native.broadleaf_YC
        # unused native woodland (SBI)
        # capital_csv$Nbroadleaf.suit = woodland_df$native.broadleaf_YC
        # capital_csv$NConifer.suit = woodland_df$native.conifer_YC
        # capital_csv$NNConifer.suit = woodland_df$non.native.conifer_YC
        # capital_csv$AgroForestry.suit = woodland_df$agroforestry_YC
        # capital_csv$Bioenergy.suit = woodland_df$bioenergy_YC
        # capital_csv$Tree.suit = woodland_df$Mixed
        
        
        # library(tidyverse)
        woodland_df = read.csv(paste0(path_output, "Capital/Woodland capital/CRAFTY_UK_Woodland_", clim_suffix_tmp, ".csv"), sep=",")
        colnames(woodland_df) # [6:12] = capital_names[c(8,9,10,11,)]
        
        # capital_csv$NNBroadleaf.suit = woodland_df$non.native.broadleaf_YC
        # unused native woodland (SBI)
        # capital_csv$Nbroadleaf.suit = woodland_df$native.broadleaf_YC
        # capital_csv$NConifer.suit = woodland_df$native.conifer_YC
        # capital_csv$NNConifer.suit = woodland_df$non.native.conifer_YC
        # capital_csv$AgroForestry.suit = woodland_df$agroforestry_YC
        # capital_csv$Bioenergy.suit = woodland_df$bioenergy_YC
        # capital_csv$Tree.suit = woodland_df$Mixed
        # woodland_df$mixed.YC / max(woodland_df$mixed.YC, na.rm=T)
        
        (woodland_names %in% as.character(capital_names[8:14]))
        
        woodland_df_tmp = data.frame(sapply(woodland_df[,woodland_names], FUN = function(x) x / max(x, na.rm=T)))
        colnames(woodland_df_tmp) = woodland_names
        
        # plot(woodland_df_tmp$NNBroadleaf.suit, woodland_df$NNBroadleaf.suit)
        # plot(woodland_df_tmp$Nbroadleaf.suit, woodland_df$Nbroadleaf.suit)
        
        str(woodland_df_tmp)
        
        capital_csv[, woodland_names] = woodland_df_tmp
        
        
        
        
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
        
        
        capital_csv[, "Financial"] = capital_csv[, "Financial"]/ max(capital_csv[, "Financial"], na.rm=T)
        capital_csv[, "Human"] = capital_csv[, "Human"]/ max(capital_csv[, "Human"], na.rm=T)
        capital_csv[, "Manufactured"] = capital_csv[, "Manufactured"]/ max(capital_csv[, "Manufactured"], na.rm=T)
        capital_csv[, "Social"] = capital_csv[, "Social"]/ max(capital_csv[, "Social"], na.rm=T)
        
        
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
        
        
        # strange strip 
        # which((capital_csv_df$X == 234) & (capital_csv_df$Y == 930)) # 8975 
        # which((capital_csv_df$X == 226) & (capital_csv_df$Y == 940)) # 8975
        
        # fc_df[8975,]
        # capital_csv_df[7321, "Financial"]
        
        
        # writeRaster(r2, filename = "borough.tif", overwrite=T)
        
        
        # capital_names
        # table(names(capital_reord_df) == capital_names)
        
        
        # write basic allocation file if baseline 
        if (scen_name_tmp$Climate == "Baseline") { 
            
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
