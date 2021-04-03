# linearly interpolate annual capital files


library(abind)

path_wd = "~/Nextcloud/CRAFTY/CRAFTY_UK/"
path_data = "~/Nextcloud/workspace_newEU/CRAFTY UK input CSV files/"
path_output =  "~/Nextcloud/CRAFTY/Output/"

setwd(path_wd)

source("RScripts/CRAFTY-UK_grid_common.R")


## demand file 

demand_years = seq(2020, 2090, 10)

climate_scenario_names = c("RCP8_5")
ssp_names = c("SSP1", "SSP5")




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


match( colnames(basealc_cb)[7:20], capital_names)


library(data.table)

# scenario_names_df = expand.grid(climate_scenario_names, ssp_names)


scenario_names_df = rbind(
    # c("Baseline", "")
    # ,
    expand.grid(climate_scenario_names, ssp_names, stringsAsFactors = F)
)
colnames(scenario_names_df) = c("Climate", "SSP")

n_scenario = nrow(scenario_names_df)

# timeslices
scene_years_l = list( seq(2020, 2070, 10), seq(2020, 2070, 10))



year_intv = 10 


scene_idx = 1
year_idx = 1


for (scene_idx in 1:n_scenario) { 
    
    
    scen_name_tmp = scenario_names_df[scene_idx,] 
    
    scene_years_tmp = scene_years_l[[scene_idx]]
    
    
    for (year_idx in seq_along(scene_years_tmp)) { 
        
        scnene_year_tmp = scene_years_tmp[year_idx]
        
        
        both_suffix_tmp = paste0(scen_name_tmp$Climate, ifelse(scen_name_tmp$SSP=="", yes = "", no = "-"), scen_name_tmp$SSP, ifelse(scnene_year_tmp=="", yes = "", no = "_"), scnene_year_tmp) 
        both_suffix_next_tmp = paste0(scen_name_tmp$Climate, ifelse(scen_name_tmp$SSP=="", yes = "", no = "-"), scen_name_tmp$SSP, ifelse(scnene_year_tmp=="", yes = "", no = "_"), scnene_year_tmp + 10) 
        
        
        ## baseline capital with x and y (rnk)
        capital_decadal = read.csv( file = paste0(path_output, "Capital/UK_capitals-", both_suffix_tmp, ".csv"))
        
        capital_next_name = paste0(path_output, "Capital/UK_capitals-", both_suffix_next_tmp, ".csv")
        if (file.exists(capital_next_name)) { 
            capital_next_decadal = read.csv( capital_next_name)
        } else { 
            capital_next_decadal = capital_decadal
        }
        
        # col_idx = 7
        res_col_l = foreach (col_idx = 3:16) %do% { 
            d1 = capital_decadal[,col_idx]
            d2 = capital_next_decadal[, col_idx]
            
            summary(d1)
            summary(d2)
            
            
            d_intp = d1 + sapply(1:(year_intv-1), FUN = function(x) x * (d2 - d1) / year_intv ) # linear interpolation 
            return(d_intp)
        }
        str(res_col_l)
        
        res_col_arr = aperm(abind(res_col_l, along=0.5), perm = c(3,2,1))
        str(res_col_arr)
        
        # year_annual_idx = 1 
 
        
        foreach(year_annual_idx = 1:9) %do% { 
            ann_df_tmp  = cbind(capital_decadal[,1:2], res_col_arr[year_annual_idx,,])
            colnames(ann_df_tmp) = colnames(capital_decadal)
            ann_name_tmp =  file = paste0(path_output, "Capital/Annual_Interpolated/UK_capitals-", paste0(scen_name_tmp$Climate, ifelse(scen_name_tmp$SSP=="", yes = "", no = "-"), scen_name_tmp$SSP, ifelse(scnene_year_tmp=="", yes = "", no = "_"), scnene_year_tmp + year_annual_idx), ".csv")
            write.csv(ann_df_tmp, file =ann_name_tmp, quote = F, row.names = F)
        }
        
        
        
    }
    
}
