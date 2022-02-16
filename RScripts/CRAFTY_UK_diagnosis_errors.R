# CRAFTY_UK_diagnosis_errors.R

dem_v26 = read.csv("~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY_WEB_UK_DATA/NoRemoval_v26/Thresholds/Baseline/Baseline-0-99-UK-AggregateServiceDemand.csv")
dem_v28 = read.csv("~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY_WEB_UK_DATA/NoRemoval_v28/Thresholds/Baseline/Baseline-0-99-UK-AggregateServiceDemand.csv")
dem_v27 = read.csv("~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY_WEB_UK_DATA/NoRemoval_v27/Thresholds/Baseline/Baseline-0-99-UK-AggregateServiceDemand.csv")


rmse = function(x1, x2) { 
 return(sqrt((mean((x1-x2)^2))))   
}
bias = function(x1, x2) { 
    return(mean(x1-x2))   
}
par(mfrow=c(4,4))
for (i in 15:28) { 
    x1 = dem_v26[,i ]
    x2 = dem_v28[,i]
    
    plot(x1, x2, main = colnames(dem_v26)[i], sub = paste0("RMSE=", round(rmse(x1, x2),1), " Bias=", round(bias(x1,x2),1)), xlab="v26", ylab="v28")
    
}


# Fodder
plot(dem_v26$Demand.GF.redMeat, dem_v28$Demand.GF.redMeat, main = rmse)
# Fodder
plot(dem_v26$Demand.Fodder.crops, dem_v28$Demand.Fodder.crops)
# Fodder
plot(dem_v26$Demand.Fuel, dem_v28$Demand.Fuel)
# Fodder
rmse(dem_v26$Demand.Biodiversity, dem_v28$Demand.Biodiversity)
# Fodder
plot(dem_v26$Demand.Fodder.crops, dem_v28$Demand.Fodder.crops)
# Fodder
plot(dem_v26$Demand.Fodder.crops, dem_v28$Demand.Fodder.crops)
# Fodder
plot(dem_v26$Demand.Fodder.crops, dem_v28$Demand.Fodder.crops)
# Fodder
plot(dem_v26$Demand.Fodder.crops, dem_v28$Demand.Fodder.crops)
# Fodder
plot(dem_v26$Demand.Fodder.crops, dem_v28$Demand.Fodder.crops)


# sus prod
plot(dem_v26$Demand.Sus.Prod, dem_v28$Demand.Sus.Prod)
plot(dem_v26$Demand.Sus.Prod, dem_v27$Demand.Sus.Prod)
plot(dem_v27$Demand.Sus.Prod, dem_v28$Demand.Sus.Prod)

plot(dem_v26$ServiceSupply.Sus.Prod, dem_v28$ServiceSupply.Sus.Prod)
plot(dem_v26$ServiceSupply.Sus.Prod, dem_v27$ServiceSupply.Sus.Prod)
plot(dem_v27$ServiceSupply.Sus.Prod, dem_v27$ServiceSupply.Sus.Prod)

# food
plot(dem_v26$Demand.Food.crops, dem_v28$Demand.Food.crops)
plot(dem_v26$Demand.Food.crops, dem_v27$Demand.Food.crops)
plot(dem_v27$Demand.Food.crops, dem_v28$Demand.Food.crops)

plot(dem_v26$ServiceSupply.Food.crops, dem_v28$ServiceSupply.Food.crops)
plot(dem_v26$ServiceSupply.Food.crops- dem_v28$ServiceSupply.Food.crops)

plot(dem_v26$ServiceSupply.Food.crops, dem_v27$ServiceSupply.Food.crops)
plot(dem_v27$ServiceSupply.Food.crops, dem_v28$ServiceSupply.Food.crops)


# Fodder
plot(dem_v26$Demand.Fodder.crops, dem_v28$Demand.Fodder.crops)
plot(dem_v26$Demand.Fodder.crops, dem_v27$Demand.Fodder.crops)
plot(dem_v27$Demand.Fodder.crops, dem_v28$Demand.Fodder.crops)

plot(dem_v26$ServiceSupply.Fodder.crops, dem_v28$ServiceSupply.Fodder.crops)
plot(dem_v26$ServiceSupply.Fodder.crops, dem_v27$ServiceSupply.Fodder.crops)
plot(dem_v27$ServiceSupply.Fodder.crops, dem_v28$ServiceSupply.Fodder.crops)
