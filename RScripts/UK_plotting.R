setwd("~/Nextcloud/CRAFTY/CRAFTY_WEB/")
source("RScripts/Data_UK.R")
source("RScripts/Functions_CRAFTY_WEB.R")

FR_L1_tb = read.csv("~/Nextcloud/CRAFTY/CRAFTY_UK/data_UK/csv/PA_L1_Restrictions.csv")
FR_L2_tb = read.csv("~/Nextcloud/CRAFTY/CRAFTY_UK/data_UK/csv/PA_L2_Restrictions.csv")

library(latticeExtra)
library(Thermimage)


levelplot(as.matrix(FR_L1_tb)[,-1], las=2)


fr_l1 = as.matrix(FR_L1_tb)[,-1]
fr_l1 = t(fr_l1)[,nrow(fr_l1):1]
levelplot((fr_l1), las=2)


rownames(fr_l1) = FR_L1_tb$FR
colnames(fr_l1) = rev(FR_L1_tb$FR)

levelplot(fr_l1, las=2)



levelplot((fr_l1), xlab="To", ylab="From", col.regions = c("blue", "grey90"), at=c(-1,0,1), scales=list(x=list( rot=90)), main = "Protected areas mask L1")
# obj2 = 


 
fr_l2 = as.matrix(FR_L2_tb)[,-1]
fr_l2 = t(fr_l2)[,nrow(fr_l2):1]

rownames(fr_l2) = FR_L2_tb$FR
colnames(fr_l2) = rev(FR_L2_tb$FR)
levelplot((fr_l2), xlab="To", ylab="From", col.regions = c("blue", "grey"), at=c(-1,0,1), scales=list(x=list( rot=90)), main = "Protected areas mask L2")

print(obj1)



# Obj <- 
  # levelplot((fr_l1), xlab="Prediction", ylab="Reference", col.regions = c(rep("white", 1), colorRampPalette(brewer.pal("YlGnBu", n = 9))(32)), at=c(-1,0, seq(1, max(confmat_MF), length=30)), scales=list(x=list( rot=90)))+xyplot(y ~ x, data = fr_l1,
  #                                                                                                                                                                                                                                        panel = function(y, x, ...) {
  #                                                                                                                                                                                                                                          ltext(x = x, y = y, labels = confmat_MF_df$lb, cex = 1, font = 1,fontfamily = "HersheySans")
  #                                                                                                                                                                                                                                        })

# print(Obj)
