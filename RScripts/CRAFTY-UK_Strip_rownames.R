
setwd("~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY_UK/data_UK/production/")

d = list.dirs()

d = d[-1]

rnames = c("Food.crops","Fodder.crops", "GF.redMeat","Fuel","Softwood","Hardwood","Biodiversity", "Carbon", "Recreation","Flood.reg", "Employment","Ldiversity","GF.milk")

for (d_tmp in d) { 
    
    a = list.files(d_tmp, full.names = T)
    print(a) 
    
    
    idxs = 1:length(a)
    
    idx=9
    
     for (idx in idxs) { 
        df1 = read.csv(a[idx])
        
        df1$X.1 = NULL
        df1[df1=="NA"] = 0 
        df1[is.na(df1)] = 0 
        colnames(df1)[1] = ""
        df1[,1] = rnames
        write.csv(df1, file = a[idx], quote = F, row.names = F)
    }
}