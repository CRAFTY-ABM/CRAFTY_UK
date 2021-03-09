
setwd("~/Nextcloud/workspace_newEU/CRAFTY_newEU/data_EU28_compact/worlds/EU/regionalisations/28/")


dirs = list.dirs(recursive = F)
dirs = dirs[-c(2:3)]
dirs
d = dirs[1]

for (d in dirs) { 
    
    f = list.files(d, pattern=".csv$", full.names = T)   
    
    if (length(f) == 1) { 
        a = read.csv2(f, sep = ",")
        a$Urban = 1 # not zero.. 
        write.table(a, file=f, quote = F, sep=",",  row.names = F)
    } else { 
        
      for (f2 in f) { 
          a = read.csv2(f2, sep = ",")
          a$Urban = 1 # not zero.. 
          write.table(a, file=f2, sep=",", quote = F, row.names = F)
          
      }
    }
}



# change urban column names
urbanmask_files = list.files("LandUseControl/UrbanMask/", recursive = T, pattern = "\\.csv$", all.files = T, full.names = T)
ub = urbanmask_files[1] 
for (ub in urbanmask_files) { 
    
    a = read.csv2(ub, sep=",")
    colnames(a)[colnames(a)=="FR_MUTABLE"] = "FR_IMMUTABLE"
    write.table(a, file=ub, quote = F, sep=",", row.names = F)
    
    
}
