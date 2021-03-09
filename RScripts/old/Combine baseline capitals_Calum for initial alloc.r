# Combine capitals

# Now read in woodland capitals
woodcaps<-read.csv("C:/Users/brown-c/Documents/Work/CRAFTY-UK/Capitals/Woodland capitals/woodlandCapitals_CHESSbaseline.csv")
# And other capitals
suit<-read.csv("C:/Users/brown-c/Documents/Work/CRAFTY-UK/Capitals/CRAFTY_UK_Suitability.csv")

# And other capitals
Fcap<-read.csv("C:/Users/brown-c/Documents/Work/CRAFTY-UK/Capitals/Processed capitals/CRAFTY_UK_FinancialCapital_F_2020_SSP1.csv")
Hcap<-read.csv("C:/Users/brown-c/Documents/Work/CRAFTY-UK/Capitals/Processed capitals/CRAFTY_UK_HumanCapital_H_2020_SSP1.csv")
Mcap<-read.csv("C:/Users/brown-c/Documents/Work/CRAFTY-UK/Capitals/Processed capitals/CRAFTY_UK_ManufacturedCapital_M_2020_SSP1.csv")
Scap<-read.csv("C:/Users/brown-c/Documents/Work/CRAFTY-UK/Capitals/Processed capitals/CRAFTY_UK_SocialCapital_S_2020_SSP1.csv")

caps<-cbind(Fcap$FinancialCapital,Hcap$HumanCapital,Mcap$ManufacturedCapital,Scap$SocialCapital)
colnames(caps)<-c("Financial.capital","Human.capital","Manufactured.capital","Social.capital" )

allcaps<-cbind(suit,woodcaps[,6:13],caps)
allcaps<-allcaps[,-4]
allcaps<-allcaps[,-4]
# Also remove wetland suitability
allcaps<-allcaps[,-5]
# And native woodland YC
allcaps<-allcaps[,-8]

# Cap names should be: Human	Social	Manufactured	Financial	Arable.suit	Igrass.suit	SNGrass.suit
# Bioeneergy.suit	AgroForestry.suit	NNConifer.suit	Nconifer.suit	NNBroadleaf.suit	Nbroadleaf.suit	Tree.suit

colnames(allcaps)<-c("FID","long","lat","Arable.suit","Igrass.suit","SNGrass.suit","NNBroadleaf.suit","Nbroadleaf.suit",
                     "Nconifer.suit","NNConifer.suit","AgroForestry.suit","Bioeneergy.suit","Tree.suit","Financial",
                     "Human","Manufactured","Social")

write.csv(allcaps,file="C:/Users/brown-c/Documents/Work/CRAFTY-UK/Capitals/All_capitals.csv")

caps[is.na(caps)]<-0
# Normalise capitals
caps$Arable.suit<-caps$Arable.suit/max(caps$Arable.suit)
caps$Igrass.suit<-caps$Igrass.suit/max(caps$Igrass.suit)
caps$SNGrass.suit<-caps$SNGrass.suit/max(caps$SNGrass.suit)
caps$NNBroadleaf.suit<-caps$NNBroadleaf.suit/max(caps$NNBroadleaf.suit)
caps$Nbroadleaf.suit<-caps$Nbroadleaf.suit/max(caps$Nbroadleaf.suit)
caps$Nconifer.suit<-caps$Nconifer.suit/max(caps$Nconifer.suit)
caps$NNConifer.suit<-caps$NNConifer.suit/max(caps$NNConifer.suit)
caps$AgroForestry.suit<-caps$AgroForestry.suit/max(caps$AgroForestry.suit)
caps$Bioeneergy.suit<-caps$Bioeneergy.suit/max(caps$Bioeneergy.suit)
caps$Tree.suit<-caps$Tree.suit/max(caps$Tree.suit)
caps$Financial<-caps$Financial/max(caps$Financial)
caps$Human<-caps$Human/max(caps$Human)
caps$Manufactured<-caps$Manufactured/max(caps$Manufactured)
caps$Social<-caps$Social/max(caps$Social)
write.csv(caps,file="C:/Users/brown-c/Documents/Work/CRAFTY-UK/Capitals/All_capitals_normalised.csv")

