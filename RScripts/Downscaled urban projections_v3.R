### urban downscaling
setwd("C:/Users/jmerkle/OneDrive - University of Edinburgh/SSP Quantification/UK Population/work with Bumsuk Seo")

library(raster)
library(dplyr)
library(ggplot2)
library(rgdal)
library(rgeos)
library(spatstat) # density map

# recategorisation of areas with the following values:
# 1 for urban areas
# 0 for non-urban areas which can become urban (SSP specific)
# -1 for non-urban areas which cannot become urban (SSP specific)

### create baseline map

# import LCM 2015 data
lcm <- raster("LCM2015_UK_1km_dominated.tif")


# check all values
lcm%>%getValues()%>%unique()%>%sort()
freq(lcm)

# reassign values to meet our urbanisation framework
lcm[lcm==1]<-0
lcm[lcm==2]<-0
lcm[lcm==3]<-0
lcm[lcm==4]<-0
lcm[lcm==5]<-0
lcm[lcm==6]<-0
lcm[lcm==7]<-0
lcm[lcm==8]<--1
lcm[lcm==9]<-0
lcm[lcm==10]<-0
lcm[lcm==11]<--1
lcm[lcm==12]<--1
lcm[lcm==13]<--1
lcm[lcm==14]<--1
lcm[lcm==15]<--1
lcm[lcm==16]<--1
lcm[lcm==17]<--1
lcm[lcm==18]<--1
lcm[lcm==19]<--1
lcm[lcm==20]<-1
lcm[lcm==21]<-1

# check values
lcm%>%getValues()%>%unique()
freq(lcm)


#### import PA data
pa<-raster("ProtectedAreas_UK_PA.tif")
# check if CRS, res & extent are identical
compareRaster(lcm,pa)
# check values
pa%>%getValues()%>%unique()%>%sort()
freq(pa)
#I set the sea to be NA (as it is in the lcm raster)
pa_unmasked<-pa
pa<-mask(pa_unmasked,lcm,inverse=FALSE,maskvalue=NA,updatevalue=NA)
remove(pa_unmasked)


### import AONB data
aonb<-raster("AONB_UK_PA.tif")
# check if CRS, res & extent are identical
compareRaster(lcm,aonb)
# check values
aonb%>%getValues()%>%unique()%>%sort()
freq(aonb)
#I set the sea to be NA (as it is in the lcm raster)
aonb_unmasked<-aonb
aonb<-mask(aonb_unmasked,lcm,inverse=FALSE,maskvalue=NA,updatevalue=NA)
remove(aonb_unmasked)


### import National Parks data
np<-raster("NationalParks_UK_PA.tif")
# check if CRS, res & extent are identical
compareRaster(lcm,np)
# check values
np%>%getValues()%>%unique()%>%sort()
freq(np)
#I set the sea to be NA (as it is in the lcm raster)
np_unmasked<-np
np<-mask(np_unmasked,lcm,inverse=FALSE,maskvalue=NA,updatevalue=NA)
remove(np_unmasked)


### import NNR data
nnr<-raster("NNR_UK_PA.tif")
# check if CRS, res & extent are identical
compareRaster(lcm,nnr)
# check values
nnr%>%getValues()%>%unique()%>%sort()
freq(nnr)
#I set the sea to be NA (as it is in the lcm raster)
nnr_unmasked<-nnr
nnr<-mask(nnr_unmasked,lcm,inverse=FALSE,maskvalue=NA,updatevalue=NA)
remove(nnr_unmasked)


### import RAMSAR data
ramsar<-raster("RAMSAR_UK_PA.tif")
# check if CRS, res & extent are identical
compareRaster(lcm,ramsar)
# check values
ramsar%>%getValues()%>%unique()%>%sort()
freq(ramsar)
#I set the sea to be NA (as it is in the lcm raster)
ramsar_unmasked<-ramsar
ramsar<-mask(ramsar_unmasked,lcm,inverse=FALSE,maskvalue=NA,updatevalue=NA)
remove(ramsar_unmasked)


### import SAC data
sac<-raster("SAC_UK_PA.tif")
# check if CRS, res & extent are identical
compareRaster(lcm,sac)
# check values
sac%>%getValues()%>%unique()%>%sort()
freq(sac)
#I set the sea to be NA (as it is in the lcm raster)
sac_unmasked<-sac
sac<-mask(sac_unmasked,lcm,inverse=FALSE,maskvalue=NA,updatevalue=NA)
remove(sac_unmasked)


### import SPA data
spa<-raster("SPA2019_PA.tif")
# check if CRS, res & extent are identical
compareRaster(lcm,spa)
# check values
spa%>%getValues()%>%unique()%>%sort()
freq(spa)
#I set the sea to be NA (as it is in the lcm raster)
spa_unmasked<-sac
spa<-mask(spa_unmasked,lcm,inverse=FALSE,maskvalue=NA,updatevalue=NA)
remove(spa_unmasked)


### import flood risk data
fld<-raster("FloodRisk_IH130_UK_1km.tif")
# check if CRS, res & extent are identical
compareRaster(lcm,fld)
# check values
fld%>%getValues()%>%unique()%>%sort()
# change values into presence/absence
fld[fld>0]<-1
#I set the sea to be NA (as it is in the lcm raster)
fld_unmasked<-fld
fld<-mask(fld_unmasked,lcm,inverse=FALSE,maskvalue=NA,updatevalue=NA)
remove(fld_unmasked)


#### create a base stack
layernames<-c("LandCover","Protected Areas","AONB","NationalParks","NNR","RAMSAR","SAC","SPA","Flood")
base.stack<-stack(lcm,pa,aonb,np,nnr,ramsar,sac,spa,fld)
names(base.stack)<-layernames
plot(base.stack)


#### check how many urban cells are in AONB
lcm.u<-lcm
lcm.u[lcm.u<1]<-NA
freq(mask(lcm.u,base.stack$AONB,maskvalue=1,updatevalue=2),value=2) # yields 200
#check how many urban cells are in protected areas
freq(mask(lcm.u,base.stack$Protected.Areas,maskvalue=1,updatevalue=2),value=2)# yields 501
# other tests like this can be carried out...


#### create baseline surface with protected areas
totalprot<-overlay(pa,aonb,np,nnr,ramsar,sac,spa,fun=sum)
plot(totalprot)
totalprot[totalprot>0]<-10
freq(totalprot)

# first case (SSP1, protected areas and flood areas cannot be urbanised)
fld[fld==1]<-100
m1<-overlay(lcm,totalprot,fld,fun=sum)
freq(m1)

m1[m1==-1]<--1 # non-potential is non-potential
m1[m1==0]<-0 # potential is potential
m1[m1==1]<-1 # urban is urban
m1[m1==9]<--1 # non-potential and protected is non-potential
m1[m1==10]<--1 # potential and protected is non-potential
m1[m1==11]<-1 # urban and protected is urban
m1[m1==99]<--1 # non-potential and flood is non-potential
m1[m1==100]<--1 # potential and flood is non-potential
m1[m1==101]<-1 # urban and flood is urban
m1[m1==109]<--1 # non-potential, protected, and flood is non-potential
m1[m1==110]<--1 # potential, protected, and flood is non-potential  
m1[m1==111]<-1 # urban, protected, and flood is urban

freq(m1)

#second case (SSP2, protected areas cannot be urbanised)
m2<-overlay(lcm,totalprot,fun=sum)
freq(m2)

m2[m2==11]<-1 # urban and protected is urban
m2[m2==1]<-1 # urban and unprotected is urban
m2[m2==0]<-0 # potential and unprotected is potential
m2[m2==10]<--1 # potential and protected is non-potential
m2[m2==9]<--1 # non-potential and protected is non-potential
m2[m2==-1]<--1 # non-potential and unprotected is non-potential

freq(m2)

#third case (protected areas and flood areas can be urbanised, SSP3,4,5)

m3<-lcm # no exclusion, apart from uninhabitable areas

freq(m3)

# stack the three layers together
Baseline<-stack(m1,m2,m3,m3,m3)
names(Baseline)<-c("SSP1 Baseline","SSP2 Baseline","SSP3 Baseline","SSP4 Baseline","SSP5 Baseline")
plot(Baseline,col=c("red","green","black"),axes=FALSE,legend=FALSE)


#clean up
remove(layernames,aonb,base.stack,lcm,lcm.u,nnr,np,pa,ramsar,sac,spa,fld,totalprot,m1,m2,m3)


### Add RUG output
setwd("C:/Users/jmerkle/OneDrive - University of Edinburgh/SSP Quantification/UK Population/Downscaling with dIAP/dIAP outputs/SSP1")


rug.s1.20<-read.csv("rug_CELLAreaOutput_2020_SSP1.csv")
rug.s1.30<-read.csv("rug_CELLAreaOutput_2030_SSP1.csv")

rug.s1.20$all<-rug.s1.20[,2]+rug.s1.20[,3]+rug.s1.20[,4]+rug.s1.20[,5]
rug.s1.30$all<-rug.s1.30[,2]+rug.s1.30[,3]+rug.s1.30[,4]+rug.s1.30[,5]



rug.s1.20.30<-rug.s1.30%>%select(CELL_ID,all)
rug.s1.20.30$all<-(rug.s1.30$all-rug.s1.20$all)/rug.s1.20$all
plot(rug.s1.20.30$all)
sort(rug.s1.20.30$all,decreasing=T) # this highlights a problem. What do we do when the initial value is 0 (urban grows inf)? Will have to address this later


#### Cell ID and coordinates 
setwd("C:/Users/jmerkle/OneDrive - University of Edinburgh/SSP Quantification/UK Population/work with Bumsuk Seo")

ctry.ids <- read.csv("Cell_ID_LatLong.csv")

#### function to make a map out of the table

raster_rug_UK<-function(rug,coord,b){ # two column rug table (CELL ID and Value), a table with cell ids and lat/lon info, the baseline map
  CELL_ID_matched <- match(rug[,1],coord[,1])
  rug[,3]<-coord[,2][CELL_ID_matched] # adds latitude column
  rug[,4]<-coord[,3][CELL_ID_matched] # adds longitude column
  rug<-rug[!is.na(rug[,3]),] # throws out all cells that we have no spatial info for
  proj4.LL <- CRS("+proj=longlat +datum=WGS84")
  rug_spdf <- SpatialPixelsDataFrame(points = SpatialPoints(cbind(rug[,4], rug[,3]), proj4string = proj4.LL), data = data.frame(rug), tolerance = 0.0011)
  rs_LL<-stack(rug_spdf) # raster stack
  rs_UK<-projectRaster(rs_LL, crs=crs(b), method="ngb", res=10000)# adjust CRS and 10 km resolution
  rs_UK<-crop(rs_UK,b) # crop out UK area
  xmin(rs_UK)<-xmin(b) # align extents
  xmax(rs_UK)<-xmax(b)
  ymin(rs_UK)<-ymin(b)
  ymax(rs_UK)<-ymax(b)
  rs_UK.disagg<-disaggregate(rs_UK,fact=10) # align resolution
  rs_UK.disagg<-mask(rs_UK.disagg,Baseline[[1]],inverse=FALSE,maskvalue=NA,updatevalue=NA) # crop out Republic Ireland
  rs_UK<-aggregate(rs_UK.disagg,fact=10) # then back to old resolution
}



trast<-raster_rug_UK(rug.s1.20.30,ctry.ids,Baseline[[1]])
plot(trast)



### Create a map of identifying cells
rug.id<-trast$CELL_ID

values(rug.id)<-seq(1,length(rug.id),by=1) # now every cell has a unique identifier
rug.id[is.nan(trast$CELL_ID)]<-NA # we crop out all the sea area again
plot(rug.id)

new.id<-resample(rug.id,Baseline[[1]],method="ngb") # create a map of identifiers of RUG cells with resolution of our Baseline maps
new.id[is.na(Baseline[[1]])]<-NA # exclude sea again
plot(new.id)

length(unique(values(new.id)))==length(unique(values(rug.id))) # test to check if all cells are there

#clean up
remove(ctry.ids)


#### now the urbanisation process

#Step 1: Urban Raster
Baseline.u<-Baseline
for(i in 1:nlayers(Baseline)){
  Baseline.u[[i]][Baseline.u[[i]]<1]<-NA
}
plot(Baseline.u)

#Step 2: Distance to Urban Raster (this takes a few minutes)
for (i in 1:nlayers(Baseline)){
  Baseline.u[[i]]<-distance(Baseline.u[[i]],doEdge=FALSE)
  Baseline.u[[i]][Baseline[[i]]==-1]<-NA # exclusion of cells that cannot be populated
}
plot(Baseline.u)

#Step 3:  Standardise distances to neighbourhood boundary
nh.bound<-10000 #10km
for (i in 1:nlayers(Baseline)){
  Baseline.u[[i]][Baseline.u[[i]]>nh.bound]<-NA # exclude cells that are too far away from urban areas (outside of applicability of the neighbourhood function)
  Baseline.u[[i]]<-1-(Baseline.u[[i]]/nh.bound) # reverse and standardise between 0 and 1
}
plot(Baseline.u)

#Step 4: Weight the distances with neighbourhood function

#neighbourhood function is derrived from Kumaraswamy Distr.
# we assume exogenous alpha and mode.
# The mode is the peak of the function, i.e. the highest likelihood of urbanisation.
# Beta is endogenised with a term that yields beta in dependence of alpha and the mode. 

kumar<-function(x,alpha,mod){
  y<-(alpha*(((mod^-alpha)*((mod^alpha)+alpha-1))/alpha)*
        (x^(alpha-1))*((1-(x^alpha))^
                         ((((mod^-alpha)*((mod^alpha)+alpha-1))/alpha)-1)))
  return(y)
}
a<-3 # Alpha parameter (free to choose)
sspmod<-c(0.8,0.75,0.7,0.9,0.65)# a vector of modes per SSP (free to choose)


# Apply neighbourhood function and show intermediate result
Baseline.w<-Baseline.u
for (i in 1:nlayers(Baseline)){
  Baseline.w[[i]]<-kumar(Baseline.w[[i]],a,sspmod[i])
  Baseline.w[[i]]<-Baseline.w[[i]]/(sort(unique(values(Baseline.w[[i]])),decreasing=TRUE)[1]) # to standardise between 0 and 1
  Baseline.w[[i]][Baseline[[i]]==1]<-1.5 # to see where current urban areas are
  Baseline.w[[i]][Baseline[[i]]==-1]<--1 # to include excluded areas
  Baseline.w[[i]][is.na(Baseline.w[[i]])]<-0 # to reinclude areas outside of the neighbourhood boundary
  Baseline.w[[i]]<-mask(Baseline.w[[i]],Baseline[[i]],inverse=FALSE,maskvalue=NA,updatevalue=NA)
  names(Baseline.w[[i]])<-paste("SSP",i,"Alpha",a,"Mode",(1-sspmod[i])*10,"km from u","NH Bound",nh.bound/1000,"km from u")
}
plot(Baseline.w,col=c("green4","green3","green","greenyellow","yellow","orange","orangered","red","white","black"),
     breaks=c(-1,-0.1,0.4,0.5,0.6,0.7,0.8,0.9,1,1.4,1.6),axes=FALSE,legend=FALSE)

# to continue work
for (i in 1:nlayers(Baseline)){
  Baseline.w[[i]][Baseline.w[[i]]==1.5]<-NA # urban areas should not urbanise
  Baseline.w[[i]][Baseline[[i]]==-1]<-NA # excluded areas should not urbanise
}


# Patch-wise function to calculate new urban surface
new.urb<-Baseline[[1]]
new.urb[]<-0


urbanise<-function(b.id,b,r,w){
  num<-vector(mode="numeric",length = length(r)) # needed for the loop
  rnk<-vector(mode = "list", length = length(r)) # needed for the loop
  for(i in min(unique(new.id)):max(unique(new.id))){ # for the whole UK area
    num[i]<-round((length(b[b.id==i & b==1])*(r[i])),digits=0) # in each patch calculate how many cells will be turned to u
    set.seed(2020)
    if((is.na(num[i])==F) & (num[i]>0)){rnk[[i]]<-rank((-w[b.id==i]) , ties.method="random" , na.last=T)} # in each patch rank cells in descending order
    if((is.na(num[i])==F) & (num[i]>0)){new.urb[b.id==i][which(rnk[[i]] %in% 1:num[i])]<-10} # in each patch take the highest likelihood cells and make them urban
  }
  new<-new.urb+b
  return(new)
}

check<-function(new.map){
  u.in.n<-length(new.map[new.map==9])
  u.in.u<-length(new.map[new.map==11])
  miss<-round(100*((length(new.map[new.map==11 | new.map==9]))/length(new.map[new.map>1])),digits=1)
  check<-paste(miss, "% of new urban pixels could not be correctly allocated.",u.in.n,"u pixels created in excluded areas and",u.in.u,"u pixels created which were already u before.")
  return(check)
}

correct<-function(new.map){
  new.map[new.map==10]<-1 # this is to assign the standard urban value as in the baseline
  new.map[new.map==11]<-1 # this is to correct for areas which were already fully urbanised
  new.map[new.map==9]<--1 # this is to correct for areas were excluded
  return(new.map)
}



# now we try it with the illustrative RUG output

ex.rug<-trast$all
max(unique(ex.rug))


ex.rug[is.nan(ex.rug)]<-NA
plot(ex.rug)
values(ex.rug)



test1<-urbanise(new.id,Baseline[[1]],ex.rug,Baseline.w[[1]])
check(test1)
test1.corr<-correct(test1)

plot(test1)
freq(test1)
plot(test1.corr,col=c("red","green","black"),axes=FALSE,legend=FALSE)

plot(Baseline[[1]],col=c("red","green","black"),axes=FALSE,legend=FALSE)

freq(Baseline[[1]])
freq(test1.corr)




