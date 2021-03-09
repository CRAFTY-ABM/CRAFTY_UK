##### UK SSP Urbanisation ######################################################

# This Script is made to model urbanisation in the UK in five SSP scenarios. 
# It uses dIAP RUG outputs and downscales them to 1km resolution

# Contents:
# 1. Create Baseline Surfaces from LCM, Protected Areas and Flood Risk Areas
# 2. Create Artificial Surface Change Surfaces from dIAP RUG
# 3. Create Cell ID Maps to Translate Different Resolutions
# 4. Define Functions for Urban Downscaling and Checks
# 5. Apply Functions for each time step

################################################################################
# 1. Create Baseline Surfaces ##################################################

wd<-"C:/Users/jmerkle/OneDrive - University of Edinburgh/SSP Quantification/UK Population/work with Bumsuk Seo"
setwd(wd)

library(raster)
library(dplyr)
library(ggplot2)
library(rgdal)
library(rgeos)
library(spatstat) # density map

##### Step 1a: Import data and recategorisation of areas

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


#### Step 1b: Create a base stack and analyse

layernames<-c("LandCover","Protected Areas","AONB","NationalParks","NNR","RAMSAR","SAC","SPA","Flood")
base.stack<-stack(lcm,pa,aonb,np,nnr,ramsar,sac,spa,fld)
names(base.stack)<-layernames
plot(base.stack)


# how many urban cells are in AONB?
lcm.u<-lcm
lcm.u[lcm.u<1]<-NA
freq(mask(lcm.u,base.stack$AONB,maskvalue=1,updatevalue=2),value=2) # yields 200
#how many urban cells are in protected areas
freq(mask(lcm.u,base.stack$Protected.Areas,maskvalue=1,updatevalue=2),value=2)# yields 501
# other tests like this can be carried out...


#### Step 1c: Create baseline surfaces

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


################################################################################
# 2. Create Change Surfaces from dIAP RUG ######################################

#### Step 2a: Import data function

import_rug_data<-function(wd.rug,wd.main){ # directories of rug data, main working directory
  setwd(wd.rug)                        # specify where data comes from
  rug.ssp<-c(                          # file names
    "rug_results_2010-2020.csv",
    "rug_results_2020-2030.csv",
    "rug_results_2030-2040.csv",
    "rug_results_2040-2050.csv",
    "rug_results_2050-2060.csv",
    "rug_results_2060-2070.csv",
    "rug_results_2070-2080.csv",
    "rug_results_2080-2090.csv",
    "rug_results_2090-2100.csv")
  rug.ssp <- lapply(rug.ssp, read.csv) # read.csv and create list of dataframes
  for (i in 1:length(rug.ssp)){        # delete obsolete columns and rename value column
    rug.ssp[[i]]<-rug.ssp[[i]]%>%select(CELL_ID,X....AS.Total)%>%rename(AS = X....AS.Total)
  }
  for(i in 1:length(rug.ssp)){         # standardise data into values between 0 and 1
    rug.ssp[[i]][,2]<-rug.ssp[[i]][,2]/100
  }
  d.rug.ssp<-rug.ssp[-9]
  for (i in 1:length(d.rug.ssp)){      # change data into absolute change between time steps
    d.rug.ssp[[i]][,2]<-rug.ssp[[i+1]][,2]-rug.ssp[[i]][,2]
  }
  setwd(wd.main)                       # back to old directory
  return(d.rug.ssp)
}

#### Step 2b: Apply import function

# Caution! dIAP RUG output files are faulty
# there is an empty cell in each 18th column without header.
# this messes up the commas in the csv file and leads to incorrect data imports
# make sure to delete 18th column in each csv file in Excel before importing

wd1<-("C:/Users/jmerkle/OneDrive - University of Edinburgh/SSP Quantification/UK Population/Downscaling with dIAP/dIAP outputs/SSP1")
wd3<-("C:/Users/jmerkle/OneDrive - University of Edinburgh/SSP Quantification/UK Population/Downscaling with dIAP/dIAP outputs/SSP3")
wd4<-("C:/Users/jmerkle/OneDrive - University of Edinburgh/SSP Quantification/UK Population/Downscaling with dIAP/dIAP outputs/SSP4")
wd5<-("C:/Users/jmerkle/OneDrive - University of Edinburgh/SSP Quantification/UK Population/Downscaling with dIAP/dIAP outputs/SSP5")

rug.ssp1<-import_rug_data(wd1,wd)
rug.ssp3<-import_rug_data(wd3,wd)
rug.ssp4<-import_rug_data(wd4,wd)
rug.ssp5<-import_rug_data(wd5,wd)

remove(wd1,wd3,wd4,wd5,import_rug_data) # clean up

#create SSP2 (interpolation between SSP1 and SSP3)
rug.ssp2<-rug.ssp3
for (i in 1:length(rug.ssp2)){
  rug.ssp2[[i]][,2]<-((rug.ssp1[[i]][,2])+(rug.ssp3[[i]][,2]))/2
}


#### Step 2c: Create functions to transform tables into maps 

# function to make a map out of the table
raster_rug_UK<-function(rug,coord,b){ # two column rug table (CELL ID and Value), a table with cell ids and lat/lon info, the baseline map
  CELL_ID_matched <- match(rug[,1],coord[,1])
  rug[,3]<-coord[,2][CELL_ID_matched] # adds latitude column
  rug[,4]<-coord[,3][CELL_ID_matched] # adds longitude column
  rug<-rug[!is.na(rug[,3]),] # throws out all cells that we have no spatial info for
  proj4.LL <- CRS("+proj=longlat +datum=WGS84")
  rug_spdf <- SpatialPixelsDataFrame(points = SpatialPoints(cbind(rug[,4], rug[,3]), proj4string = proj4.LL), data = data.frame(rug), tolerance = 0.0011)
  rs_LL<-stack(rug_spdf) # raster stack
  rs_UK<-projectRaster(rs_LL, crs=crs(b), method="bilinear", res=10000)# adjust CRS and 10 km resolution
  rs_UK<-crop(rs_UK,b) # crop out UK area
  xmin(rs_UK)<-xmin(b) # align extents
  xmax(rs_UK)<-xmax(b)
  ymin(rs_UK)<-ymin(b)
  ymax(rs_UK)<-ymax(b)
  rs_UK.disagg<-disaggregate(rs_UK,fact=10) # align resolution
  rs_UK.disagg<-mask(rs_UK.disagg,Baseline[[1]],inverse=FALSE,maskvalue=NA,updatevalue=NA) # crop out Republic Ireland
  rs_UK<-aggregate(rs_UK.disagg,fact=10) # then back to old resolution
  rs_UK<-dropLayer(rs_UK,c(1,3,4))
  return(rs_UK)
}


# apply function one by one for our eight time steps
make_stack<-function(rug,coord,b){
  drast.1<-raster_rug_UK(rug[[1]],coord,b)
  drast.2<-raster_rug_UK(rug[[2]],coord,b)
  drast.3<-raster_rug_UK(rug[[3]],coord,b)
  drast.4<-raster_rug_UK(rug[[4]],coord,b)
  drast.5<-raster_rug_UK(rug[[5]],coord,b)
  drast.6<-raster_rug_UK(rug[[6]],coord,b)
  drast.7<-raster_rug_UK(rug[[7]],coord,b)
  drast.8<-raster_rug_UK(rug[[8]],coord,b)
  drast<-stack(drast.1,drast.2,drast.3,drast.4,drast.5,drast.6,drast.7,drast.8)
  return(drast)
}


#### Step 2d: Apply functions, reformat, and check plots

#Cell ID and coordinates 
ctry.ids <- read.csv("Cell_ID_LatLong.csv")

# now use the functions
rug.ssp1<-make_stack(rug.ssp1,ctry.ids,Baseline[[1]])
rug.ssp2<-make_stack(rug.ssp2,ctry.ids,Baseline[[2]])
rug.ssp3<-make_stack(rug.ssp3,ctry.ids,Baseline[[3]])
rug.ssp4<-make_stack(rug.ssp4,ctry.ids,Baseline[[4]])
rug.ssp5<-make_stack(rug.ssp5,ctry.ids,Baseline[[5]])


# format according to time steps
rug.ssp.t1<-stack(rug.ssp1[[1]],rug.ssp2[[1]],rug.ssp3[[1]],rug.ssp4[[1]],rug.ssp5[[1]])
rug.ssp.t2<-stack(rug.ssp1[[2]],rug.ssp2[[2]],rug.ssp3[[2]],rug.ssp4[[2]],rug.ssp5[[2]])
rug.ssp.t3<-stack(rug.ssp1[[3]],rug.ssp2[[3]],rug.ssp3[[3]],rug.ssp4[[3]],rug.ssp5[[3]])
rug.ssp.t4<-stack(rug.ssp1[[4]],rug.ssp2[[4]],rug.ssp3[[4]],rug.ssp4[[4]],rug.ssp5[[4]])
rug.ssp.t5<-stack(rug.ssp1[[5]],rug.ssp2[[5]],rug.ssp3[[5]],rug.ssp4[[5]],rug.ssp5[[5]])
rug.ssp.t6<-stack(rug.ssp1[[6]],rug.ssp2[[6]],rug.ssp3[[6]],rug.ssp4[[6]],rug.ssp5[[6]])
rug.ssp.t7<-stack(rug.ssp1[[7]],rug.ssp2[[7]],rug.ssp3[[7]],rug.ssp4[[7]],rug.ssp5[[7]])
rug.ssp.t8<-stack(rug.ssp1[[8]],rug.ssp2[[8]],rug.ssp3[[8]],rug.ssp4[[8]],rug.ssp5[[8]])


# plot to check
plot(rug.ssp.t1,
     zlim=c(0,max(max(unique(rug.ssp.t1[[1]])),max(unique(rug.ssp.t1[[2]])),
                  max(unique(rug.ssp.t1[[3]])),max(unique(rug.ssp.t1[[4]])),
                  max(unique(rug.ssp.t1[[5]])))))
plot(rug.ssp.t2,
     zlim=c(0,max(max(unique(rug.ssp.t2[[1]])),max(unique(rug.ssp.t2[[2]])),
                  max(unique(rug.ssp.t2[[3]])),max(unique(rug.ssp.t2[[4]])),
                  max(unique(rug.ssp.t2[[5]])))))
plot(rug.ssp.t3,
     zlim=c(0,max(max(unique(rug.ssp.t3[[1]])),max(unique(rug.ssp.t3[[2]])),
                  max(unique(rug.ssp.t3[[3]])),max(unique(rug.ssp.t3[[4]])),
                  max(unique(rug.ssp.t3[[5]])))))
plot(rug.ssp.t4,
     zlim=c(0,max(max(unique(rug.ssp.t4[[1]])),max(unique(rug.ssp.t4[[2]])),
                  max(unique(rug.ssp.t4[[3]])),max(unique(rug.ssp.t4[[4]])),
                  max(unique(rug.ssp.t4[[5]])))))
plot(rug.ssp.t5,
     zlim=c(0,max(max(unique(rug.ssp.t5[[1]])),max(unique(rug.ssp.t5[[2]])),
                  max(unique(rug.ssp.t5[[3]])),max(unique(rug.ssp.t5[[4]])),
                  max(unique(rug.ssp.t5[[5]])))))
plot(rug.ssp.t6,
     zlim=c(0,max(max(unique(rug.ssp.t6[[1]])),max(unique(rug.ssp.t6[[2]])),
                  max(unique(rug.ssp.t6[[3]])),max(unique(rug.ssp.t6[[4]])),
                  max(unique(rug.ssp.t6[[5]])))))
plot(rug.ssp.t7,
     zlim=c(0,max(max(unique(rug.ssp.t7[[1]])),max(unique(rug.ssp.t7[[2]])),
                  max(unique(rug.ssp.t7[[3]])),max(unique(rug.ssp.t7[[4]])),
                  max(unique(rug.ssp.t7[[5]])))))
plot(rug.ssp.t8,
     zlim=c(0,max(max(unique(rug.ssp.t8[[1]])),max(unique(rug.ssp.t8[[2]])),
                  max(unique(rug.ssp.t8[[3]])),max(unique(rug.ssp.t8[[4]])),
                  max(unique(rug.ssp.t8[[5]])))))


# clean up
remove(ctry.ids,make_stack,raster_rug_UK)
remove(rug.ssp1,rug.ssp2,rug.ssp3,rug.ssp4,rug.ssp5)


################################################################################
# 3. Create Cell ID Maps #######################################################

### Maps of identifying cells
rug.id<-rug.ssp.t1[[1]]
values(rug.id)<-seq(1,length(rug.id),by=1) # now every cell has a unique identifier
rug.id[is.nan(rug.ssp.t1[[1]])]<-NA # we crop out all the sea area again
plot(rug.id)

new.id<-resample(rug.id,Baseline[[1]],method="ngb") # create a map of identifiers of RUG cells with resolution of our Baseline maps
new.id[is.na(Baseline[[1]])]<-NA # exclude sea again
plot(new.id)

length(unique(values(new.id)))==length(unique(values(rug.id))) # test to check if all cells are there


################################################################################
# 4. Create Downscaling and Check Functions ####################################


#### Step 4a: Neighbourhood function

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
nh.bound<-10000 # 10km neighbourhood bound


#### Step 4b: Weighting function using the neighbourhood function
weighting_function<-function(b){   # reads the baseline maps and weights them to get transition probability maps
  # Urban raster
  b.u<-b
  for(i in 1:nlayers(b)){
    b.u[[i]][b.u[[i]]<1]<-NA
  }
  # Distance to urban raster
  for (i in 1:nlayers(b)){
    b.u[[i]]<-distance(b.u[[i]],doEdge=FALSE)
    b.u[[i]][b[[i]]==-1]<-NA # exclusion of cells that cannot be populated
  }
  # Standardise distance
  for (i in 1:nlayers(b)){
    b.u[[i]][b.u[[i]]>nh.bound]<-NA # exclude cells that are too far away from urban areas (outside of applicability of the neighbourhood function)
    b.u[[i]]<-1-(b.u[[i]]/nh.bound) # reverse and standardise between 0 and 1
  }
  # Weight distance with neighbourhood function from step 4a
  b.w<-b.u
  for (i in 1:nlayers(b)){
    b.w[[i]]<-kumar(b.w[[i]],a,sspmod[i])
    b.w[[i]]<-b.w[[i]]/(sort(unique(values(b.w[[i]])),decreasing=TRUE)[1]) # to standardise between 0 and 1
    b.w[[i]][b[[i]]==1]<-1.5 # to see where current urban areas are
    b.w[[i]][b[[i]]==-1]<--1 # to include excluded areas
    b.w[[i]][is.na(b.w[[i]])]<-0 # to reinclude areas outside of the neighbourhood boundary
    b.w[[i]]<-mask(b.w[[i]],b[[i]],inverse=FALSE,maskvalue=NA,updatevalue=NA)
    names(b.w[[i]])<-paste("SSP",i,"Alpha",a,"Mode",(1-sspmod[i])*10,"km from u","NH Bound",nh.bound/1000,"km from u")
  }
  return(b.w)
}


#### Step 4c: Urbanisation function using a patch-wise loop

urbanisation<-function(b.id,b,r,b.w){
  new.urb<-b
  new.urb[]<-0 # just to create an empty layer
  num<-vector(mode="numeric",length = length(r)) # needed for the loop
  rnk<-vector(mode = "list", length = length(r)) # needed for the loop
  for(i in min(unique(new.id)):max(unique(new.id))){ # for the whole UK area
    num[i]<-round((length(b[b.id==i])*(r[i])),digits=0) # in each patch calculate how many cells will be turned to u
    set.seed(2020)
    if((is.na(num[i])==F) & (num[i]>0)){rnk[[i]]<-rank((-b.w[b.id==i]) , ties.method="random" , na.last=T)} # in each patch rank cells in descending order
    if((is.na(num[i])==F) & (num[i]>0)){new.urb[b.id==i][which(rnk[[i]] %in% 1:num[i])]<-10} # in each patch take the highest likelihood cells and make them urban
  }
  new<-new.urb+b
  return(new)
}



#### Step 4d: Function to apply urbanisation
urb_apply<-function(b.id,b,r,b.w){
  b.n<-b
  for (i in 1:nlayers(b)){
    b.w[[i]][b.w[[i]]==1.5]<-NA # exclude urban areas
    b.w[[i]][b[[i]]==-1]<-NA # exclude excluded areas
    b.n[[i]][]<-0 # empty stack
    b.n[[i]]<-urbanisation(b.id,b[[i]],r[[i]],b.w[[i]]) # apply urbanisation function from step 4c
  }
  return(b.n)
}



#### Step 4e: Function to check incorrect allocations of new urban areas
check_newurb<-function(b.n,b){
  check<-vector(length=nlayers(b))
  u.in.n<-c(rep(0,nlayers(b)))
  n.id<-vector(mode = "list", length = nlayers(b))
  u.in.u<-c(rep(0,nlayers(b)))
  u.id<-vector(mode = "list", length = nlayers(b))
  total<-c(rep(0,nlayers(b)))
  miss<-c(rep(0,nlayers(b)))
  check<-vector(length=nlayers(b))
  for(i in 1:nlayers(b)){
    u.in.n[i]<-length(b.n[[i]][b.n[[i]]==9])
    n.id[[i]]<-as.vector(new.id[b.n[[i]]==9])
    u.in.u[i]<-length(b.n[[i]][b.n[[i]]==11])
    u.id[[i]]<-as.vector(new.id[b.n[[i]]==11])
    total[i]<-length(b.n[[i]][b.n[[i]]==11 | b.n[[i]]==10 | b.n[[i]]==9])
    miss[i]<-round(100*((length(b.n[[i]][b.n[[i]]==11 | b.n[[i]]==9]))/length(b.n[[i]][b.n[[i]]>1])),digits=1)
    check[i]<-paste(total[i]," new urban pixels created. ",
                 miss[i], "% of these pixels could not be correctly allocated: ",
                 u.in.n[i]," urban pixels created on excluded pixels (patch ID ",n.id[i],") and ",
                 u.in.u[i]," urban pixels created on already urban pixels (patch ID ",u.id[i],").",
                 sep="")
  }
  return(check)
}



#### Step 4f: Function to assign the standard categories again and correct incorrect allocations
correct_newurb<-function(b.n){
  for(i in 1:nlayers(b.n)){
    b.n[[i]][b.n[[i]]==10]<-1 
    b.n[[i]][b.n[[i]]==11]<-1 
    b.n[[i]][b.n[[i]]==9]<--1 
  }
  return(b.n)
}



################################################################################
# 5. Apply Downscaling Functions ###############################################



#### Step 5a: First time step (2020 - 2030)

Baseline.w<-weighting_function(Baseline)
plot(Baseline.w,col=c("green4","green3","green","greenyellow","yellow","orange","orangered","red","white","black"),
     breaks=c(-1,-0.1,0.4,0.5,0.6,0.7,0.8,0.9,1,1.4,1.6),axes=FALSE)

Baseline.n<-urb_apply(new.id,Baseline,rug.ssp.t1,Baseline.w)
notes.n<-check_newurb(Baseline.n,Baseline)
notes.n
Baseline.n<-correct_newurb(Baseline.n)

names(Baseline.n)<-c("SSP1 2030","SSP2 2030","SSP3 2030","SSP4 2030","SSP5 2030")
plot(Baseline.n,col=c("red","green","black"),axes=T,legend=FALSE)

# Rename so that we can continue with next time step
Original.Baseline<-Baseline
result.2030<-Baseline.n
notes.2030<-notes.n
# clean up
remove (Baseline,Baseline.w,Baseline.n,notes.n)



#### Step 5b: Second time step (2030 - 2040)

Baseline<-result.2030

Baseline.w<-weighting_function(Baseline)
plot(Baseline.w,col=c("green4","green3","green","greenyellow","yellow","orange","orangered","red","white","black"),
     breaks=c(-1,-0.1,0.4,0.5,0.6,0.7,0.8,0.9,1,1.4,1.6),axes=FALSE)

Baseline.n<-urb_apply(new.id,Baseline,rug.ssp.t2,Baseline.w)
notes.n<-check_newurb(Baseline.n,Baseline)
notes.n
Baseline.n<-correct_newurb(Baseline.n)

names(Baseline.n)<-c("SSP1 2040","SSP2 2040","SSP3 2040","SSP4 2040","SSP5 2040")
plot(Baseline.n,col=c("red","green","black"),axes=T,legend=FALSE)

# Rename so that we can continue with next time step
result.2040<-Baseline.n
notes.2040<-notes.n
# clean up
remove (Baseline,Baseline.w,Baseline.n,notes.n)



#### Step 5c: Third time step (2040 - 2050)

Baseline<-result.2040

Baseline.w<-weighting_function(Baseline)
plot(Baseline.w,col=c("green4","green3","green","greenyellow","yellow","orange","orangered","red","white","black"),
     breaks=c(-1,-0.1,0.4,0.5,0.6,0.7,0.8,0.9,1,1.4,1.6),axes=FALSE)

Baseline.n<-urb_apply(new.id,Baseline,rug.ssp.t3,Baseline.w)
notes.n<-check_newurb(Baseline.n,Baseline)
notes.n
Baseline.n<-correct_newurb(Baseline.n)

names(Baseline.n)<-c("SSP1 2050","SSP2 2050","SSP3 2050","SSP4 2050","SSP5 2050")
plot(Baseline.n,col=c("red","green","black"),axes=T,legend=FALSE)

# Rename so that we can continue with next time step
result.2050<-Baseline.n
notes.2050<-notes.n
# clean up
remove (Baseline,Baseline.w,Baseline.n,notes.n)



#### Step 5d: Fourth time step (2050 - 2060)

Baseline<-result.2050

Baseline.w<-weighting_function(Baseline)
plot(Baseline.w,col=c("green4","green3","green","greenyellow","yellow","orange","orangered","red","white","black"),
     breaks=c(-1,-0.1,0.4,0.5,0.6,0.7,0.8,0.9,1,1.4,1.6),axes=FALSE)

Baseline.n<-urb_apply(new.id,Baseline,rug.ssp.t4,Baseline.w)
notes.n<-check_newurb(Baseline.n,Baseline)
notes.n
Baseline.n<-correct_newurb(Baseline.n)

names(Baseline.n)<-c("SSP1 2060","SSP2 2060","SSP3 2060","SSP4 2060","SSP5 2060")
plot(Baseline.n,col=c("red","green","black"),axes=T,legend=FALSE)

# Rename so that we can continue with next time step
result.2060<-Baseline.n
notes.2060<-notes.n
# clean up
remove (Baseline,Baseline.w,Baseline.n,notes.n)



#### Step 5e: Fifth time step (2060 - 2070)

Baseline<-result.2060

Baseline.w<-weighting_function(Baseline)
plot(Baseline.w,col=c("green4","green3","green","greenyellow","yellow","orange","orangered","red","white","black"),
     breaks=c(-1,-0.1,0.4,0.5,0.6,0.7,0.8,0.9,1,1.4,1.6),axes=FALSE)

Baseline.n<-urb_apply(new.id,Baseline,rug.ssp.t5,Baseline.w)
notes.n<-check_newurb(Baseline.n,Baseline)
notes.n
Baseline.n<-correct_newurb(Baseline.n)

names(Baseline.n)<-c("SSP1 2070","SSP2 2070","SSP3 2070","SSP4 2070","SSP5 2070")
plot(Baseline.n,col=c("red","green","black"),axes=T,legend=FALSE)

# Rename so that we can continue with next time step
result.2070<-Baseline.n
notes.2070<-notes.n
# clean up
remove (Baseline,Baseline.w,Baseline.n,notes.n)



#### Step 5f: Sixth time step (2070 - 2080)

Baseline<-result.2070

Baseline.w<-weighting_function(Baseline)
plot(Baseline.w,col=c("green4","green3","green","greenyellow","yellow","orange","orangered","red","white","black"),
     breaks=c(-1,-0.1,0.4,0.5,0.6,0.7,0.8,0.9,1,1.4,1.6),axes=FALSE)

Baseline.n<-urb_apply(new.id,Baseline,rug.ssp.t6,Baseline.w)
notes.n<-check_newurb(Baseline.n,Baseline)
notes.n
Baseline.n<-correct_newurb(Baseline.n)

names(Baseline.n)<-c("SSP1 2080","SSP2 2080","SSP3 2080","SSP4 2080","SSP5 2080")
plot(Baseline.n,col=c("red","green","black"),axes=T,legend=FALSE)

# Rename so that we can continue with next time step
result.2080<-Baseline.n
notes.2080<-notes.n
# clean up
remove (Baseline,Baseline.w,Baseline.n,notes.n)



#### Step 5g: Seventh time step (2080 - 2090)

Baseline<-result.2080

Baseline.w<-weighting_function(Baseline)
plot(Baseline.w,col=c("green4","green3","green","greenyellow","yellow","orange","orangered","red","white","black"),
     breaks=c(-1,-0.1,0.4,0.5,0.6,0.7,0.8,0.9,1,1.4,1.6),axes=FALSE)

Baseline.n<-urb_apply(new.id,Baseline,rug.ssp.t7,Baseline.w)
notes.n<-check_newurb(Baseline.n,Baseline)
notes.n
Baseline.n<-correct_newurb(Baseline.n)

names(Baseline.n)<-c("SSP1 2090","SSP2 2090","SSP3 2090","SSP4 2090","SSP5 2090")
plot(Baseline.n,col=c("red","green","black"),axes=T,legend=FALSE)

# Rename so that we can continue with next time step
result.2090<-Baseline.n
notes.2090<-notes.n
# clean up
remove (Baseline,Baseline.w,Baseline.n,notes.n)



#### Step 5h: Eighth time step (2090 - 2100)

Baseline<-result.2090

Baseline.w<-weighting_function(Baseline)
plot(Baseline.w,col=c("green4","green3","green","greenyellow","yellow","orange","orangered","red","white","black"),
     breaks=c(-1,-0.1,0.4,0.5,0.6,0.7,0.8,0.9,1,1.4,1.6),axes=FALSE)

Baseline.n<-urb_apply(new.id,Baseline,rug.ssp.t8,Baseline.w)
notes.n<-check_newurb(Baseline.n,Baseline)
notes.n
Baseline.n<-correct_newurb(Baseline.n)

names(Baseline.n)<-c("SSP1 2100","SSP2 2100","SSP3 2100","SSP4 2100","SSP5 2100")
plot(Baseline.n,col=c("red","green","black"),axes=T,legend=FALSE)

# Rename so that we can continue with next time step
result.2100<-Baseline.n
notes.2100<-notes.n
# clean up
remove (Baseline,Baseline.w,Baseline.n,notes.n)


################################################################################

