##### UK SSP Urbanisation ######################################################

# This Script is made to model UK artificial surface in five SSP scenarios. 
# It uses dIAP RUG outputs and downscales them to 1km resolution
# Important note: The terms "artificial surface" and "urban surface" are used 
# interchangeably in this script. Read as "surface that is populated by people"

# Contents:
# 1. Create Baseline Surfaces from LCM, Protected Areas and Flood Risk Areas
# 2. Create Artificial Surface Change Surfaces from dIAP RUG
# 3. Optional: Plot Artificial Surface Change by Local Authority District Maps
# 4. Define Functions for Urban Downscaling and Checks
# 5. Apply Functions for each time step
# 6. Reformat and Plot for SSP-wise Visualisations

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
# check if sea area is consistent
length(lcm[is.na(lcm)]) == length(pa[is.na(pa)])


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
# check if sea area is consistent
length(lcm[is.na(lcm)]) == length(aonb[is.na(aonb)])


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
# check if sea area is consistent
length(lcm[is.na(lcm)]) == length(np[is.na(np)])


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
# check if sea area is consistent
length(lcm[is.na(lcm)]) == length(nnr[is.na(nnr)])


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
# check if sea area is consistent
length(lcm[is.na(lcm)]) == length(ramsar[is.na(ramsar)])


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
# check if sea area is consistent
length(lcm[is.na(lcm)]) == length(sac[is.na(sac)])


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
# check if sea area is consistent
length(lcm[is.na(lcm)]) == length(spa[is.na(spa)])


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
# the fld map does not include some islands that are in the lcm map, so I first delete the whole sea area from fld_unmasked
fld_unmasked[is.na(fld_unmasked)]<-0
# then mask with the sea area from the lcm map
fld<-mask(fld_unmasked,lcm,inverse=FALSE,maskvalue=NA,updatevalue=NA)
remove(fld_unmasked)
# check if sea area is consistent
length(lcm[is.na(lcm)]) == length(fld[is.na(fld)])


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
m1<-overlay(lcm,totalprot,fld,fun=sum) # check if this introduces more NAs!!!
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
remove(layernames,base.stack,totalprot,m1,m2,m3)


################################################################################
# 2. Create Change Surfaces from dIAP RUG ######################################

#### Step 2a: Import data function

# this function imports artificial surface projections from a dIAP run that is
# saved online and accessible via a unique URL
import_rug_data<-function(rug.URL){ # URL of rug data
  docs<-c(                          # file names
    "rug_results_2010-2020.csv",
    "rug_results_2020-2030.csv",
    "rug_results_2030-2040.csv",
    "rug_results_2040-2050.csv",
    "rug_results_2050-2060.csv",
    "rug_results_2060-2070.csv",
    "rug_results_2070-2080.csv",
    "rug_results_2080-2090.csv",
    "rug_results_2090-2100.csv")
  rug.files <- paste(c(rep(rug.URL,length(docs))),docs,sep="") # paste to get full URL
  data   <- lapply(rug.files, read.csv, sep=",",skip=0) # read.csv and create list of dataframes
  colnms <- lapply(data,colnames)      # get correct header
  rug.ssp<- lapply(rug.files,read.csv,sep=",",skip=1,header=F) # read.csv without header
  for (i in 1:length(rug.ssp)){        # delete the empty 18th column (mitigates dIAP bug)
    rug.ssp[[i]][,18]=NULL
  }
  for (i in 1:length(rug.ssp)){        # add the correct colnames
    colnames(rug.ssp[[i]])<-colnms[[i]]
  }
  for (i in 1:length(rug.ssp)){        # delete obsolete columns and rename value column
    rug.ssp[[i]]<-rug.ssp[[i]]%>%select(CELL_ID,X....AS.Total)%>%rename(AS = X....AS.Total)
  }
  for(i in 1:length(rug.ssp)){         # standardise data into values between 0 and 1
    rug.ssp[[i]][,2]<-rug.ssp[[i]][,2]/100
  }
  return(rug.ssp)
}

#### Step 2b: Apply import function

URL1<-"http://5.2.157.195:16530/rIAM_alpha/Content/Files/Output/Integrated/magnus.merkle@ed.ac.uk_2020_11_06_20_16_52/"
URL3<-"http://5.2.157.195:16530/rIAM_alpha/Content/Files/Output/Integrated/magnus.merkle@ed.ac.uk_2021_02_08_12_28_03/" #PC R1 and R2 adjusted
URL4<-"http://5.2.157.195:16530/rIAM_alpha/Content/Files/Output/Integrated/magnus.merkle@ed.ac.uk_2020_11_19_14_02_55/"
URL5<-"http://5.2.157.195:16530/rIAM_alpha/Content/Files/Output/Integrated/magnus.merkle@ed.ac.uk_2021_02_08_12_15_46/" #PC R1 and R2 adjusted

rug.ssp1<-import_rug_data(URL1)
rug.ssp3<-import_rug_data(URL3)
rug.ssp4<-import_rug_data(URL4)
rug.ssp5<-import_rug_data(URL5)

remove(URL1,URL3,URL4,URL5,import_rug_data) # clean up

#create SSP2 
# this function creates a 0.9/0.1 interpolation between SSP1 and SSP3 
# the ratio derived from IIASA GDP and population projections
rug.ssp2<-rug.ssp1
for (i in 1:length(rug.ssp2)){
  rug.ssp2[[i]][,2]<-((0.9*rug.ssp1[[i]][,2])+(0.1*rug.ssp3[[i]][,2]))
}

#### Step 2c: Create LAD raster

shapefile <- readOGR(dsn="C:/Users/jmerkle/OneDrive - University of Edinburgh/SSP Quantification/UK Population/LAD Shapefile", layer="Local_Authority_Districts__April_2019__Boundaries_UK_BFE")
shpT<-spTransform(shapefile,crs(Baseline)) # adjust crs and extent to fit our raster data
area_r<-rasterize(shpT,Baseline,"FID")
area_r<-mask(area_r,Baseline[[1]],inverse=FALSE,maskvalue=NA,updatevalue=NA) # make sure that all sea area is excluded

# now we have got a map of LAD IDs
plot(area_r)
length(unique(area_r)) # it has 382 areas. 

# get a dataframe of LAD IDs, codes & names
LAD.table<-as.data.frame(shapefile)


#### Step 2d: Create functions to reproject RUG output to LAD (using the mean) 

# This function takes a dIAP output dataframe, reprojects it from 10' resolution
# to 1km resolution and then creates a mean value of artficial surface
# for each local authority district
LAD_rug_UK<-function(rug,coord,b,lad.map){
  # First Step: Project to a 1km map
  CELL_ID_matched <- match(rug[,1],coord[,1])
  rug[,3]<-coord[,2][CELL_ID_matched] # adds latitude column
  rug[,4]<-coord[,3][CELL_ID_matched] # adds longitude column
  rug<-rug[!is.na(rug[,3]),] # throws out all cells that we have no spatial info for
  proj4.LL <- CRS("+proj=longlat +datum=WGS84")
  rug_spdf <- SpatialPixelsDataFrame(points = SpatialPoints(cbind(rug[,4], rug[,3]), proj4string = proj4.LL), data = data.frame(rug), tolerance = 0.0011)
  rs_LL<-stack(rug_spdf) # raster stack
  rs_UK<-projectRaster(rs_LL, crs=crs(b), method="bilinear", res=1000)# adjust CRS and 1 km resolution
  rs_UK<-crop(rs_UK,b) # crop out UK area
  xmin(rs_UK)<-xmin(b) # align extents
  xmax(rs_UK)<-xmax(b)
  ymin(rs_UK)<-ymin(b)
  ymax(rs_UK)<-ymax(b)
  rs_UK<-mask(rs_UK,Baseline[[1]],inverse=FALSE,maskvalue=NA,updatevalue=NA) # crop out Republic Ireland
  rs_UK<-dropLayer(rs_UK,c(1,3,4))
  # Second Step: Create a vector of average artificial surface per LAD
  as_lad<-vector(length=length(unique(lad.map)))
  for(i in 1:length(as_lad)){
    as_lad[i]<-mean((rs_UK[lad.map==i]),na.rm=TRUE)
  }
  as_lad[50]<-as_lad[49] # Isles of Scilly get the Cornwall value
  return(as_lad)
}

# Apply function for our eight time steps (absolute change per area per time step)
# This function subtracts average artificial surface in t+1
# from average artificial surface in t
# and stores the resulting values vector of local authority districts
make.list.of.as_change<-function(rug,coord,b,lad.map){
  dvect.1<-LAD_rug_UK(rug[[2]],coord,b,lad.map)-LAD_rug_UK(rug[[1]],coord,b,lad.map)
  dvect.2<-LAD_rug_UK(rug[[3]],coord,b,lad.map)-LAD_rug_UK(rug[[2]],coord,b,lad.map)
  dvect.3<-LAD_rug_UK(rug[[4]],coord,b,lad.map)-LAD_rug_UK(rug[[3]],coord,b,lad.map)
  dvect.4<-LAD_rug_UK(rug[[5]],coord,b,lad.map)-LAD_rug_UK(rug[[4]],coord,b,lad.map)
  dvect.5<-LAD_rug_UK(rug[[6]],coord,b,lad.map)-LAD_rug_UK(rug[[5]],coord,b,lad.map)
  dvect.6<-LAD_rug_UK(rug[[7]],coord,b,lad.map)-LAD_rug_UK(rug[[6]],coord,b,lad.map)
  dvect.7<-LAD_rug_UK(rug[[8]],coord,b,lad.map)-LAD_rug_UK(rug[[7]],coord,b,lad.map)
  dvect.8<-LAD_rug_UK(rug[[9]],coord,b,lad.map)-LAD_rug_UK(rug[[8]],coord,b,lad.map)
  dvect<-list(dvect.1,dvect.2,dvect.3,dvect.4,dvect.5,dvect.6,dvect.7,dvect.8)
  return(dvect)
}


#### Step 2e: Apply functions, reformat and check plots

#Cell ID and coordinates
ctry.ids <- read.csv("Cell_ID_LatLong.csv")

# now use the functions
# this function applies the previous two functions and returns a list of vectors
# for each scenario
rug.ssp1<-make.list.of.as_change(rug.ssp1,ctry.ids,Baseline[[1]],area_r)
rug.ssp2<-make.list.of.as_change(rug.ssp2,ctry.ids,Baseline[[2]],area_r)
rug.ssp3<-make.list.of.as_change(rug.ssp3,ctry.ids,Baseline[[3]],area_r)
rug.ssp4<-make.list.of.as_change(rug.ssp4,ctry.ids,Baseline[[4]],area_r)
rug.ssp5<-make.list.of.as_change(rug.ssp5,ctry.ids,Baseline[[5]],area_r)


# format according to time steps
# this function reformats the outputs from the previous function to time step lists
rug.ssp.t1<-list(rug.ssp1[[1]],rug.ssp2[[1]],rug.ssp3[[1]],rug.ssp4[[1]],rug.ssp5[[1]])
rug.ssp.t2<-list(rug.ssp1[[2]],rug.ssp2[[2]],rug.ssp3[[2]],rug.ssp4[[2]],rug.ssp5[[2]])
rug.ssp.t3<-list(rug.ssp1[[3]],rug.ssp2[[3]],rug.ssp3[[3]],rug.ssp4[[3]],rug.ssp5[[3]])
rug.ssp.t4<-list(rug.ssp1[[4]],rug.ssp2[[4]],rug.ssp3[[4]],rug.ssp4[[4]],rug.ssp5[[4]])
rug.ssp.t5<-list(rug.ssp1[[5]],rug.ssp2[[5]],rug.ssp3[[5]],rug.ssp4[[5]],rug.ssp5[[5]])
rug.ssp.t6<-list(rug.ssp1[[6]],rug.ssp2[[6]],rug.ssp3[[6]],rug.ssp4[[6]],rug.ssp5[[6]])
rug.ssp.t7<-list(rug.ssp1[[7]],rug.ssp2[[7]],rug.ssp3[[7]],rug.ssp4[[7]],rug.ssp5[[7]])
rug.ssp.t8<-list(rug.ssp1[[8]],rug.ssp2[[8]],rug.ssp3[[8]],rug.ssp4[[8]],rug.ssp5[[8]])


# clean up
remove(ctry.ids,make.list.of.as_change,LAD_rug_UK)
remove(rug.ssp1,rug.ssp2,rug.ssp3,rug.ssp4,rug.ssp5)


################################################################################
# 3. Optional: Plot Maps of AS change per LAD ##################################

# Careful this takes a lot of time to run

# create function
# this function plots maps of artificial surface change from the vectors
# constructed in the previous time steps
plot_as_change_lad<-function(r,b.id){
  # create maps
  rast<-b.id
  rast[]<-0
  ms<-list()
  for(i in 1:5){ # for all five SSPs
    ms[[i]]<-rast
    for(j in 1:length(rast)){
      if(is.na(b.id[j])==F){ # for all pixels that are not the sea
        ms[[i]][j]<-r[[i]][b.id[j]] # take the change of AS surface change value that corresponds to the LAD in which the pixel is located
      }
    }
  }
  # plot maps
  ms<-stack(ms)
  plot(ms,
       zlim=c(0,max(max(unique(ms[[1]])),max(unique(ms[[2]])),
                    max(unique(ms[[3]])),max(unique(ms[[4]])),
                    max(unique(ms[[5]])))))
}


# plot maps
plot_as_change_lad(rug.ssp.t1,area_r)
plot_as_change_lad(rug.ssp.t2,area_r)
plot_as_change_lad(rug.ssp.t3,area_r)
plot_as_change_lad(rug.ssp.t4,area_r)
plot_as_change_lad(rug.ssp.t5,area_r)
plot_as_change_lad(rug.ssp.t6,area_r)
plot_as_change_lad(rug.ssp.t7,area_r)
plot_as_change_lad(rug.ssp.t8,area_r)


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
sspmod<-c(0.8,0.75,0.7,0.75,0.65)# a vector of modes per SSP (free to choose)
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
  for(i in 1:length(r)){ # for the whole UK area (all 382 districts)
    num[i]<-round((length(b[b.id==i])*(r[i])),digits=0) # in each patch calculate how many cells will be turned to u
    set.seed(2020)
    rnk[[i]]<-rank((-b.w[b.id==i]) , ties.method="random" , na.last=T) # in each patch rank cells in descending order
    new.urb[b.id==i][which(rnk[[i]] %in% 1:num[i])]<-10 # in each patch take the highest likelihood cells and make them urban
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



#### Step 4e: Function to check allocations of new urban areas
check_newurb<-function(b.n,b,ladt){
  check<-vector(length=nlayers(b))
  u.in.n<-c(rep(0,nlayers(b)))
  n.id<-vector(mode = "list", length = nlayers(b))
  u.in.u<-c(rep(0,nlayers(b)))
  u.id<-vector(mode = "list", length = nlayers(b))
  total<-c(rep(0,nlayers(b)))
  accepted<-c(rep(0,nlayers(b)))
  share.old<-c(rep(0,nlayers(b)))
  share.new<-c(rep(0,nlayers(b)))
  check<-vector(length=nlayers(b))
  for(i in 1:nlayers(b)){
    u.in.n[i]<-length(b.n[[i]][b.n[[i]]==9])
    n.id[[i]]<-ladt[,3][ladt[,1] %in% unique(as.vector(area_r[b.n[[i]]==9]))] # this selects the LAD names of of the patch in which new u pixels have been created on excluded land
    u.in.u[i]<-length(b.n[[i]][b.n[[i]]==11])
    u.id[[i]]<-ladt[,3][ladt[,1] %in% unique(as.vector(area_r[b.n[[i]]==11]))] # this selects the LAD names of of the patch in which new u pixels have been created on already urban land
    total[i]<-length(b.n[[i]][b.n[[i]]==11 | b.n[[i]]==10 | b.n[[i]]==9])
    accepted[i]<-length(b.n[[i]][b.n[[i]]==10 | b.n[[i]]==9])
    share.old[i]<-round(100*(length(b[[i]][b[[i]]==1])/
                               (length(b[[i]][!is.na(b[[i]])]))),
                                 digits=2) # the urban surface as a share of the total land surface of the UK
    share.new[i]<-round(100*(length(b.n[[i]][b.n[[i]]==1 | b.n[[i]]==11 | b.n[[i]]==10 | b.n[[i]]==9])/
                               (length(b.n[[i]][!is.na(b.n[[i]])]))),
                                 digits=2) # the accepted urban surface as a share of the total land surface of the UK
    check[i]<-paste(total[i]," artificial surface pixels suggested, out of which ", accepted[i], " pixels are accepted. ",
                    "The total share of artificial surface in the UK increases from ",share.old[i],"% to ", share.new[i],"%. ",
                    u.in.n[i]," new artificial surface pixels are suggested on excluded space and accepted as occasional rogue urbanisation (LAD ",n.id[i],"). ",
                    u.in.u[i]," new artificial surface pixels are suggested on space that is already artificial surface and therefore rejected (LAD ",u.id[i],"). ",
                    sep="")
  }
  return(check)
}


#### Step 4f: Function to assign the standard categories again and correct incorrect allocations
correct_newurb<-function(b.n){
  for(i in 1:nlayers(b.n)){
    b.n[[i]][b.n[[i]]==10]<-1 # this is the standard case: urbanisation on available land
    b.n[[i]][b.n[[i]]==11]<-1 # this is the "double urbanisation" case, which we treat as no change
    b.n[[i]][b.n[[i]]==9]<-1 # means we accept excluded pixels to become urban, if there is no space elsewhere
  }
  return(b.n)
}


#### Step 4g: Function to crop London area (just to visualise more closely)
crop_London<-function(map.stack){
  L<-list()
  for (i in 1:nlayers(map.stack)){
    L[[i]]<-crop(map.stack[[i]],c(400000,650000,95000,300000),snap='near')
  }
  L<-stack(L)
  return(L)
}


################################################################################
# 5. Apply Downscaling Functions ###############################################


#### Step 5a: First time step (2020 - 2030)

Baseline.w<-weighting_function(Baseline)
plot(Baseline.w,col=c("green4","green3","green","greenyellow","yellow","orange","orangered","red","white","black"),
     breaks=c(-1,-0.1,0.4,0.5,0.6,0.7,0.8,0.9,1,1.4,1.6),axes=FALSE)

Baseline.n<-urb_apply(area_r,Baseline,rug.ssp.t1,Baseline.w)
notes.n<-check_newurb(Baseline.n,Baseline,LAD.table)
notes.n
Baseline.n<-correct_newurb(Baseline.n)

names(Baseline.n)<-c("SSP1 2030","SSP2 2030","SSP3 2030","SSP4 2030","SSP5 2030")
plot(Baseline.n,col=c("red","green","black"),axes=T,legend=FALSE)

London.o<-crop_London(Baseline)
plot(London.o,col=c("red","green","black"),axes=T,legend=FALSE)

London<-crop_London(Baseline.n)
plot(London,col=c("red","green","black"),axes=T,legend=FALSE)

# Rename so that we can continue with next time step
Original.Baseline<-Baseline
result.2030<-Baseline.n
notes.2030<-notes.n
London.2020<-London.o
London.2030<-London
# clean up
remove (Baseline,Baseline.w,Baseline.n,notes.n,London.o,London)



#### Step 5b: Second time step (2030 - 2040)

Baseline<-result.2030

Baseline.w<-weighting_function(Baseline)
plot(Baseline.w,col=c("green4","green3","green","greenyellow","yellow","orange","orangered","red","white","black"),
     breaks=c(-1,-0.1,0.4,0.5,0.6,0.7,0.8,0.9,1,1.4,1.6),axes=FALSE)

Baseline.n<-urb_apply(area_r,Baseline,rug.ssp.t2,Baseline.w)
notes.n<-check_newurb(Baseline.n,Baseline,LAD.table)
notes.n
Baseline.n<-correct_newurb(Baseline.n)

names(Baseline.n)<-c("SSP1 2040","SSP2 2040","SSP3 2040","SSP4 2040","SSP5 2040")
plot(Baseline.n,col=c("red","green","black"),axes=T,legend=FALSE)

London<-crop_London(Baseline.n)
plot(London,col=c("red","green","black"),axes=T,legend=FALSE)

# Rename so that we can continue with next time step
result.2040<-Baseline.n
notes.2040<-notes.n
London.2040<-London
# clean up
remove (Baseline,Baseline.w,Baseline.n,notes.n,London)



#### Step 5c: Third time step (2040 - 2050)

Baseline<-result.2040

Baseline.w<-weighting_function(Baseline)
plot(Baseline.w,col=c("green4","green3","green","greenyellow","yellow","orange","orangered","red","white","black"),
     breaks=c(-1,-0.1,0.4,0.5,0.6,0.7,0.8,0.9,1,1.4,1.6),axes=FALSE)

Baseline.n<-urb_apply(area_r,Baseline,rug.ssp.t3,Baseline.w)
notes.n<-check_newurb(Baseline.n,Baseline,LAD.table)
notes.n
Baseline.n<-correct_newurb(Baseline.n)

names(Baseline.n)<-c("SSP1 2050","SSP2 2050","SSP3 2050","SSP4 2050","SSP5 2050")
plot(Baseline.n,col=c("red","green","black"),axes=T,legend=FALSE)

London<-crop_London(Baseline.n)
plot(London,col=c("red","green","black"),axes=T,legend=FALSE)

# Rename so that we can continue with next time step
result.2050<-Baseline.n
notes.2050<-notes.n
London.2050<-London
# clean up
remove (Baseline,Baseline.w,Baseline.n,notes.n,London)



#### Step 5d: Fourth time step (2050 - 2060)

Baseline<-result.2050

Baseline.w<-weighting_function(Baseline)
plot(Baseline.w,col=c("green4","green3","green","greenyellow","yellow","orange","orangered","red","white","black"),
     breaks=c(-1,-0.1,0.4,0.5,0.6,0.7,0.8,0.9,1,1.4,1.6),axes=FALSE)

Baseline.n<-urb_apply(area_r,Baseline,rug.ssp.t4,Baseline.w)
notes.n<-check_newurb(Baseline.n,Baseline,LAD.table)
notes.n
Baseline.n<-correct_newurb(Baseline.n)

names(Baseline.n)<-c("SSP1 2060","SSP2 2060","SSP3 2060","SSP4 2060","SSP5 2060")
plot(Baseline.n,col=c("red","green","black"),axes=T,legend=FALSE)

London<-crop_London(Baseline.n)
plot(London,col=c("red","green","black"),axes=T,legend=FALSE)

# Rename so that we can continue with next time step
result.2060<-Baseline.n
notes.2060<-notes.n
London.2060<-London
# clean up
remove (Baseline,Baseline.w,Baseline.n,notes.n,London)



#### Step 5e: Fifth time step (2060 - 2070)

Baseline<-result.2060

Baseline.w<-weighting_function(Baseline)
plot(Baseline.w,col=c("green4","green3","green","greenyellow","yellow","orange","orangered","red","white","black"),
     breaks=c(-1,-0.1,0.4,0.5,0.6,0.7,0.8,0.9,1,1.4,1.6),axes=FALSE)

Baseline.n<-urb_apply(area_r,Baseline,rug.ssp.t5,Baseline.w)
notes.n<-check_newurb(Baseline.n,Baseline,LAD.table)
notes.n
Baseline.n<-correct_newurb(Baseline.n)

names(Baseline.n)<-c("SSP1 2070","SSP2 2070","SSP3 2070","SSP4 2070","SSP5 2070")
plot(Baseline.n,col=c("red","green","black"),axes=T,legend=FALSE)

London<-crop_London(Baseline.n)
plot(London,col=c("red","green","black"),axes=T,legend=FALSE)

# Rename so that we can continue with next time step
result.2070<-Baseline.n
notes.2070<-notes.n
London.2070<-London
# clean up
remove (Baseline,Baseline.w,Baseline.n,notes.n,London)



#### Step 5f: Sixth time step (2070 - 2080)

Baseline<-result.2070

Baseline.w<-weighting_function(Baseline)
plot(Baseline.w,col=c("green4","green3","green","greenyellow","yellow","orange","orangered","red","white","black"),
     breaks=c(-1,-0.1,0.4,0.5,0.6,0.7,0.8,0.9,1,1.4,1.6),axes=FALSE)

Baseline.n<-urb_apply(area_r,Baseline,rug.ssp.t6,Baseline.w)
notes.n<-check_newurb(Baseline.n,Baseline,LAD.table)
notes.n
Baseline.n<-correct_newurb(Baseline.n)

names(Baseline.n)<-c("SSP1 2080","SSP2 2080","SSP3 2080","SSP4 2080","SSP5 2080")
plot(Baseline.n,col=c("red","green","black"),axes=T,legend=FALSE)

London<-crop_London(Baseline.n)
plot(London,col=c("red","green","black"),axes=T,legend=FALSE)

# Rename so that we can continue with next time step
result.2080<-Baseline.n
notes.2080<-notes.n
London.2080<-London
# clean up
remove (Baseline,Baseline.w,Baseline.n,notes.n,London)



#### Step 5g: Seventh time step (2080 - 2090)

Baseline<-result.2080

Baseline.w<-weighting_function(Baseline)
plot(Baseline.w,col=c("green4","green3","green","greenyellow","yellow","orange","orangered","red","white","black"),
     breaks=c(-1,-0.1,0.4,0.5,0.6,0.7,0.8,0.9,1,1.4,1.6),axes=FALSE)

Baseline.n<-urb_apply(area_r,Baseline,rug.ssp.t7,Baseline.w)
notes.n<-check_newurb(Baseline.n,Baseline,LAD.table)
notes.n
Baseline.n<-correct_newurb(Baseline.n)

names(Baseline.n)<-c("SSP1 2090","SSP2 2090","SSP3 2090","SSP4 2090","SSP5 2090")
plot(Baseline.n,col=c("red","green","black"),axes=T,legend=FALSE)

London<-crop_London(Baseline.n)
plot(London,col=c("red","green","black"),axes=T,legend=FALSE)

# Rename so that we can continue with next time step
result.2090<-Baseline.n
notes.2090<-notes.n
London.2090<-London
# clean up
remove (Baseline,Baseline.w,Baseline.n,notes.n,London)



#### Step 5h: Eighth time step (2090 - 2100)

Baseline<-result.2090

Baseline.w<-weighting_function(Baseline)
plot(Baseline.w,col=c("green4","green3","green","greenyellow","yellow","orange","orangered","red","white","black"),
     breaks=c(-1,-0.1,0.4,0.5,0.6,0.7,0.8,0.9,1,1.4,1.6),axes=FALSE)

Baseline.n<-urb_apply(area_r,Baseline,rug.ssp.t8,Baseline.w)
notes.n<-check_newurb(Baseline.n,Baseline,LAD.table)
notes.n
Baseline.n<-correct_newurb(Baseline.n)

names(Baseline.n)<-c("SSP1 2100","SSP2 2100","SSP3 2100","SSP4 2100","SSP5 2100")
plot(Baseline.n,col=c("red","green","black"),axes=T,legend=FALSE)

London<-crop_London(Baseline.n)
plot(London,col=c("red","green","black"),axes=T,legend=FALSE)

# Rename so that we can continue with next time step
result.2100<-Baseline.n
notes.2100<-notes.n
London.2100<-London
# clean up
remove (Baseline,Baseline.w,Baseline.n,notes.n,London)


################################################################################
# 6. Reformat and Plot #########################################################



#### Step 6a: Reformat

# SSP1
result.SSP1<-stack(Original.Baseline[[1]],result.2040[[1]],
                   result.2070[[1]],result.2100[[1]])
London.SSP1<-stack(London.2020[[1]],London.2040[[1]],
                   London.2070[[1]],London.2100[[1]])
notes.SSP1<-c(notes.2030[1],notes.2040[1],notes.2050[1],notes.2060[1],
              notes.2070[1],notes.2080[1],notes.2090[1],notes.2100[1])

# SSP2
result.SSP2<-stack(Original.Baseline[[2]],result.2040[[2]],
                   result.2070[[2]],result.2100[[2]])
London.SSP2<-stack(London.2020[[2]],London.2040[[2]],
                   London.2070[[2]],London.2100[[2]])
notes.SSP2<-c(notes.2030[2],notes.2040[2],notes.2050[2],notes.2060[2],
              notes.2070[2],notes.2080[2],notes.2090[2],notes.2100[2])

# SSP3
result.SSP3<-stack(Original.Baseline[[3]],result.2040[[3]],
                   result.2070[[3]],result.2100[[3]])
London.SSP3<-stack(London.2020[[3]],London.2040[[3]],
                   London.2070[[3]],London.2100[[3]])
notes.SSP3<-c(notes.2030[3],notes.2040[3],notes.2050[3],notes.2060[3],
              notes.2070[3],notes.2080[3],notes.2090[3],notes.2100[3])

# SSP4
result.SSP4<-stack(Original.Baseline[[4]],result.2040[[4]],
                   result.2070[[4]],result.2100[[4]])
London.SSP4<-stack(London.2020[[4]],London.2040[[4]],
                   London.2070[[4]],London.2100[[4]])
notes.SSP4<-c(notes.2030[4],notes.2040[4],notes.2050[4],notes.2060[4],
              notes.2070[4],notes.2080[4],notes.2090[4],notes.2100[4])

# SSP5
result.SSP5<-stack(Original.Baseline[[5]],result.2040[[5]],
                   result.2070[[5]],result.2100[[5]])
London.SSP5<-stack(London.2020[[5]],London.2040[[5]],
                   London.2070[[5]],London.2100[[5]])
notes.SSP5<-c(notes.2030[5],notes.2040[5],notes.2050[5],notes.2060[5],
              notes.2070[5],notes.2080[5],notes.2090[5],notes.2100[5])


#### Step 6b: Plot

plot(result.SSP1,col=c("red","green","black"),axes=F,legend=FALSE)
plot(London.SSP1,col=c("red","green","black"),axes=F,legend=FALSE)

plot(result.SSP2,col=c("red","green","black"),axes=F,legend=FALSE)
plot(London.SSP2,col=c("red","green","black"),axes=F,legend=FALSE)

plot(result.SSP3,col=c("red","green","black"),axes=F,legend=FALSE)
plot(London.SSP3,col=c("red","green","black"),axes=F,legend=FALSE)

plot(result.SSP4,col=c("red","green","black"),axes=F,legend=FALSE)
plot(London.SSP4,col=c("red","green","black"),axes=F,legend=FALSE)

plot(result.SSP5,col=c("red","green","black"),axes=F,legend=FALSE)
plot(London.SSP5,col=c("red","green","black"),axes=F,legend=FALSE)

################################################################################

