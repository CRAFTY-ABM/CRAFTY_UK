library(raster)
library(dplyr)



# Make a neighbourhood function (derrived from Kumaraswamy Distr.)
# we assume endogenous alpha and mode.
# The mode is the peak of the function, i.e. the highest likelihood of urbanisation.
alpha<-2
mod<-0.7 # this is for SSP1

# I have replaced beta with a term that yields beta in dependence of alpha and the mode. 
kumar<-function(x){
  y<-(alpha*(((mod^-alpha)*((mod^alpha)+alpha-1))/alpha)*
        (x^(alpha-1))*((1-(x^alpha))^
                         ((((mod^-alpha)*((mod^alpha)+alpha-1))/alpha)-1)))
  return(y)
}



# Here follows an illustrative example
# First create an example baseline raster
rast<-raster(matrix(runif(225),15,15))
crs(rast)<-"+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000
+a=6377563.396 +rf=299.324975315035 +units=m +no_defs"
plot(rast)
rast[c(round(runif(30,min=1,max=225),digits=0))]<-1 # these are urban
rast[c(round(runif(30,min=1,max=225),digits=0))]<--1 # these are excluded (non-potential)
plot(rast)
hist(rast)
# Then create an example RUG output (let's assume that urban increases by 20%)
RUG.rast<-rast
values(RUG.rast)<-0.2
plot(RUG.rast)




# Step 1: Create a raster with only urban cells
rast.u<-rast
rast.u[rast.u<1]<-NA
plot(rast.u)

# Step 2: Transform it to distance raster (distance to urban cells) and re-exclude cells that cannot be populated
rast.d<-distance(rast.u,doEdge=FALSE)
rast.d[rast==-1]<-NA # exclusion of cells that cannot be populated
plot(rast.d)
values(rast.d)

# Step 3: Standardise the distances to a defined neighbourhood boundary
nh.bound<-1 # define neighbourhood boundary (whatever unit we use, I take 1 (10km) as an example)
rast.d[rast.d>nh.bound]<-NA # any cell that is further than the neighbourhood boundary should not be included
plot(rast.d)
rast.d<-1-(rast.d/nh.bound) # to standardise it between 0 and 1. Now urban cells are 1 again and the value of each cell decreases the further away it is from an urban cell.
plot(rast.d)

# Step 4: Apply neighbourhood function
rast.w<-kumar(rast.d) # apply neighbourhood function, which puts a weight to non-urban cells
plot(rast.w)
values(rast.w)
length(unique(values(rast.w))) # so this is annoying. It means that there are many cells that have the same likeliness to become urban.


# Step 5: Calculate cells that will become urban
num<-round((length(rast[rast==1])*(values(RUG.rast)[1])),digits=0) # calculate how many cells will be turned to u
thd<-utils::tail(sort(getValues(rast.w)),num)[1] # get the first number of this vector
length(rast.w[rast.w==thd])# this shows a potential problem. We have several cells that have precisely this threshold value...


# we can break tie by randomly 
# e.g. 
table(rank(values(rast.w))) # tie braking by "average" the ordinary way
table(rank(values(rast.w), ties.method = "average")) # by default (just to confirm)

table(rank(values(rast.w), ties.method = "random"))  # tie braking randomly (using small random noise)

set.seed(2020) # RNG seeding 
v1 = values(rast.w)
rnk1 = rank(v1, ties.method = "random", na.last = F) # must take care the NA values. Otherwise NA comes in arbitrary positions
plot( rnk1, v1) # rank is in ascending order 
rnk2 = abs(length(v1) - rnk1) + 1  # reverse it
plot( rnk2, v1) # rank is reversed


rast.w2 = rast.w
rast.w2[] = 0 
rast.w2[which(rnk2 %in% 1:num)]<-1

values(rast.w2)
plot(rast.w2)



# Step 6: new surface
rast.n<-rast.w2+rast
rast.n[rast.n>=1]<-1 # these are urban cells. New and old.
rast.n[is.na(rast.n)]<--1 # these are excluded cells (non-potential)
freq(rast.n)
plot(rast.n)
freq(rast)
plot(rast)

# check
num==length(rast.n[rast.n==1])-length(rast[rast==1])
length(rast.n[rast.n==1])-length(rast[rast==1]) # it sometimes adds more cells than wanted.

