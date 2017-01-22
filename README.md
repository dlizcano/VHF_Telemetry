# VHF_Telemetry
Script for VHF load and analyzing radio tracking data. From azimuths to home range plot 

## load packages


```r

library (sp)  # classes for spatial data
library (raster)  # grids, rasters
library (rasterVis)  # raster visualisation
library (maptools)
library (rgeos)
library (rgdal)
library (ggmap)
library (lubridate)
library (sigloc) #load locations and azimuth

```
## Load the stations and angles data set

```r
birds <- read.csv("data/telemetria_Salento.csv", header = T, sep = ";")

coordinates(birds) <- cbind(birds$Easting, birds$Northing)



crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(birds) <- crs.geo  # define projection system of our data
summary(birds)


topo <- getData('SRTM', lon=-74.4, lat=4.6)
topo2 <-  crop(topo, c(-74.46378, -74.43911, 4.631078,  4.652000 ))

extent(birds)
extent(topo)

plot(topo2)
plot(birds, add=T)

utm18n <- CRS("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs") # define UTM
birds_utm <- spTransform(birds, utm18n) # project to UTM
topo_utm <- projectRaster(topo2, crs=utm18n)


# fix time
birds_utm$Time <- as.numeric(as.POSIXct(paste(as.Date(birds_utm$Date, "%m/%d/%y"), birds_utm$Time)))

## Convert data to class 'receiver
momotus1 <- as.data.frame(birds_utm)
sp <- "Momotus aequatorialis"
momotus <- subset(momotus1, species == "Momotus aequatorialis")
momotus <- momotus[-4,]
ind <- which(is.na(momotus$Azimuth)) # mark NA
momotus <- momotus[-ind,] # delete NAs

conteos_GID1 <- table(momotus$GID)
conteos_GID2 <- which( conteos_GID1 < 3 )
ind_GID <- as.numeric(names(conteos_GID2))

# loop to select DIG less than two
ind_del <- data.frame(2,2)
for (i in 1: length(ind_GID)){
  ind_del[i,] <- which(momotus$GID == ind_GID[i])
  }
ind_del_end <- unique(unlist(ind_del)) # 
momotus <- momotus[-c(ind_del_end),] # delete index


momotus_reciver<-as.receiver(as.data.frame(momotus))

# compute the location of bearing intersections using basic algebra
(cross<-findintersects(momotus_reciver)) 
totalpt <- max(cross$GID)

#  compute the location of a transmitter signal using the maximum likelihood estimation (MLE)
(loc<-locate(momotus_reciver))


## Display resulting object with bearings
plot(topo2, main=paste(sp,totalpt, sep=" "))
# plot(birds, add=T, )
plot(momotus_reciver, col= "purple", bearings=TRUE,xlab="Easting",ylab="Northing", add=T)
plot(cross, add=T)
plot(loc, errors=TRUE, col="orange", add=T)


```
