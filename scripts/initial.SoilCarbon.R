rm(list = ls())

library(raster)
library(dplyr)

file <- "~/Downloads/ocs_0-30cm_mean.tif"
OCS = raster(file)
OCS.aggregate <- aggregate(OCS, fact=10)

GRID_RES <- 0.1
clat = 9.075; clon = -79.85;

e <- extent(clon-GRID_RES/2,clon+GRID_RES/2,clat-GRID_RES/2,clat+GRID_RES/2)
cOCS <- raster::extract(OCS.aggregate, e)
mOCS <- mean(cOCS,na.rm=TRUE)/10
