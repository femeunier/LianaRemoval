rm(list = ls())

dirpath <- "/home/femeunier/Documents/data/met/BCI/"

library("hdf5r")

month <- c('JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC')
year <- seq(2003,2016)

# Reading Prate
A <- sapply(X = seq(month),function(i){

  sapply(X = seq_along(year), FUN = function(j) {

    fname <- paste0("BCI_",year[j],month[i],".h5")
    met_driver <- file.path(dirpath,fname)

    metfile    <- h5file(met_driver, mode = "r")

    prate <- metfile[["prate"]] [,,]

    metfile$close_all()

    prate_loc <- prate*60*60 #time step = 30 min #kg/m2/s --> mm
    prate_loc[prate_loc < 0] <- 0

    return(sum(prate_loc))

  }) # year sapply

},simplify=FALSE) # lat sapply

PRate <- array(unlist(A),c(length(year),length(month)))
rownames(PRate) <- year
colnames(PRate) <- month

matplot(t(PRate),type='l')
lines(PRate[8,],lty=1,col='red',cex=2)

############################################################################################################
# Other variables

var <- c("tmp","pres","sh","dlwrf","nbdsf","nddsf","vbdsf","vddsf")
# Temperature, pressure, specific humidity, downward longwave radiation, Near-IR beam radiation, Near-IR diffuse radiation, Visible beam radiation, Visible diffuse radiation

All <- lapply(X = seq(var), function(ivar){   # Not the most efficient, should be applied when read
  #ivar
  print(paste('... Reading',var[ivar],'...',sep=' '))
  B <- sapply(X = seq(month),function(i){

    sapply(X = seq_along(year), FUN = function(j) {

      fname <- paste0("BCI_",year[j],month[i],".h5")
      met_driver <- file.path(dirpath,fname)

      metfile    <- h5file(met_driver, mode = "r")

      temp <- metfile[[var[ivar]]] [,,]

      metfile$close_all()

      temp_loc <- temp
      return(mean(temp_loc))

    })

  },simplify=FALSE)

  return(array(unlist(B),c(length(year),length(month))))

}) # ivar lapply
names(All) <- var

matplot(t(All$tmp)-273.15,type='l')
